;; CST reader for CapyScheme.
;;
;; A concrete-syntax-tree front end for tooling (LSP, formatters, ...). Unlike
;; the boot reader it:
;;
;;   * never raises on malformed input - it produces a best-effort tree plus
;;     `cst-error` records (tolerant parsing);
;;   * preserves tokens: parentheses vs brackets, dots, abbreviations,
;;     whitespace and comments (line, `#;` datum, `#| |#` block);
;;   * records exact spans: 1-based line / 0-based column (the same convention
;;     as `syntax-sourcev`) plus byte offsets;
;;   * produces, for each top-level form, a `syntax` object built with
;;     `datum->syntax` carrying a sourcev, ready for expansion.
;;
;; The lexer is reused from the boot reader (`get-token` with atmosphere
;; enabled, tolerant mode), so tokenization matches the real reader exactly.

(library (capy cst-reader)
  (export
    ;; parsing
    cst-read-document
    cst-read-document-incremental
    cst-parse-tail
    ;; documents
    cst-document?
    cst-document-text
    cst-document-filename
    cst-document-forms
    cst-document-errors
    cst-document-trailing
    ;; forms
    cst-form-prelude
    cst-form-node
    cst-form-syntax
    cst-form-errors
    cst-form-start-line cst-form-start-col
    cst-form-end-line cst-form-end-col
    cst-form-start-byte cst-form-end-byte
    ;; nodes
    cst-node?
    cst-node-kind
    cst-node-children
    cst-node-datum
    cst-node-syntax
    cst-node-start-line cst-node-start-col
    cst-node-end-line cst-node-end-col
    cst-node-start-byte cst-node-end-byte
    ;; tokens
    cst-token?
    cst-token-kind
    cst-token-text
    cst-token-datum
    cst-token-extra
    cst-token-start-line cst-token-start-col
    cst-token-end-line cst-token-end-col
    cst-token-start-byte cst-token-end-byte
    ;; errors
    cst-error?
    cst-error-severity
    cst-error-code
    cst-error-message
    cst-error-start-line cst-error-start-col
    cst-error-end-line cst-error-end-col
    cst-error-start-byte cst-error-end-byte
    cst-error-related
    ;; walking / helpers
    cst-walk
    cst-token-at-position)
  (import (rnrs)
    (rnrs bytevectors)
    (rnrs hashtables)
    (srfi 8)
    (capy))

  ;; ------------------------------------------------------------------------
  ;; Records
  ;; ------------------------------------------------------------------------

  (define-record-type (<cst-token> %make-cst-token cst-token?)
    (fields
      (immutable kind cst-token-kind)
      (immutable text cst-token-text)
      (immutable datum cst-token-datum)
      (immutable extra cst-token-extra)
      (immutable sl cst-token-start-line)
      (immutable sc cst-token-start-col)
      (immutable el cst-token-end-line)
      (immutable ec cst-token-end-col)
      (immutable sb cst-token-start-byte)
      (immutable eb cst-token-end-byte)))

  (define-record-type (<cst-node> %make-cst-node cst-node?)
    (fields
      (immutable kind cst-node-kind)
      (immutable children cst-node-children)
      (immutable datum cst-node-datum)
      (immutable syntax cst-node-syntax)
      (immutable sl cst-node-start-line)
      (immutable sc cst-node-start-col)
      (immutable el cst-node-end-line)
      (immutable ec cst-node-end-col)
      (immutable sb cst-node-start-byte)
      (immutable eb cst-node-end-byte)))

  (define-record-type (<cst-error> %make-cst-error cst-error?)
    (fields
      (immutable severity cst-error-severity)
      ;; Symbolic code: read/unclosed, read/mismatched, read/misplaced-dot,
      ;; read/unexpected-close, read/unexpected-dot, read/dot-in-datum,
      ;; read/missing-operand, read/lexical.
      (immutable code cst-error-code)
      (immutable message cst-error-message)
      (immutable sl cst-error-start-line)
      (immutable sc cst-error-start-col)
      (immutable el cst-error-end-line)
      (immutable ec cst-error-end-col)
      (immutable sb cst-error-start-byte)
      (immutable eb cst-error-end-byte)
      ;; Secondary labels: list of (start-line start-col end-line end-col text).
      (immutable related cst-error-related)))

  (define-record-type (<cst-form> %make-cst-form cst-form?)
    (fields
      (immutable prelude cst-form-prelude)
      (immutable node cst-form-node)
      (immutable syntax cst-form-syntax)
      (immutable errors cst-form-errors)
      (immutable sl cst-form-start-line)
      (immutable sc cst-form-start-col)
      (immutable el cst-form-end-line)
      (immutable ec cst-form-end-col)
      (immutable sb cst-form-start-byte)
      (immutable eb cst-form-end-byte)))

  (define-record-type (<cst-document> %make-cst-document cst-document?)
    (fields
      (immutable text cst-document-text)
      (immutable filename cst-document-filename)
      (immutable forms cst-document-forms)
      (immutable errors cst-document-errors)
      (immutable trailing cst-document-trailing)))

  (define-record-type (<line-index> %make-line-index line-index?)
    (fields
      (immutable text line-index-text)
      (immutable entries line-index-entries)
      (immutable caches line-index-caches)))


  (define (utf8-length ch)
    (let ((n (char->integer ch)))
      (cond ((<= n #x7F) 1)
            ((<= n #x7FF) 2)
            ((<= n #xFFFF) 3)
            (else 4))))

  ;; Whether the character I of TEXT ends a line: a line feed, or a carriage return that no line
  ;; feed follows (the same line breaks as (capy text line-index) and the reader of the runtime).
  (define (line-break-at? text i)
    (let ((c (string-ref text i)))
      (or (char=? c #\newline)
          (and (char=? c #\return)
               (or (= (+ i 1) (string-length text))
                   (not (char=? (string-ref text (+ i 1)) #\newline)))))))

  (define (make-line-table text)
    ;; entries[i] = (char-start . byte-start) of line i+1. A final sentinel
    ;; entry marks the end of the text. A line starts at char 0 and right
    ;; after each newline.
    (let* ((len (string-length text))
           (entries
             (let loop ((i 0) (bytes 0) (out '()) (at-line-start? #t))
               (cond
                 ((>= i len)
                  (list->vector (reverse (cons (cons i bytes) out))))
                 (at-line-start?
                  (loop i bytes (cons (cons i bytes) out) #f))
                 ((line-break-at? text i)
                  (loop (+ i 1) (+ bytes (utf8-length (string-ref text i)))
                    out #t))
                 (else
                  (loop (+ i 1) (+ bytes (utf8-length (string-ref text i)))
                    out #f))))))
      (%make-line-index text entries (make-hashtable equal-hash equal?))))

  (define (line-index-line-count idx)
    (- (vector-length (line-index-entries idx)) 1))

  ;; Byte offset of (line, col) - 1-based line, 0-based col. Memoizes a
  ;; cumulative byte table per line. `col` may point one past the end of the
  ;; line (e.g. the EOF position); it is clamped to the line length.
  (define (line-index-byte-at idx line col)
    (define entries (line-index-entries idx))
    (define text (line-index-text idx))
    (define caches (line-index-caches idx))
    (define (line-char-start l) (car (vector-ref entries (- l 1))))
    (define (line-byte-start l) (cdr (vector-ref entries (- l 1))))
    (define (line-end-char l)
      (if (>= l (vector-length entries))
        (car (vector-ref entries (- (vector-length entries) 1)))
        (car (vector-ref entries l))))
    (define (build-cache l)
      (let* ((start (line-char-start l))
             (end (line-end-char l))
             (chars (- end start))
             (cum (make-vector (+ chars 1) (line-byte-start l))))
        (let loop ((i start) (j 1) (bytes (line-byte-start l)))
          (if (< i end)
            (let ((nb (+ bytes (utf8-length (string-ref text i)))))
              (vector-set! cum j nb)
              (loop (+ i 1) (+ j 1) nb))
            (begin
              (hashtable-set! caches l cum)
              cum)))))
    (let* ((cum (hashtable-ref caches line #f))
           (table (or cum (build-cache line)))
           (chars (- (vector-length table) 1)))
      (vector-ref table (min col chars))))

  ;; Char offset of (line, col).
  (define (line-index-char-at idx line col)
    (define entries (line-index-entries idx))
    (define (line-end-char l)
      (if (>= l (vector-length entries))
        (car (vector-ref entries (- (vector-length entries) 1)))
        (car (vector-ref entries l))))
    (let ((start (car (vector-ref entries (- line 1))))
          (chars (- (line-end-char line) (car (vector-ref entries (- line 1))))))
      (+ start (min col chars))))


  (define (make-token-stream text filename base-line base-col base-byte
                             reader-mode)
    ;; Positions inside `text` (a tail substring for incremental re-parses)
    ;; start at line 1/col 0; map them into the original document's
    ;; coordinates.
    (define idx (make-line-table text))
    (define reader (make-reader (open-input-string text) filename))
    (define warnings '())
    ;; where the text ends, as a line and a column of its own: the reader counts the end of the
    ;; input as a character, which puts the end of a token that reaches it one column too far
    (define end-line
      (let loop ((i 0) (n 1))
        (cond ((= i (string-length text)) n)
              ((line-break-at? text i) (loop (+ i 1) (+ n 1)))
              (else (loop (+ i 1) n)))))
    (define end-col
      (let loop ((i (string-length text)) (n 0))
        (if (or (= i 0) (line-break-at? text (- i 1)))
          n
          (loop (- i 1) (+ n 1)))))
    (define (map-pos line* col*)
      (let* ((past? (or (> line* end-line) (and (= line* end-line) (> col* end-col))))
             (line (if past? end-line line*))
             (col (if past? end-col col*))
             (al (+ base-line line -1))
             (ac (if (= line 1) (+ base-col col) col))
             (ab (+ base-byte (line-index-byte-at idx line col))))
        (values al ac ab)))
    ;; The warnings the reader raised while it read the token that is being read, as
    ;; (line column message): they are recorded when the token ends, as ranges up to its end.
    (define pending '())
    (define (record-warning! c)
      (let* ((sv (condition-sourcev c))
             (wl (if (and (vector? sv) (>= (vector-length sv) 3))
                   (vector-ref sv 1)
                   (reader-saved-line reader)))
             (wc (if (and (vector? sv) (>= (vector-length sv) 3))
                   (vector-ref sv 2)
                   (reader-saved-column reader)))
             (msg (if (message-condition? c)
                    (condition-message c)
                    "reader warning")))
        (set! pending (cons (list wl wc msg) pending))
        #f))
    (define (flush-warnings! end-line end-col)
      (for-each
        (lambda (w)
          (receive (al ac ab) (map-pos (car w) (cadr w))
            (receive (el ec eb) (map-pos end-line end-col)
              (let ((past-start? (or (> el al) (and (= el al) (>= ec ac)))))
                (set! warnings
                  (cons (%make-cst-error 'warning 'read/lexical (caddr w) al ac
                          (if past-start? el al) (if past-start? ec ac)
                          ab (if past-start? eb ab) '())
                    warnings))))))
        (reverse pending))
      (set! pending '()))
    ;; A token starts where the one before it ended: the reader marks the start of the token it
    ;; is reading, and a token that reads others (`#;` and its datum) moves that mark.
    ;;
    ;; An error that is not a warning (an unclosed datum after `#;`) cannot continue: it ends the
    ;; token, as an `inline-comment` that spans what was read, and is recorded as an error.
    (define (next-token)
      (let ((start-line (reader-line reader))
            (start-col (reader-column reader)))
        (define (failed-token c)
          (let ((end-line (reader-line reader))
                (end-col (reader-column reader)))
            (flush-warnings! end-line end-col)
            (if (and (= end-line start-line) (= end-col start-col))
              (list 'eof #f)
              (call-with-values
                (lambda () (map-pos start-line start-col))
                (lambda (sl sc sb)
                  (call-with-values
                    (lambda () (map-pos end-line end-col))
                    (lambda (el ec eb)
                      (set! warnings
                        (cons (%make-cst-error 'error 'read/lexical
                                (if (message-condition? c)
                                  (condition-message c)
                                  "reader error")
                                sl sc el ec sb eb '())
                          warnings))
                      (list 'inline-comment #f start-line start-col end-line end-col
                        sl sc sb el ec eb))))))))
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c)
                (if (warning? c)
                  (record-warning! c)
                  (k (failed-token c))))
              (lambda ()
                (call-with-values
                  (lambda () (get-token reader))
                  (lambda (lextype payload)
                    (call-with-values
                      (lambda () (map-pos start-line start-col))
                      (lambda (sl sc sb)
                        (call-with-values
                          (lambda () (map-pos (reader-line reader) (reader-column reader)))
                          (lambda (el ec eb)
                            (flush-warnings! (reader-line reader) (reader-column reader))
                            (list lextype payload
                              start-line start-col
                              (reader-line reader) (reader-column reader)
                              sl sc sb el ec eb)))))))))))))
    (define (token-stream-warnings)
      (reverse warnings))
    (reader-mode-set! reader reader-mode)
    (reader-tolerant-set! reader #t)
    (values next-token token-stream-warnings idx))

  (define (atmosphere-token? t)
    (and (cst-token? t)
         (memq (cst-token-kind t)
           '(whitespace comment block-comment datum-comment directive shebang))))

  (define (token-kind lextype payload)
    (case lextype
      ((whitespace) 'whitespace)
      ((comment) 'comment)
      ((nested-comment) 'block-comment)
      ((inline-comment) 'datum-comment)
      ((directive) 'directive)
      ((shebang) 'shebang)
      ((openp) 'open)
      ((openb) 'open)
      ((vector) 'open)
      ((bytevector) 'open)
      ((closep) 'close)
      ((closeb) 'close)
      ((dot) 'dot)
      ((abbrev) 'abbrev)
      ((identifier) 'identifier)
      ((value)
       (cond
         ((eof-object? payload) 'error)
         ((boolean? payload) 'boolean)
         ((keyword? payload) 'keyword)
         ((string? payload) 'string)
         ((char? payload) 'char)
         (else 'number)))
      ((eof) 'eof)
      (else 'error)))

  (define (bracket-extra lextype)
    (case lextype
      ((openp closep) 'paren)
      ((openb closeb) 'bracket)
      ((vector) 'vector)
      ((bytevector) 'bytevector)
      (else #f)))

  ;; The symbol an abbreviation stands for: the lexer names it (`quote`, `quasiquote`, `unquote`,
  ;; `unquote-splicing`, `syntax`, `quasisyntax`, `unsyntax`, `unsyntax-splicing`).
  (define (abbrev-symbol a) a)

  (define (make-sourcev filename sl sc el ec sb eb)
    (vector filename sl sc el ec sb eb 'read '()))

  ;; Parses `text` starting at base coordinates; returns a document. When
  ;; base-line/base-col/base-byte are 1/0/0 this is a full-document parse;
  ;; otherwise it parses a tail substring.
  (define (cst-parse-tail* text filename base-line base-col base-byte reader-mode recover?)
    (call-with-values
      (lambda ()
        (make-token-stream text filename base-line base-col base-byte reader-mode))
      (lambda (next-token token-stream-warnings idx)
        (define errors '())
        (define (add-error! code message sl sc el ec sb eb . related)
          (set! errors
            (cons (%make-cst-error 'error code message sl sc el ec sb eb
                    (if (pair? related) (car related) '()))
              errors)))
        (define (add-token-error! code message t . related)
          (apply add-error! code message
            (cst-token-start-line t) (cst-token-start-col t)
            (cst-token-end-line t) (cst-token-end-col t)
            (cst-token-start-byte t) (cst-token-end-byte t)
            related))

        ;; One token of pushback, used by indentation recovery.
        (define pending #f)
        (define (unread! t) (set! pending t))

        ;; Read one token; on eof returns #f. Token positions are mapped into
        ;; document coordinates. The raw (unmapped) positions are used to
        ;; slice the token text out of `text`.
        (define (read-raw!)
          (cond
            (pending (let ((t pending)) (set! pending #f) t))
            (queued (let ((t queued)) (set! queued #f) t))
            (else (split-comment (read-lexed!)))))
        ;; The token after a line comment: the reader takes the line break that ends the comment
        ;; as part of it, and the comment would end at the start of the next line. The comment
        ;; ends where its text does, and the line break is a whitespace token of its own.
        (define queued #f)
        (define (split-comment t)
          (let* ((text (and t (eq? (cst-token-kind t) 'comment) (cst-token-text t)))
                 (len (and text (string-length text)))
                 (term (cond
                         ((not text) 0)
                         ((and (> len 1) (char=? (string-ref text (- len 1)) #\newline)
                               (char=? (string-ref text (- len 2)) #\return))
                          2)
                         ((and (> len 0) (memv (string-ref text (- len 1)) '(#\newline #\return))) 1)
                         (else 0))))
            (if (= term 0)
              t
              (let* ((body (substring text 0 (- len term)))
                     (end-col (+ (cst-token-start-col t) (string-length body)))
                     (end-byte (+ (cst-token-start-byte t) (let loop ((i 0) (n 0)) (if (= i (string-length body)) n (loop (+ i 1) (+ n (utf8-length (string-ref body i)))))))))
                (set! queued
                  (%make-cst-token 'whitespace (substring text (- len term) len) #f #f
                    (cst-token-start-line t) end-col
                    (cst-token-end-line t) (cst-token-end-col t)
                    end-byte (cst-token-end-byte t)))
                (%make-cst-token 'comment body #f #f
                  (cst-token-start-line t) (cst-token-start-col t)
                  (cst-token-start-line t) end-col
                  (cst-token-start-byte t) end-byte)))))
        (define (read-lexed!)
          (let ((t (next-token)))
            (if (eq? (car t) 'eof)
              #f
              (let ((lextype (car t))
                    (payload (cadr t))
                    (rsl (caddr t))
                    (rsc (cadddr t))
                    (rel (list-ref t 4))
                    (rec (list-ref t 5))
                    (sl (list-ref t 6))
                    (sc (list-ref t 7))
                    (sb (list-ref t 8))
                    (el (list-ref t 9))
                    (ec (list-ref t 10))
                    (eb (list-ref t 11)))
                (%make-cst-token (token-kind lextype payload)
                  (let* ((rstart (line-index-char-at idx rsl rsc))
                         (rend (line-index-char-at idx rel rec)))
                    (if (<= rstart rend)
                      (substring text rstart (min rend (string-length text)))
                      ""))
                  (if (and (memq lextype '(value identifier)) (not (eof-object? payload))) payload #f)
                  (if (memq lextype '(abbrev openp openb vector bytevector
                                      closep closeb))
                    (if (eq? lextype 'abbrev)
                      (abbrev-symbol payload)
                      (bracket-extra lextype))
                    #f)
                  sl sc el ec sb eb)))))

        ;; Parse one element (token or compound). An abbreviation is returned
        ;; together with the elements forming its operand, so pairing never
        ;; loses the operand: `(abbrev . operand-elements)`.
        (define (parse-element!)
          (let loop ((atmosphere '()))
            (let ((t (read-raw!)))
              (define (with-atmosphere elements)
                (append (reverse atmosphere) elements))
              (cond
                ((not t) (reverse atmosphere))
                ((atmosphere-token? t) (loop (cons t atmosphere)))
                ;; what ends the enclosing list is not an operand
                ((and (cst-token? t) (memq (cst-token-kind t) '(close dot)))
                 (unread! t)
                 (reverse atmosphere))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'open))
                 (with-atmosphere (list (parse-compound! t))))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'abbrev))
                 (with-atmosphere (cons t (parse-element!))))
                (else (with-atmosphere (list t)))))))

        ;; Parse a compound starting from its already-read open token.
        (define (parse-compound! open-token)
          (define kind
            (case (cst-token-extra open-token)
              ((paren bracket) 'list)
              (else (cst-token-extra open-token))))
          (define (closes? extra)
            (case (cst-token-extra open-token)
              ((paren) (eq? extra 'paren))
              ((bracket) (eq? extra 'bracket))
              (else (eq? extra 'paren))))
          (let loop ((children (list open-token)))
            (let ((t (read-raw!)))
              (cond
                ((not t)
                 (add-token-error! 'read/unclosed
                   "unexpected end of file: unclosed list" open-token)
                 (finish-compound kind (reverse children) open-token #f))
                ;; Indentation recovery: a `(` in column 0 on a later line
                ;; cannot belong to this list; close it before that line.
                ((and recover?
                      (cst-token? t)
                      (eq? (cst-token-kind t) 'open)
                      (= (cst-token-start-col t) 0)
                      (> (cst-token-start-line t)
                         (cst-token-start-line open-token)))
                 (add-token-error! 'read/unclosed
                   "unclosed list (closed before the next top-level form)"
                   open-token
                   (list (list (cst-token-start-line t) (cst-token-start-col t)
                           (cst-token-end-line t) (cst-token-end-col t)
                           "a closing delimiter was expected before this line")))
                 (unread! t)
                 (finish-compound kind (reverse children) open-token #f))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'close))
                 (if (closes? (cst-token-extra t))
                   (finish-compound kind (reverse (cons t children)) open-token t)
                   (begin
                     (add-token-error! 'read/mismatched
                       "mismatched parenthesis/bracket" t
                       (list (list (cst-token-start-line open-token)
                               (cst-token-start-col open-token)
                               (cst-token-end-line open-token)
                               (cst-token-end-col open-token)
                               "the delimiter opened here")))
                     (finish-compound kind (reverse (cons t children)) open-token t))))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'dot))
                 (if (eq? kind 'list)
                   (loop (cons t children))
                   (begin
                     (add-token-error! 'read/dot-in-datum
                       "dot used in non-list datum" t)
                     (loop (cons t children)))))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'open))
                 (loop (cons (parse-compound! t) children)))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'abbrev))
                 (loop (append (reverse (cons t (parse-element!))) children)))
                ((and (cst-token? t)
                      (memq (cst-token-kind t) '(value identifier)))
                 (loop (cons t children)))
                (else ;; atmosphere
                 (loop (cons t children)))))))

        ;; ---- Syntax objects ----
        ;;
        ;; Every element gets a syntax object whose source is its own range, as the reader of the
        ;; compiler does (`annotate`), so the expander sees where each identifier was written. An
        ;; abbreviation (`'x`) is a list from the mark to the end of its operand. Nothing here
        ;; reports an error: the datum assembly does that, and a malformed element has a syntax
        ;; object of #f's datum.

        (define (element-start-line e)
          (if (cst-token? e) (cst-token-start-line e) (cst-node-start-line e)))
        (define (element-start-col e)
          (if (cst-token? e) (cst-token-start-col e) (cst-node-start-col e)))
        (define (element-start-byte e)
          (if (cst-token? e) (cst-token-start-byte e) (cst-node-start-byte e)))
        (define (element-end-line* e)
          (if (cst-token? e) (cst-token-end-line e) (cst-node-end-line e)))
        (define (element-end-col* e)
          (if (cst-token? e) (cst-token-end-col e) (cst-node-end-col e)))
        (define (element-end-byte* e)
          (if (cst-token? e) (cst-token-end-byte e) (cst-node-end-byte e)))

        (define (span-sourcev first last)
          (make-sourcev filename
            (element-start-line first) (element-start-col first)
            (element-end-line* last) (element-end-col* last)
            (element-start-byte first) (element-end-byte* last)))

        (define (element-syntax e)
          (if (cst-token? e)
            (datum->syntax #f (cst-token-datum e) (span-sourcev e e))
            (or (cst-node-syntax e)
              (datum->syntax #f (cst-node-datum e) (span-sourcev e e)))))

        ;; The syntax objects of ELTS (no atmosphere), as a list, improper after a dot.
        (define (assemble-syntax elts)
          (define (abbrev-chain elts)
            ;; (cons syntax rest-elts)
            (let ((a (car elts)))
              (cond
                ((and (pair? (cdr elts))
                      (cst-token? (cadr elts))
                      (eq? (cst-token-kind (cadr elts)) 'abbrev))
                 (let* ((sub (abbrev-chain (cdr elts)))
                        (operand (car sub)))
                   (cons (datum->syntax #f (list (cst-token-extra a) operand)
                           (span-sourcev a (last-elt-of (cdr elts) (cdr sub))))
                     (cdr sub))))
                ((pair? (cdr elts))
                 (cons (datum->syntax #f
                         (list (cst-token-extra a) (element-syntax (cadr elts)))
                         (span-sourcev a (cadr elts)))
                   (cddr elts)))
                (else
                 (cons (datum->syntax #f (list (cst-token-extra a) #f) (span-sourcev a a))
                   (cdr elts))))))
          (let loop ((elts elts) (out '()))
            (cond
              ((null? elts) (reverse out))
              ((cst-token? (car elts))
               (case (cst-token-kind (car elts))
                 ((abbrev)
                  (let ((chain (abbrev-chain elts)))
                    (loop (cdr chain) (cons (car chain) out))))
                 ((dot)
                  ;; the tail is one datum, an abbreviation (`. ,x`) with its operand included
                  (let ((rest (if (pair? (cdr elts)) (assemble-syntax (cdr elts)) '())))
                    (if (and (pair? rest) (null? (cdr rest)))
                      (if (null? out) (car rest) (append (reverse out) (car rest)))
                      (reverse out))))
                 (else
                  (loop (cdr elts) (cons (element-syntax (car elts)) out)))))
              (else
               (loop (cdr elts) (cons (element-syntax (car elts)) out))))))

        ;; The last element of the operand chain that starts at ELTS and stops before REST.
        (define (last-elt-of elts rest)
          (let loop ((es elts) (last #f))
            (if (or (null? es) (eq? es rest))
              last
              (loop (cdr es) (car es)))))

        ;; The syntax of a top-level form: the one item, else the items as a list.
        (define (top-level-syntax elts first last)
          (let ((assembled (assemble-syntax elts)))
            (if (and (pair? assembled) (null? (cdr assembled)))
              (car assembled)
              (datum->syntax #f assembled (span-sourcev first last)))))

        (define (element-datum e)
          (if (cst-token? e)
            (cst-token-datum e)
            (cst-node-datum e)))

        ;; Datum of a top-level form: a single assembled item is the datum
        ;; itself (not a list). A top-level form is always one datum - for a
        ;; bare element that is the element's datum; for an abbreviation chain
        ;; it is the assembled `(quote ...)` chain.
        (define (top-level-datum elts)
          (let ((assembled (assemble-datum elts)))
            (if (and (pair? assembled) (null? (cdr assembled)))
              (car assembled)
              assembled)))

        ;; elts: non-atmosphere elements (tokens + nodes). Handles
        ;; abbreviations (including chains: `''x`) and dots producing improper
        ;; lists.
        (define (assemble-datum elts)
          (define (abbrev-chain elts)
            ;; elts starts with an abbrev token. Returns (cons datum rest-elts).
            (let ((a (car elts)))
              (cond
                ((and (pair? (cdr elts))
                      (cst-token? (cadr elts))
                      (eq? (cst-token-kind (cadr elts)) 'abbrev))
                 (let ((sub (abbrev-chain (cdr elts))))
                   (cons (list (cst-token-extra a) (car sub)) (cdr sub))))
                ((pair? (cdr elts))
                 (cons (list (cst-token-extra a) (element-datum (cadr elts)))
                   (cddr elts)))
                (else
                 (add-token-error! 'read/missing-operand
                   "abbreviation without operand" a)
                 (cons (list (cst-token-extra a) #f) (cdr elts))))))
          (let loop ((elts elts) (out '()))
            (cond
              ((null? elts) (reverse out))
              ((cst-token? (car elts))
               (case (cst-token-kind (car elts))
                 ((abbrev)
                  (let ((chain (abbrev-chain elts)))
                    (loop (cdr chain) (cons (car chain) out))))
                 ((dot)
                  (let ((rest (if (pair? (cdr elts)) (assemble-datum (cdr elts)) '())))
                   (if (and (pair? rest) (null? (cdr rest)))
                    (let ((tail (car rest)))
                      (if (null? out)
                        tail
                        (append (reverse out) tail)))
                    (begin
                      (add-token-error! 'read/misplaced-dot
                        "misplaced dot" (car elts))
                      (reverse out)))))
                 (else
                  (loop (cdr elts) (cons (element-datum (car elts)) out)))))
              (else
               (loop (cdr elts) (cons (element-datum (car elts)) out))))))

        (define (finish-compound kind children open-token close-token)
          (define elems
            (filter (lambda (t)
                      (not (or (atmosphere-token? t)
                               (and (cst-token? t)
                                 (memq (cst-token-kind t) '(open close))))))
              children))
          (define datum
            (guard (exn (else #f))
              (case kind
                ((list) (assemble-datum elems))
                ((vector) (list->vector (assemble-datum elems)))
                ((bytevector)
                 (u8-list->bytevector (map element-datum elems)))
                (else #f))))
          ;; An unclosed list ends where its last element ends.
          (define last-elem
            (let loop ((cs (reverse children)))
              (cond ((null? cs) open-token)
                    ((atmosphere-token? (car cs)) (loop (cdr cs)))
                    (else (car cs)))))
          (define (end-line e)
            (if (cst-token? e) (cst-token-end-line e) (cst-node-end-line e)))
          (define (end-col e)
            (if (cst-token? e) (cst-token-end-col e) (cst-node-end-col e)))
          (define (end-byte e)
            (if (cst-token? e) (cst-token-end-byte e) (cst-node-end-byte e)))
          (define end-elem (or close-token last-elem))
          (define stx
            (guard (exn (else #f))
              (let ((sv (span-sourcev open-token end-elem)))
                (case kind
                  ((list) (datum->syntax #f (assemble-syntax elems) sv))
                  ((vector) (datum->syntax #f (list->vector (assemble-syntax elems)) sv))
                  (else (datum->syntax #f datum sv))))))
          (%make-cst-node kind children datum stx
            (cst-token-start-line open-token)
            (cst-token-start-col open-token)
            (end-line end-elem)
            (end-col end-elem)
            (cst-token-start-byte open-token)
            (end-byte end-elem)))

        ;; Top-level loop: accumulate atmosphere tokens as form preludes.
        (define (parse-top-level)
          (let loop ((prelude '()) (forms '()) (trailing '()))
            (let ((t (read-raw!)))
              (cond
                ((not t)
                 (%make-cst-document text filename
                   (reverse forms)
                   (append (reverse errors) (token-stream-warnings))
                   (reverse (append trailing prelude))))
                ((and (cst-token? t) (atmosphere-token? t))
                 (loop (cons t prelude) forms trailing))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'close))
                 (add-token-error! 'read/unexpected-close
                   "unexpected closing delimiter" t)
                 (loop prelude forms trailing))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'dot))
                 (add-token-error! 'read/unexpected-dot "unexpected dot" t)
                 (loop prelude forms trailing))
                (else
                 (let* ((elts (parse-element-from-token! t))
                        (first (car elts))
                        (last (list-ref elts (- (length elts) 1)))
                        (sl (if (cst-token? first)
                              (cst-token-start-line first)
                              (cst-node-start-line first)))
                        (sc (if (cst-token? first)
                              (cst-token-start-col first)
                              (cst-node-start-col first)))
                        (el (if (cst-token? last)
                              (cst-token-end-line last)
                              (cst-node-end-line last)))
                        (ec (if (cst-token? last)
                              (cst-token-end-col last)
                              (cst-node-end-col last)))
                        (sb (if (cst-token? first)
                              (cst-token-start-byte first)
                              (cst-node-start-byte first)))
                        (eb (if (cst-token? last)
                              (cst-token-end-byte last)
                              (cst-node-end-byte last)))
                        (datum
                          (guard (exn (else #f))
                            (top-level-datum
                              (filter (lambda (x) (not (atmosphere-token? x)))
                                elts))))
                        (node (%make-cst-node 'form elts datum #f
                                 sl sc el ec sb eb))
                        (syntax (or (guard (exn (else #f))
                                      (top-level-syntax
                                        (filter (lambda (x) (not (atmosphere-token? x))) elts)
                                        first last))
                                  (datum->syntax #f datum
                                    (make-sourcev filename sl sc el ec sb eb)))))
                   (loop '()
                     (cons (%make-cst-form (reverse prelude) node syntax
                               '() sl sc el ec sb eb)
                       forms)
                     trailing)))))))

        (define (parse-element-from-token! t)
          ;; Like parse-element! but starting from an already-read token.
          (cond
            ((and (cst-token? t) (eq? (cst-token-kind t) 'open))
             (list (parse-compound! t)))
            ((and (cst-token? t) (eq? (cst-token-kind t) 'abbrev))
             (cons t (parse-element!)))
            (else (list t))))

        (parse-top-level)))))

  (define (cst-read-document text filename)
    (cst-parse-tail text filename 1 0 0 'capy))

  ;; Errors that suggest unbalanced delimiters, where indentation recovery
  ;; may do better than plain delimiter matching.
  (define (delimiter-errors? doc)
    (exists (lambda (e)
              (memq (cst-error-code e)
                '(read/unclosed read/mismatched read/unexpected-close)))
      (cst-document-errors doc)))

  ;; Incremental re-parse: a document for NEW-TEXT reusing OLD-DOC's forms up
  ;; to the first change (their bytes are identical in the new text), then
  ;; re-parsing the remainder with cst-parse-tail. Returns a document, or #f
  ;; when an incremental parse is not applicable (caller falls back to a full
  ;; parse). Parse cost scales with the distance from the edit to EOF.
  (define (cst-read-document-incremental old-doc new-text filename)
    (let* ((old-text (cst-document-text old-doc))
           (old-forms (cst-document-forms old-doc))
           (b (and (pair? old-forms) (first-diff-char old-text new-text))))
      (if (not b)
        #f
        (let* ((b-byte (char-index->byte old-text b))
               (k (first-affected-form old-forms b-byte))
               (tail-start (and k (> k 0)
                              (cst-form-start-byte (list-ref old-forms k)))))
          ;; A `#!` directive (`#!fold-case`, `#!r6rs`, ...) changes how the text after it reads,
          ;; and a tail read on its own starts with the default mode: a tail is only read when
          ;; nothing before it is a directive.
          (if (or (not tail-start) (directive-before? new-text (byte->char-index new-text tail-start)))
            #f
            (let* ((tail (substring new-text
                           (byte->char-index new-text tail-start)))
                   (base (byte->line-col new-text tail-start))
                   (tdoc (cst-parse-tail tail filename
                           (car base) (cdr base) tail-start 'capy))
                   (reused (take-forms old-forms k))
                   (tail-sl (car base))
                   (tail-sc (cdr base)))
              (%make-cst-document new-text filename
                (append reused (cst-document-forms tdoc))
                (append
                  ;; errors in the unchanged region keep their diagnostics;
                  ;; approximate by position
                  (filter (lambda (e)
                            (pos< (cst-error-start-line e)
                              (cst-error-start-col e) tail-sl tail-sc))
                      (cst-document-errors old-doc))
                  (cst-document-errors tdoc))
                (cst-document-trailing tdoc))))))))

  ;; Whether the text before the character END has `#!` in it (after the first line when that is
  ;; a shebang): conservatively, a directive.
  (define (directive-before? text end)
    (let ((from (if (and (>= (string-length text) 3) (string=? (substring text 0 3) "#!/")) 3 0)))
      (let loop ((i from))
        (cond
          ((>= (+ i 1) end) #f)
          ((and (char=? (string-ref text i) #\#) (char=? (string-ref text (+ i 1)) #\!)) #t)
          (else (loop (+ i 1)))))))

  ;; Char index of the first differing character between A and B, or #f when
  ;; they are identical.
  (define (first-diff-char a b)
    (let ((n (min (string-length a) (string-length b))))
      (let loop ((i 0))
        (cond
          ((>= i n)
           (if (= (string-length a) (string-length b)) #f n))
          ((char=? (string-ref a i) (string-ref b i))
           (loop (+ i 1)))
          (else i)))))

  ;; Byte offset of char index I in TEXT.
  (define (char-index->byte text i)
    (let loop ((j 0) (bytes 0))
      (if (>= j i)
        bytes
        (loop (+ j 1) (+ bytes (utf8-length (string-ref text j)))))))

  ;; Char index of the BYTE offset in TEXT.
  (define (byte->char-index text byte)
    (let loop ((i 0) (bytes 0))
      (if (or (>= bytes byte) (>= i (string-length text)))
        i
        (loop (+ i 1) (+ bytes (utf8-length (string-ref text i)))))))

  ;; (line . col) of the BYTE offset in TEXT (1-based line, 0-based col).
  (define (byte->line-col text byte)
    (let loop ((i 0) (bytes 0) (line 1) (col 0))
      (cond
        ((>= bytes byte) (cons line col))
        ((>= i (string-length text)) (cons line col))
        (else
         (let ((ch (string-ref text i)))
           (if (line-break-at? text i)
             (loop (+ i 1) (+ bytes 1) (+ line 1) 0)
             (loop (+ i 1) (+ bytes (utf8-length ch)) line (+ col 1))))))))

  ;; Index of the first form to parse again after a change at BYTE: the last form that starts
  ;; at or before it. That form is the one the change is in, or the one before the whitespace
  ;; and comments it is in (an unclosed form can grow into them), and its start is the same
  ;; byte in the old and the new text. #f when the change is before the first form (a full
  ;; parse is as cheap) or no form starts before it.
  (define (first-affected-form forms byte)
    (let loop ([i 0] [forms forms] [last #f])
      (if (or (null? forms) (> (cst-form-start-byte (car forms)) byte))
        last
        (loop (+ i 1) (cdr forms) i))))

  ;; The first K forms of FORMS.
  (define (take-forms forms k)
    (let loop ((fs forms) (i 0) (out '()))
      (if (or (null? fs) (>= i k))
        (reverse out)
        (loop (cdr fs) (+ i 1) (cons (car fs) out)))))

  ;; Is (l1 c1) strictly before (l2 c2)?
  (define (pos< l1 c1 l2 c2)
    (or (< l1 l2) (and (= l1 l2) (< c1 c2))))

  ;; Valid input is parsed with plain delimiter matching. Only when that
  ;; reports unbalanced delimiters is the text parsed again with indentation
  ;; recovery (a column-0 `(` closes the lists still open), so that formatting
  ;; of correct code can never be misread.
  (define (cst-parse-tail text filename base-line base-col base-byte reader-mode)
    (let ((doc (cst-parse-tail* text filename base-line base-col base-byte
                 reader-mode #f)))
      (if (delimiter-errors? doc)
        (cst-parse-tail* text filename base-line base-col base-byte
          reader-mode #t)
        doc)))

  (define (cst-walk node visitor)
    (visitor node)
    (when (cst-node? node)
      (for-each (lambda (c) (cst-walk c visitor))
        (cst-node-children node))))

  ;; First token containing (line, col); #f if none. Useful for hit testing.
  (define (cst-token-at-position node line col)
    (define best #f)
    (cst-walk node
      (lambda (n)
        (when (cst-token? n)
          (let ((sl (cst-token-start-line n))
                (sc (cst-token-start-col n))
                (el (cst-token-end-line n))
                (ec (cst-token-end-col n)))
            (when (and (or (< sl line) (and (= sl line) (<= sc col)))
                     (or (< line el) (and (= line el) (< col ec))))
              (set! best n))))))
    best)

