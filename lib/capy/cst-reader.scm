;; CST reader for CapyScheme.
;;
;; A concrete-syntax-tree front end for tooling (LSP, formatters, ...). Unlike
;; the boot reader it:
;;
;;   * never raises on malformed input — it produces a best-effort tree plus
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
    cst-error-message
    cst-error-start-line cst-error-start-col
    cst-error-end-line cst-error-end-col
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
      (immutable message cst-error-message)
      (immutable sl cst-error-start-line)
      (immutable sc cst-error-start-col)
      (immutable el cst-error-end-line)
      (immutable ec cst-error-end-col)))

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

  (define (make-line-table text)
    ;; entries[i] = (char-start . byte-start) of line i+1; a final sentinel
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
                 ((char=? (string-ref text i) #\newline)
                  (loop (+ i 1) (+ bytes (utf8-length (string-ref text i)))
                    out #t))
                 (else
                  (loop (+ i 1) (+ bytes (utf8-length (string-ref text i)))
                    out #f))))))
      (%make-line-index text entries (make-hashtable equal-hash equal?))))

  (define (line-index-line-count idx)
    (- (vector-length (line-index-entries idx)) 1))

  ;; Byte offset of (line, col) — 1-based line, 0-based col. Memoizes a
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
    (define (map-pos line col)
      (let* ((al (+ base-line line -1))
             (ac (if (= line 1) (+ base-col col) col))
             (ab (+ base-byte (line-index-byte-at idx line col))))
        (values al ac ab)))
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
        (receive (al ac _) (map-pos wl wc)
          (set! warnings
            (cons (%make-cst-error 'warning msg al ac al ac) warnings)))
        #f))
    (define (next-token)
      ;; Tolerant mode makes lexer-level issues continuable `&warning`s; catch
      ;; them, record them, and let the lexer continue. Anything
      ;; non-continuable re-raises and aborts the parse (reported by the outer
      ;; guard).
      (with-exception-handler
        (lambda (c)
          (if (warning? c)
            (record-warning! c)
            (raise c)))
        (lambda ()
          (call-with-values
            (lambda () (get-token reader))
            (lambda (lextype payload)
              (call-with-values
                (lambda ()
                  (map-pos (reader-saved-line reader)
                           (reader-saved-column reader)))
                (lambda (sl sc sb)
                  (call-with-values
                    (lambda () (map-pos (reader-line reader)
                                        (reader-column reader)))
                    (lambda (el ec eb)
                      (list lextype payload
                        (reader-saved-line reader) (reader-saved-column reader)
                        (reader-line reader) (reader-column reader)
                        sl sc sb el ec eb))))))))))
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

  (define (abbrev-symbol a)
    (case a
      ((syntax) 'quote)
      ((quasisyntax) 'quasiquote)
      ((unsyntax) 'unquote)
      ((unsyntax-splicing) 'unquote-splicing)
      (else a)))

  (define (make-sourcev filename sl sc el ec)
    (vector filename sl sc el ec #f #f 'read '()))

  ;; Parses `text` starting at base coordinates; returns a document. When
  ;; base-line/base-col/base-byte are 1/0/0 this is a full-document parse;
  ;; otherwise it parses a tail substring.
  (define (cst-parse-tail* text filename base-line base-col base-byte reader-mode)
    (call-with-values
      (lambda ()
        (make-token-stream text filename base-line base-col base-byte reader-mode))
      (lambda (next-token token-stream-warnings idx)
        (define errors '())
        (define (add-error! severity message sl sc el ec)
          (set! errors (cons (%make-cst-error severity message sl sc el ec) errors)))

        ;; Read one token; on eof returns #f. Token positions are mapped into
        ;; document coordinates; the raw (unmapped) positions are used to
        ;; slice the token text out of `text`.
        (define (read-raw!)
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
                  (if (memq lextype '(value identifier)) payload #f)
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
          (let ((t (read-raw!)))
            (cond
              ((not t) '())
              ((and (cst-token? t) (eq? (cst-token-kind t) 'open))
               (list (parse-compound! t)))
              ((and (cst-token? t) (eq? (cst-token-kind t) 'abbrev))
               (cons t (parse-element!)))
              (else (list t)))))

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
                 (add-error! 'error "unexpected end of file: unclosed list"
                   (cst-token-start-line open-token)
                   (cst-token-start-col open-token)
                   (cst-token-end-line open-token)
                   (cst-token-end-col open-token))
                 (finish-compound kind (reverse children) open-token #f))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'close))
                 (if (closes? (cst-token-extra t))
                   (finish-compound kind (reverse (cons t children)) open-token t)
                   (begin
                     (add-error! 'error "mismatched parenthesis/bracket"
                       (cst-token-start-line t)
                       (cst-token-start-col t)
                       (cst-token-end-line t)
                       (cst-token-end-col t))
                     (finish-compound kind (reverse (cons t children)) open-token t))))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'dot))
                 (if (eq? kind 'list)
                   (loop (cons t children))
                   (begin
                     (add-error! 'error "dot used in non-list datum"
                       (cst-token-start-line t)
                       (cst-token-start-col t)
                       (cst-token-end-line t)
                       (cst-token-end-col t))
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

        (define (element-datum e)
          (if (cst-token? e)
            (cst-token-datum e)
            (cst-node-datum e)))

        ;; Datum of a top-level form: a single assembled item is the datum
        ;; itself (not a list). A top-level form is always one datum — for a
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
            ;; elts starts with an abbrev token; returns (cons datum rest-elts).
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
                 (add-error! 'error "abbreviation without operand"
                   (cst-token-start-line a) (cst-token-start-col a)
                   (cst-token-end-line a) (cst-token-end-col a))
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
                  (if (and (pair? (cdr elts)) (null? (cddr elts)))
                    (let ((tail (element-datum (cadr elts))))
                      (if (null? out)
                        tail
                        (append (reverse out) tail)))
                    (begin
                      (add-error! 'error "misplaced dot"
                        (cst-token-start-line (car elts))
                        (cst-token-start-col (car elts))
                        (cst-token-end-line (car elts))
                        (cst-token-end-col (car elts)))
                      (reverse out))))
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
          (define end-token (or close-token open-token))
          (%make-cst-node kind children datum #f
            (cst-token-start-line open-token)
            (cst-token-start-col open-token)
            (cst-token-end-line end-token)
            (cst-token-end-col end-token)
            (cst-token-start-byte open-token)
            (cst-token-end-byte end-token)))

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
                 (add-error! 'error "unexpected closing delimiter"
                   (cst-token-start-line t)
                   (cst-token-start-col t)
                   (cst-token-end-line t)
                   (cst-token-end-col t))
                 (loop prelude forms trailing))
                ((and (cst-token? t) (eq? (cst-token-kind t) 'dot))
                 (add-error! 'error "unexpected dot"
                   (cst-token-start-line t)
                   (cst-token-start-col t)
                   (cst-token-end-line t)
                   (cst-token-end-col t))
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
                        (syntax (datum->syntax #f datum
                                  (make-sourcev filename sl sc el ec))))
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
          (if (not tail-start)
            #f
            (let* ((tail (substring new-text tail-start))
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

  ;; (line . col) of the BYTE offset in TEXT (1-based line, 0-based col).
  (define (byte->line-col text byte)
    (let loop ((i 0) (bytes 0) (line 1) (col 0))
      (cond
        ((>= bytes byte) (cons line col))
        ((>= i (string-length text)) (cons line col))
        (else
         (let ((ch (string-ref text i)))
           (if (char=? ch #\newline)
             (loop (+ i 1) (+ bytes 1) (+ line 1) 0)
             (loop (+ i 1) (+ bytes (utf8-length ch)) line (+ col 1))))))))

  ;; Index of the first form affected by a change at BYTE (the form whose
  ;; span contains BYTE, else the first form after it); #f when BYTE is at
  ;; the very start (full re-parse is just as cheap) or past all forms.
  (define (first-affected-form forms byte)
    (let loop ((i 0) (containing #f) (after #f))
      (if (>= i (length forms))
        (or containing after)
        (let ((f (list-ref forms i)))
          (cond
            ((<= (cst-form-start-byte f) byte
                 (cst-form-end-byte f))
             (loop (+ i 1) (or containing i) after))
            ((> (cst-form-start-byte f) byte)
             (loop (+ i 1) containing (or after i)))
            (else (loop (+ i 1) containing after)))))))

  ;; The first K forms of FORMS.
  (define (take-forms forms k)
    (let loop ((fs forms) (i 0) (out '()))
      (if (or (null? fs) (>= i k))
        (reverse out)
        (loop (cdr fs) (+ i 1) (cons (car fs) out)))))

  ;; Is (l1 c1) strictly before (l2 c2)?
  (define (pos< l1 c1 l2 c2)
    (or (< l1 l2) (and (= l1 l2) (< c1 c2))))

  (define (cst-parse-tail text filename base-line base-col base-byte reader-mode)
    (cst-parse-tail* text filename base-line base-col base-byte reader-mode))

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

