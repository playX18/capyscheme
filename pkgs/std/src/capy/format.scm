;;; A comment-preserving formatter for Scheme source, built on the CST reader.
;;;
;;;   (format-text "(define (f x)\n(+ x 1))\n")  => "(define (f x)\n  (+ x 1))\n"
;;;
;;; The formatter normalises indentation and trailing whitespace and leaves everything else
;;; alone: it never joins or splits lines and never moves a token, so comments (`;`, `#| |#`,
;;; `#;`), strings, characters and the spacing inside a line come through exactly as written.
;;; The lines of a multi-line string, block comment or datum comment are left as they are.
;;; Formatting is idempotent. Text that does not read without errors is returned unchanged.
;;;
;;; The indentation of a line that starts with a token is decided by the innermost list that
;;; contains it, the head of that list and how many elements precede the token:
;;;
;;;   * a data list (quoted, quasiquoted or a vector) lines its elements up with the first one;
;;;   * a list whose head is not a symbol lines up with its first element too;
;;;   * a list headed by a symbol with a rule N in the indentation table is a body form: its
;;;     first N arguments are indented by 4 and the rest (the body) by 2 from the open paren;
;;;   * any other call lines its arguments up with the first one, or indents by 1 when there is
;;;     none yet.
;;;
;;; The table is an alist of (name . N), where N is a non-negative integer or `align` (the
;;; default for calls). `default-indent-rules` is the built-in table; the rules of the
;;; `indent-rules` option and then those of the parameter `current-indent-rules` come before it.
;;;
;;; Edits are lists (start-line start-col end-line end-col new-text) in the coordinates of the
;;; CST reader: lines from 1, columns from 0 in code points.

(library (capy format)
  (export format-text
    format-edits
    format-range
    format-on-type
    default-indent-rules
    current-indent-rules)
  (import (rnrs)
    (rnrs hashtables)
    (capy)
    (capy cst-reader))

  (define default-indent-rules
    '((begin . 0) (import . 0) (export . 0) (case-lambda . 0) (cond-expand . 0)
      (match-lambda . 0) (delay . 0) (delay-force . 0)
      (define . 1) (define-inline . 1) (define-syntax . 1) (define-values . 1)
      (define-record-type . 1) (define-library . 1) (library . 1) (define-macro . 1)
      (lambda . 1) (named-lambda . 1)
      (let . 1) (let* . 1) (letrec . 1) (letrec* . 1) (let-values . 1) (let*-values . 1)
      (let-syntax . 1) (letrec-syntax . 1) (fluid-let . 1) (parameterize . 1)
      (when . 1) (unless . 1) (case . 1) (guard . 1) (match . 1)
      (syntax-rules . 1) (with-syntax . 1) (syntax-case . 2) (do . 2)
      (call-with-port . 1) (call-with-input-file . 1) (call-with-output-file . 1)
      (with-mutex . 1) (let/ec . 1) (let/cc . 1)))

  (define current-indent-rules (make-parameter '()))

  (define (rules->table user)
    (let ([table (make-hashtable string-hash string=?)])
      (for-each
        (lambda (rule)
          (let ([name (if (symbol? (car rule)) (symbol->string (car rule)) (car rule))])
            (unless (hashtable-contains? table name)
              (hashtable-set! table name (cdr rule)))))
        (append user (current-indent-rules) default-indent-rules))
      table))

  (define (option options key default)
    (let ([p (and (pair? options) (assq key options))])
      (if p (cdr p) default)))

  ;; ---- Items: the tokens of a document, as spans of its text ----

  ;; kind is one of ws comment block dcomment directive open close dot abbrev atom string char;
  ;; extra is the bracket kind of an open, the symbol of an abbreviation, or #t for an identifier.
  (define (make-item kind start end extra) (vector kind start end extra))
  (define (item-kind i) (vector-ref i 0))
  (define (item-start i) (vector-ref i 1))
  (define (item-end i) (vector-ref i 2))
  (define (item-extra i) (vector-ref i 3))

  (define (blank-char? c) (or (char=? c #\space) (char=? c #\tab)))

  (define (all-whitespace? text start end)
    (let loop ([i start])
      (or (>= i end)
        (and (char-whitespace? (string-ref text i)) (loop (+ i 1))))))

  (define (count-blank text start end)
    (let loop ([i start])
      (if (and (< i end) (blank-char? (string-ref text i))) (loop (+ i 1)) (- i start))))

  (define (line-starts text)
    (let loop ([i 0] [acc '(0)])
      (cond
        [(>= i (string-length text)) (list->vector (reverse acc))]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (cons (+ i 1) acc))]
        [else (loop (+ i 1) acc)])))

  (define (document-tokens doc)
    (let ([acc '()])
      (define (add! t) (set! acc (cons t acc)))
      (for-each
        (lambda (form)
          (for-each add! (cst-form-prelude form))
          (cst-walk (cst-form-node form) (lambda (n) (when (cst-token? n) (add! n)))))
        (cst-document-forms doc))
      (for-each add! (cst-document-trailing doc))
      (reverse acc)))

  ;; The vector of items covering the text of DOC, or #f when the tokens do not account for
  ;; it. The lexer reports a datum comment at the position of its last token and a directive
  ;; without its `#!`: those two items start where the previous token ended.
  (define (document-items doc starts)
    (define text (cst-document-text doc))
    (define len (string-length text))
    (define (offset line col)
      (if (<= 1 line (vector-length starts)) (+ (vector-ref starts (- line 1)) col) -1))
    (let loop ([tokens (document-tokens doc)] [prev 0] [out '()])
      (cond
        [(null? tokens)
          (cond
            [(= prev len) (list->vector (reverse out))]
            [(all-whitespace? text prev len)
              (list->vector (reverse (cons (make-item 'ws prev len #f) out)))]
            [else #f])]
        [else
          (let* ([t (car tokens)]
                 [s (offset (cst-token-start-line t) (cst-token-start-col t))]
                 [e (offset (cst-token-end-line t) (cst-token-end-col t))]
                 [kind (cst-token-kind t)])
            (cond
              [(or (< s 0) (< e s) (> e len)) #f]
              [(memq kind '(datum-comment directive))
                (and (<= prev e)
                  (loop (cdr tokens) e
                    (cons (make-item (if (eq? kind 'directive) 'directive 'dcomment) prev e #f) out)))]
              [(< s prev) #f]
              [else
                (let ([item
                       (case kind
                         [(whitespace) (make-item 'ws s e #f)]
                         [(comment shebang) (make-item 'comment s e #f)]
                         [(block-comment) (make-item 'block s e #f)]
                         [(open) (make-item 'open s e (cst-token-extra t))]
                         [(close) (make-item 'close s e #f)]
                         [(dot) (make-item 'dot s e #f)]
                         [(abbrev) (make-item 'abbrev s e (cst-token-extra t))]
                         [(identifier) (make-item 'atom s e #t)]
                         [(string) (make-item 'string s e #f)]
                         [(char) (make-item 'char s e #f)]
                         [(boolean keyword number) (make-item 'atom s e #f)]
                         [else #f])])
                  (and item
                    (cond
                      [(= s prev) (loop (cdr tokens) e (cons item out))]
                      [(all-whitespace? text prev s)
                        (loop (cdr tokens) e (cons item (cons (make-item 'ws prev s #f) out)))]
                      [else #f])))]))])))

  ;; ---- Indentation ----

  (define-record-type (<frame> make-frame frame?)
    (fields
      (immutable col frame-col)
      (immutable len frame-len)
      (immutable data? frame-data?)
      (mutable n frame-n set-frame-n!)
      (mutable head-col frame-head-col set-frame-head-col!)
      (mutable head frame-head set-frame-head!)
      (mutable arg-col frame-arg-col set-frame-arg-col!)
      (mutable arg-ident? frame-arg-ident? set-frame-arg-ident?!)))

  ;; The column of a line that starts with the Nth element (from 0) of the list of F.
  (define (indent-for f table n)
    (cond
      [(not f) 0]
      [(frame-data? f)
        (if (= n 0) (+ (frame-col f) (frame-len f)) (frame-head-col f))]
      [(= n 0) (+ (frame-col f) 1)]
      [(not (frame-head f)) (frame-head-col f)]
      [else
        (let ([rule (hashtable-ref table (frame-head f) 'align)])
          (cond
            [(integer? rule)
              (let ([distinguished
                     (if (and (string=? (frame-head f) "let") (frame-arg-ident? f))
                       (+ rule 1)
                       rule)])
                (+ (frame-col f) (if (<= n distinguished) 4 2)))]
            [(= n 1) (+ (frame-col f) 1)]
            [else (frame-arg-col f)]))]))

  ;; ---- The formatter ----

  ;; The new lines of DOC (strings without their newline), as a vector, or #f when DOC cannot
  ;; be formatted. The blank lines in BLANK-TARGETS (line numbers from 1) get the indentation a
  ;; form there would have, instead of being emptied.
  (define (format-lines doc table blank-targets)
    (define text (cst-document-text doc))
    (define len (string-length text))
    (define starts (line-starts text))
    (define nlines (vector-length starts))
    (define items
      (and (not (exists (lambda (e) (eq? (cst-error-severity e) 'error))
                 (cst-document-errors doc)))
        (document-items doc starts)))
    ;; the offset of the newline of line LI, or the end of the text
    (define (line-end li)
      (if (< (+ li 1) nlines) (- (vector-ref starts (+ li 1)) 1) len))
    (and items
      (let ([protected (make-bytevector (+ len 1) 0)]
            [indent (make-vector nlines #f)]
            [delta (make-vector nlines 0)])
        (define (protect! s e)
          (do ([i s (+ i 1)]) ((>= i e))
            (bytevector-u8-set! protected i 1)))
        (define (start-protected? li)
          (and (> li 0)
            (let ([s (vector-ref starts li)])
              (and (= (bytevector-u8-ref protected (- s 1)) 1)
                (= (bytevector-u8-ref protected s) 1)))))
        (define (blank-line? li)
          (all-whitespace? text (vector-ref starts li) (line-end li)))
        (vector-for-each
          (lambda (item)
            (case (item-kind item)
              [(string block dcomment directive) (protect! (item-start item) (item-end item))]
              [(char)
                (when (blank-char? (string-ref text (- (item-end item) 1)))
                  (protect! (- (item-end item) 1) (item-end item)))]
              [else #f]))
          items)
        (let ([stack '()]
              [pending #f]
              [line 0]
              [targets (list-sort < (map (lambda (l) (- l 1)) blank-targets))])
          (define (top) (and (pair? stack) (car stack)))
          (define (line-of! s)
            (let loop ()
              (when (and (< (+ line 1) nlines) (<= (vector-ref starts (+ line 1)) s))
                (set! line (+ line 1))
                (loop)))
            line)
          (define (column s li) (+ (- s (vector-ref starts li)) (vector-ref delta li)))
          (define (element-index) (let ([f (top)]) (if f (frame-n f) 0)))
          (define (flush-targets! before)
            (let loop ()
              (when (and (pair? targets) (< (car targets) before))
                (let ([li (car targets)])
                  (when (and (< li nlines) (not (start-protected? li)) (blank-line? li))
                    (vector-set! indent li (indent-for (top) table (element-index)))))
                (set! targets (cdr targets))
                (loop))))
          (define (first-on-line? s li)
            (= (count-blank text (vector-ref starts li) s) (- s (vector-ref starts li))))
          (define (triple-semicolon? s)
            (and (<= (+ s 3) len) (string=? (substring text s (+ s 3)) ";;;")))
          (vector-for-each
            (lambda (item)
              (let* ([s (item-start item)]
                     [kind (item-kind item)]
                     [li (line-of! s)])
                (flush-targets! li)
                (unless (eq? kind 'ws)
                  (when (and (first-on-line? s li) (not (vector-ref indent li)))
                    (let ([old (count-blank text (vector-ref starts li) s)]
                          [new (if (and (eq? kind 'comment) (triple-semicolon? s))
                                0
                                (indent-for (top) table (element-index)))])
                      (vector-set! indent li new)
                      (vector-set! delta li (- new old))))
                  (let ([col (column s li)])
                    (case kind
                      [(open abbrev atom string char dot)
                        (let ([f (top)])
                          (when (and f (not pending))
                            (case (frame-n f)
                              [(0)
                                (set-frame-head-col! f col)
                                (when (and (eq? kind 'atom) (item-extra item))
                                  (set-frame-head! f (substring text s (item-end item))))]
                              [(1)
                                (set-frame-arg-col! f col)
                                (set-frame-arg-ident?! f (and (eq? kind 'atom) (item-extra item) #t))]
                              [else #f])
                            (set-frame-n! f (+ (frame-n f) 1))))
                        (case kind
                          [(abbrev) (set! pending (item-extra item))]
                          [(open)
                            (let* ([f (top)]
                                   [extra (item-extra item)]
                                   [data? (or (memq extra '(vector bytevector))
                                           (memq pending '(quote quasiquote))
                                           (and f (frame-data? f)
                                             (not (memq pending '(unquote unquote-splicing)))))])
                              (set! pending #f)
                              (set! stack
                                (cons (make-frame col (- (item-end item) s) (and data? #t)
                                       0 col #f col #f)
                                  stack)))]
                          [else (set! pending #f)])]
                      [(close)
                        (set! pending #f)
                        (when (pair? stack) (set! stack (cdr stack)))]
                      [else #f])))))
            items)
          (flush-targets! nlines))
        (let ([out (make-vector nlines "")])
          (do ([li 0 (+ li 1)]) ((= li nlines))
            (let* ([s (vector-ref starts li)]
                   [nl (line-end li)]
                   [cr? (and (> nl s) (char=? (string-ref text (- nl 1)) #\return))]
                   [e (if cr? (- nl 1) nl)]
                   ;; the trailing blanks that are not part of a token go
                   [e2 (let loop ([i e])
                        (if (and (> i s) (blank-char? (string-ref text (- i 1)))
                             (= (bytevector-u8-ref protected (- i 1)) 0))
                          (loop (- i 1))
                          i))]
                   [body
                    (cond
                      [(start-protected? li) (substring text s e2)]
                      [(vector-ref indent li)
                        (string-append (make-string (vector-ref indent li) #\space)
                          (substring text (+ s (count-blank text s e2)) e2))]
                      [(all-whitespace? text s e2) ""]
                      [else (substring text s e2)])])
              (vector-set! out li (if cr? (string-append body "\r") body))))
          out))))

  ;; The new lines with the blank ones at the end dropped and a newline at the end: the lines of
  ;; the text that `format-text` returns.
  (define (normalize-end lines)
    (let loop ([k (- (vector-length lines) 1)])
      (cond
        [(< k 0) (vector "")]
        [(string=? (string-trim-cr (vector-ref lines k)) "") (loop (- k 1))]
        [else
          (let ([v (make-vector (+ k 2) "")])
            (do ([i 0 (+ i 1)]) ((> i k))
              (vector-set! v i (vector-ref lines i)))
            v)])))

  (define (string-trim-cr s)
    (if (and (> (string-length s) 0) (char=? (string-ref s (- (string-length s) 1)) #\return))
      (substring s 0 (- (string-length s) 1))
      s))

  (define (join-lines lines)
    (let ([n (vector-length lines)])
      (call-with-string-output-port
        (lambda (port)
          (do ([i 0 (+ i 1)]) ((= i n))
            (when (> i 0) (write-char #\newline port))
            (put-string port (vector-ref lines i)))))))

  (define (->document x)
    (if (cst-document? x) x (cst-read-document x "format")))

  (define (split-lines text)
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(>= i (string-length text)) (list->vector (reverse (cons (substring text start i) acc)))]
        [(char=? (string-ref text i) #\newline)
          (loop (+ i 1) (+ i 1) (cons (substring text start i) acc))]
        [else (loop (+ i 1) start acc)])))

  ;; ---- Edits ----

  (define (common-prefix a b)
    (let ([n (min (string-length a) (string-length b))])
      (let loop ([i 0])
        (if (and (< i n) (char=? (string-ref a i) (string-ref b i))) (loop (+ i 1)) i))))

  (define (common-suffix a b limit)
    (let ([la (string-length a)] [lb (string-length b)])
      (let loop ([i 0])
        (if (and (< i limit)
             (char=? (string-ref a (- la i 1)) (string-ref b (- lb i 1))))
          (loop (+ i 1))
          i))))

  ;; The edit that turns the line OLD (number LINE, from 1) into NEW, or #f.
  (define (line-edit line old new)
    (if (string=? old new)
      #f
      (let* ([p (common-prefix old new)]
             [q (common-suffix old new (- (min (string-length old) (string-length new)) p))])
        (list line p line (- (string-length old) q) (substring new p (- (string-length new) q))))))

  ;; The edits turning the vector of lines OLD into NEW. Only the lines for which KEEP? holds
  ;; are edited; the lines the end of the text gains or loses are edited when TAIL? is true.
  (define (lines->edits old new keep? tail?)
    (let* ([m (vector-length old)] [k (vector-length new)] [common (min m k)])
      (append
        (let loop ([i 0] [acc '()])
          (if (= i common)
            (reverse acc)
            (loop (+ i 1)
              (let ([e (and (keep? (+ i 1)) (line-edit (+ i 1) (vector-ref old i) (vector-ref new i)))])
                (if e (cons e acc) acc)))))
        (cond
          [(not tail?) '()]
          [(> m k)
            ;; the lines after the last new one go, with the newline before them
            (list (list k (string-length (vector-ref old (- k 1))) m (string-length (vector-ref old (- m 1))) ""))]
          [(< m k)
            (let ([end (string-length (vector-ref old (- m 1)))])
              (list (list m end m end
                     (let loop ([i m] [acc ""])
                       (if (= i k) acc (loop (+ i 1) (string-append acc "\n" (vector-ref new i))))))))]
          [else '()]))))

  ;; ---- API ----

  ;; Formats X (a string or a CST document) and returns the new text. OPTIONS is an alist;
  ;; `indent-rules` adds indentation rules.
  (define format-text
    (case-lambda
      [(x) (format-text x '())]
      [(x options)
        (let* ([doc (->document x)]
               [table (rules->table (option options 'indent-rules '()))]
               [lines (format-lines doc table '())])
          (if lines
            (join-lines (normalize-end lines))
            (cst-document-text doc)))]))

  ;; The edits that format the whole text.
  (define format-edits
    (case-lambda
      [(x) (format-edits x '())]
      [(x options)
        (let* ([doc (->document x)]
               [table (rules->table (option options 'indent-rules '()))]
               [lines (format-lines doc table '())])
          (if lines
            (lines->edits (split-lines (cst-document-text doc)) (normalize-end lines)
              (lambda (l) #t) #t)
            '()))]))

  (define (edits-in-lines doc options first last blank-targets)
    (let* ([table (rules->table (option options 'indent-rules '()))]
           [lines (format-lines doc table blank-targets)])
      (if lines
        (lines->edits (split-lines (cst-document-text doc)) lines
          (lambda (l) (<= first l last)) #f)
        '())))

  ;; The edits that format the lines of the range START..END (pairs (line . col)), widened to
  ;; the top-level forms that it touches.
  (define format-range
    (case-lambda
      [(x start end) (format-range x start end '())]
      [(x start end options)
        (let* ([doc (->document x)]
               [first (car start)]
               [last (if (and (= (cdr end) 0) (> (car end) (car start))) (- (car end) 1) (car end))])
          (let loop ([forms (cst-document-forms doc)] [lo first] [hi last])
            (if (null? forms)
              (edits-in-lines doc options lo hi '())
              (let ([f (car forms)])
                (if (and (<= (cst-form-start-line f) last) (>= (cst-form-end-line f) first))
                  (loop (cdr forms) (min lo (cst-form-start-line f)) (max hi (cst-form-end-line f)))
                  (loop (cdr forms) lo hi))))))]))

  ;; The edits after the character CH was typed at POS, a pair (line . col) just after it: a
  ;; newline indents the new line and cleans the one before; a closing paren or bracket indents
  ;; its line.
  (define format-on-type
    (case-lambda
      [(x pos ch) (format-on-type x pos ch '())]
      [(x pos ch options)
        (let ([doc (->document x)]
              [ch (if (string? ch) (and (= (string-length ch) 1) (string-ref ch 0)) ch)]
              [line (car pos)])
          (cond
            [(eqv? ch #\newline)
              (edits-in-lines doc options (max 1 (- line 1)) line (list line))]
            [(memv ch '(#\) #\]))
              (edits-in-lines doc options line line '())]
            [else '()]))])))
