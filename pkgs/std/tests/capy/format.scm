(import (rnrs)
  (srfi 64)
  (srfi 39)
  (capy)
  (capy cst-reader)
  (capy format))

(test-begin "capy format")

(define (fmt text) (format-text text))

;; ---------------------------------------------------------------------------------------
;; Goldens
;; ---------------------------------------------------------------------------------------

(test-equal "body forms"
  "(define (f x)\n  (let loop ((i 0)\n             (acc '()))\n    (when (< i x)\n      (loop (+ i 1) (cons i acc)))))\n"
  (fmt "(define (f x)\n(let loop ((i 0)\n(acc '()))\n        (when (< i x)\n   (loop (+ i 1) (cons i acc)))))\n"))

(test-equal "calls align with the first argument"
  "(foo 1\n     2\n     (bar 3\n          4))\n"
  (fmt "(foo 1\n 2\n    (bar 3\n 4))"))

(test-equal "a call with nothing after its head indents by 1"
  "(foo\n (bar 1)\n (baz 2))\n"
  (fmt "(foo\n      (bar 1)\n(baz 2))\n"))

(test-equal "a list that starts with a list aligns with its first element"
  "(let ((a 1)\n      (b 2))\n  (+ a b))\n"
  (fmt "(let ((a 1)\n(b 2))\n(+ a b))\n"))

(test-equal "named let and do"
  "(let loop ((i 0))\n  (do ((j 0 (+ j 1)))\n      ((= j 3))\n    (display j)))\n"
  (fmt "(let loop ((i 0))\n(do ((j 0 (+ j 1)))\n((= j 3))\n(display j)))\n"))

(test-equal "quoted data and vectors align with their first element"
  "'(a b\n  c d)\n#(1 2\n  3)\n`(x ,(f 1\n        2))\n"
  (fmt "'(a b\nc d)\n#(1 2\n3)\n`(x ,(f 1\n2))\n"))

(test-equal "libraries"
  "(library (a b)\n  (export f g)\n  (import (rnrs)\n    (capy))\n  (define (f) 1)\n  (define-record-type point\n    (fields x y)))\n"
  (fmt "(library (a b)\n(export f g)\n(import (rnrs)\n(capy))\n(define (f) 1)\n(define-record-type point\n(fields x y)))\n"))

(test-equal "syntax-rules"
  "(define-syntax swap!\n  (syntax-rules ()\n    ((_ a b)\n     (let ((t a))\n       (set! a b)\n       (set! b t)))))\n"
  (fmt "(define-syntax swap!\n(syntax-rules ()\n((_ a b)\n(let ((t a))\n(set! a b)\n(set! b t)))))\n"))

(test-equal "comments are indented like the form that follows"
  "(define (f)\n  ;; one\n  (g)\n  ; two\n  #| three |#\n  (h))\n;; top\n;;; heading\n(x)\n"
  (fmt "(define (f)\n;; one\n    (g)\n  ; two\n#| three |#\n(h))\n    ;; top\n   ;;; heading\n(x)\n"))

(test-equal "trailing comments and spacing inside a line are kept"
  "(foo  a   b) ; keep   this\n(bar) ;; and this\n"
  (fmt "(foo  a   b) ; keep   this   \n(bar) ;; and this\t\n"))

(test-equal "multi-line strings and block comments are verbatim"
  "(f \"a\n   b  \n c\"\n   x)\n#| one\n     two\n|#\n(g)\n"
  (fmt "(f \"a\n   b  \n c\"\n x)\n#| one\n     two\n|#\n  (g)\n"))

(test-equal "datum comments are opaque"
  "(f a\n   #;(b\n  c)\n   d)\n"
  (fmt "(f a\n#;(b\n  c)\n  d)\n"))

(test-equal "characters that are blanks keep them"
  "(list #\\space #\\ \n      x)\n"
  (fmt "(list #\\space #\\  \n x)\n"))

(test-equal "blank lines are emptied, line breaks are kept, one newline at the end"
  "(a)\n\n\n(b)\n"
  (fmt "(a)   \n  \n\t\n(b)\n\n\n  \n"))

(test-equal "text outside Latin-1 (wide strings) is formatted like any other"
  "(define (f x)\n  (list \"\x1f600;\" x\n        x))\n; \x3bb;\n"
  (fmt "(define (f x)\n(list \"\x1f600;\" x\nx))  \n; \x3bb; \n"))

(test-equal "missing newline at the end" "(a)\n" (fmt "(a)"))
(test-equal "empty text" "" (fmt ""))
(test-equal "only blanks" "" (fmt " \n\n  "))
(test-equal "CRLF" "(a\r\n b)\r\n" (fmt "(a  \r\n      b)  \r\n"))

(test-equal "text with read errors is returned as it is"
  "(define (f x)\n    (+ x 1)  \n"
  (fmt "(define (f x)\n    (+ x 1)  \n"))

(test-equal "an extra rule"
  "(my-with x\n  (a)\n  (b))\n"
  (format-text "(my-with x\n(a)\n(b))\n" '((indent-rules . ((my-with . 1))))))

(test-equal "a rule that says align overrides the table"
  "(let a\n     b)\n"
  (format-text "(let a\nb)\n" '((indent-rules . ((let . align))))))

(test-equal "the rules of a parameter"
  "(my-with x\n  (a)\n  (b))\n"
  (parameterize ([current-indent-rules '((my-with . 1))])
    (fmt "(my-with x\n(a)\n(b))\n")))

(test-equal "a document can be formatted too"
  "(a\n b)\n"
  (format-text (cst-read-document "(a\n    b)" "t")))

;; ---------------------------------------------------------------------------------------
;; Edits
;; ---------------------------------------------------------------------------------------

;; Applies edits (lines from 1, columns in code points) to TEXT, last to first.
(define (apply-edits text edits)
  (define (offset line col)
    (let loop ([i 0] [l 1])
      (if (= l line)
        (+ i col)
        (loop (+ (let find ([j i]) (if (char=? (string-ref text j) #\newline) (+ j 1) (find (+ j 1)))) 0)
          (+ l 1)))))
  (let loop ([edits (list-sort (lambda (a b) (or (> (car a) (car b))
                                                (and (= (car a) (car b)) (> (cadr a) (cadr b)))))
                      edits)]
             [text text])
    (if (null? edits)
      text
      (let* ([e (car edits)]
             [s (offset (car e) (cadr e))]
             [t (offset (caddr e) (cadddr e))])
        (loop (cdr edits)
          (string-append (substring text 0 s) (list-ref e 4) (substring text t (string-length text))))))))

(let ([text "(define (f x)\n(+ x\n1))\n\n(define (g)\n    (h))   \n"])
  (test-equal "full edits give the formatted text"
    (fmt text)
    (apply-edits text (format-edits text)))
  (test-equal "no edits for formatted text" '() (format-edits (fmt text)))
  (test-equal "a range reaches the top-level forms it touches"
    "(define (f x)\n  (+ x\n     1))\n\n(define (g)\n    (h))   \n"
    (apply-edits text (format-range text '(2 . 0) '(2 . 3))))
  (test-equal "a range that ends at a line start leaves that line"
    "(define (f x)\n  (+ x\n     1))\n\n(define (g)\n    (h))   \n"
    (apply-edits text (format-range text '(1 . 0) '(4 . 0))))
  (test-equal "a range over blank lines between forms"
    "(define (f x)\n(+ x\n1))\n\n(define (g)\n    (h))   \n"
    (apply-edits text (format-range text '(4 . 0) '(4 . 0)))))

(let ([text "(define (f x)\n   \n  x)"])
  (test-equal "a newline indents the new line"
    "(define (f x)\n  \n  x)"
    (apply-edits text (format-on-type text '(2 . 3) #\newline)))
  (test-equal "other characters do nothing" '() (format-on-type text '(2 . 3) #\a)))

(let ([text "(let ((a 1))\n        (f a))"])
  (test-equal "a closing paren re-indents its line"
    "(let ((a 1))\n  (f a))"
    (apply-edits text (format-on-type text '(2 . 14) #\)))))

;; ---------------------------------------------------------------------------------------
;; The sources of the repository
;; ---------------------------------------------------------------------------------------

(define (source-files dir)
  (append-map
    (lambda (name)
      (let ([path (string-append dir "/" name)]
            [n (string-length name)])
        (cond
          [(file-directory? path) (source-files path)]
          [(and (> n 4) (member (substring name (- n 4) n) '(".scm" ".sls"))) (list path)]
          [else '()])))
    (list-sort string<? (directory-list dir))))

(define (read-file path)
  (call-with-input-file path (lambda (port) (get-string-all port))))

(define (without-whitespace text)
  (list->string (filter (lambda (c) (not (char-whitespace? c))) (string->list text))))

(define (trim-right s)
  (let loop ([n (string-length s)])
    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
      (loop (- n 1))
      (substring s 0 n))))

;; The sorted tokens of TEXT as strings, ignoring blanks and the trailing blanks of comments.
(define (token-multiset text)
  (let ([doc (cst-read-document text "f")] [acc '()])
    (define (add! t)
      (unless (memq (cst-token-kind t) '(whitespace datum-comment))
        (set! acc (cons (string-append (symbol->string (cst-token-kind t)) ":"
                         (trim-right (cst-token-text t)))
                   acc))))
    (for-each
      (lambda (form)
        (for-each add! (cst-form-prelude form))
        (cst-walk (cst-form-node form) (lambda (n) (when (cst-token? n) (add! n)))))
      (cst-document-forms doc))
    (for-each add! (cst-document-trailing doc))
    (list-sort string<? acc)))

;; every package of the repository (the test runs in pkgs/std)
(define files (source-files ".."))
(test-assert "there are sources" (> (length files) 100))

(let loop ([files files] [bad-idempotent '()] [bad-tokens '()] [changed 0])
  (if (null? files)
    (begin
      (test-equal "formatting twice gives the same text in every source" '() bad-idempotent)
      (test-equal "formatting keeps the tokens of every source" '() bad-tokens)
      (test-assert "the formatter changes sources that are not formatted its way" (> changed 0)))
    (let* ([path (car files)]
           [text (read-file path)]
           [once (format-text text)]
           [twice (format-text once)])
      (loop (cdr files)
        (if (string=? once twice) bad-idempotent (cons path bad-idempotent))
        (if (and (string=? (without-whitespace text) (without-whitespace once))
                 (equal? (token-multiset text) (token-multiset once)))
          bad-tokens
          (cons path bad-tokens))
        (if (string=? once text) changed (+ changed 1))))))

(test-end)
