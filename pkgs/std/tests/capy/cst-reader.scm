(import (rnrs)
  (srfi 64)
  (capy)
  (capy cst-reader))

(test-begin "capy cst-reader")

(define (parse text) (cst-read-document text "test.scm"))

(define (error-codes doc) (map cst-error-code (cst-document-errors doc)))

(define (error-pos e)
  (list (cst-error-start-line e) (cst-error-start-col e)))

(define (form-spans doc)
  (map (lambda (f)
         (list (cst-form-start-line f) (cst-form-start-col f)
           (cst-form-end-line f) (cst-form-end-col f)
           (cst-form-start-byte f) (cst-form-end-byte f)))
    (cst-document-forms doc)))

;; ---------------------------------------------------------------------------------------
;; Spans and multibyte text
;; ---------------------------------------------------------------------------------------

(let ((doc (parse "(define \x3bb; \"\xe9;\")\n(foo \x1f600; x)")))
  (test-equal "no errors" '() (cst-document-errors doc))
  (test-equal "two forms" 2 (length (cst-document-forms doc)))
  ;; line 1 is 14 code points and 16 bytes, then a newline
  (test-equal "form spans: columns in code points, offsets in bytes"
    '((1 0 1 14 0 16) (2 0 2 9 17 29))
    (form-spans doc)))

(let* ((doc (parse "(a \x3bb;)\n(b \x1f600; c)"))
       (stx (cst-form-syntax (cadr (cst-document-forms doc))))
       (sv (syntax-sourcev stx)))
  (test-equal "sourcev start byte" 7 (vector-ref sv 5))
  (test-equal "sourcev end byte" 17 (vector-ref sv 6))
  (test-equal "sourcev line" 2 (vector-ref sv 1))
  (test-equal "sourcev end column" 7 (vector-ref sv 4))
  (test-equal "sourcev file" "test.scm" (vector-ref sv 0))
  (test-equal "syntax datum" (list 'b (string->symbol "\x1f600;") 'c) (syntax->datum stx)))

(let* ((doc (parse "(a \x3bb;)"))
       (node (cst-form-node (car (cst-document-forms doc))))
       (toks (let ((acc '()))
               (cst-walk node (lambda (n) (when (cst-token? n) (set! acc (cons n acc)))))
               (reverse acc)))
       (lam (find (lambda (t) (and (eq? (cst-token-kind t) 'identifier)
                                (eq? (cst-token-datum t) '\x3bb;)))
              toks)))
  (test-assert "found the identifier token" lam)
  (test-equal "token bytes" '(3 5) (list (cst-token-start-byte lam) (cst-token-end-byte lam)))
  (test-equal "token columns" '(3 4) (list (cst-token-start-col lam) (cst-token-end-col lam))))

;; ---------------------------------------------------------------------------------------
;; Error codes
;; ---------------------------------------------------------------------------------------

(let ((doc (parse "(a (b c")))
  (test-equal "unclosed list at end of file" '(read/unclosed read/unclosed) (error-codes doc))
  (test-equal "unclosed delimiters are reported at their opening position"
    '((1 3) (1 0))
    (map error-pos (cst-document-errors doc)))
  (test-equal "the partial form is a closed list" '(a (b c))
    (syntax->datum (cst-form-syntax (car (cst-document-forms doc)))))
  (test-equal "the partial form ends at its last element" '(1 7 7)
    (let ((f (car (cst-document-forms doc))))
      (list (cst-form-end-line f) (cst-form-end-col f) (cst-form-end-byte f)))))

(let ((doc (parse "(a ]")))
  (test-equal "mismatched delimiter" '(read/mismatched) (error-codes doc))
  (test-equal "mismatch points at the closer" '(1 3) (error-pos (car (cst-document-errors doc))))
  (test-equal "mismatch carries the opener as a related label"
    '((1 0 1 1 "the delimiter opened here"))
    (cst-error-related (car (cst-document-errors doc)))))

(let ((doc (parse "(a))")))
  (test-equal "unexpected closer" '(read/unexpected-close) (error-codes doc))
  (test-equal "one form is kept" 1 (length (cst-document-forms doc))))

(test-equal "misplaced dot" '(read/misplaced-dot) (error-codes (parse "(a . b c)")))
(test-equal "dot outside a list" '(read/dot-in-datum) (error-codes (parse "#(a . b)")))
(test-equal "dot at top level" '(read/unexpected-dot) (error-codes (parse ". a")))
(test-equal "valid text has no errors" '()
  (error-codes (parse "(a . b) [c d] #(1 2) #u8(1 2) 'x `(,y ,@z) ; c\n#| b |# #;d e")))

;; ---------------------------------------------------------------------------------------
;; Indentation recovery
;; ---------------------------------------------------------------------------------------

(let* ((text "(define (f x)\n  (g x\n(define (h) 1)\n(display 2)\n")
       (doc (parse text)))
  (test-equal "recovered forms" 3 (length (cst-document-forms doc)))
  (test-equal "every unclosed list is reported" '(read/unclosed read/unclosed) (error-codes doc))
  (test-equal "reported at the opening parenthesis, innermost first"
    '((2 2) (1 0))
    (map error-pos (cst-document-errors doc)))
  (test-equal "the later forms are intact"
    '((define (h) 1) (display 2))
    (map (lambda (f) (syntax->datum (cst-form-syntax f)))
      (cdr (cst-document-forms doc))))
  (test-equal "the broken form is a closed list ending before the recovery line"
    '(define (f x) (g x))
    (syntax->datum (cst-form-syntax (car (cst-document-forms doc)))))
  (test-equal "and ends where its last element ends" '(2 6)
    (let ((f (car (cst-document-forms doc))))
      (list (cst-form-end-line f) (cst-form-end-col f))))
  (test-equal "the recovery point is a related label"
    '((3 0 3 1 "a closing delimiter was expected before this line"))
    (cst-error-related (car (cst-document-errors doc)))))

;; Correct code with a column-0 parenthesis inside a list is not touched.
(let ((doc (parse "(a\n(b)\n)\n(c)")))
  (test-equal "no recovery for balanced input" '() (cst-document-errors doc))
  (test-equal "forms" 2 (length (cst-document-forms doc))))

;; ---------------------------------------------------------------------------------------
;; Incremental parsing keeps byte offsets right
;; ---------------------------------------------------------------------------------------

(let* ((old "(a \x3bb;)\n(b 1)\n(c \x1f600;)")
       (new "(a \x3bb;)\n(b 10)\n(c \x1f600;)")
       (doc (parse old))
       (inc (cst-read-document-incremental doc new "test.scm"))
       (full (parse new)))
  (test-assert "incremental parse applies" inc)
  (test-equal "same spans as a full parse" (form-spans full) (form-spans inc)))

;; A change in the whitespace and comments between forms, or right after a form, is part of what
;; is parsed again (it used to be dropped: the new text was parsed from the old offset).
(let* ((old "(0)\n(a)\n\n; note\n(b)\n(c)\n(d)")
       (doc (parse old)))
  (define (check name new)
    (let ((inc (cst-read-document-incremental doc new "test.scm"))
          (full (parse new)))
      (test-assert (string-append name ": applies") inc)
      (test-equal (string-append name ": spans") (form-spans full) (form-spans inc))
      (test-equal (string-append name ": data")
        (map (lambda (f) (syntax->datum (cst-form-syntax f))) (cst-document-forms full))
        (map (lambda (f) (syntax->datum (cst-form-syntax f))) (cst-document-forms inc)))
      (test-equal (string-append name ": errors")
        (map cst-error-code (cst-document-errors full))
        (map cst-error-code (cst-document-errors inc)))))
  (check "typing in a blank line" "(0)\n(a)\nx\n; note\n(b)\n(c)\n(d)")
  (check "typing in a comment" "(0)\n(a)\n\n; a note\n(b)\n(c)\n(d)")
  (check "a form after a form" "(0)\n(a)\n\n; note\n(b)(z)\n(c)\n(d)")
  (check "gluing onto a form" "(0)\n(a)\n\n; note\n(b)q\n(c)\n(d)")
  (check "an unclosed form" "(0)\n(a)\n\n; note\n(b\n(c)\n(d)")
  (check "after the last form" "(0)\n(a)\n\n; note\n(b)\n(c)\n(d)\n(e"))

;; ---------------------------------------------------------------------------------------
;; Every sub-form has a syntax object with its own span
;; ---------------------------------------------------------------------------------------

;; The (datum line col end-line end-col) of the syntax objects below STX, outermost first.
(define (syntax-spans stx)
  (let walk ((x stx))
    (cond
      ((syntax? x)
       (let ((sv (syntax-sourcev x)) (e (syntax-expression x)))
         (cons (list (syntax->datum x) (vector-ref sv 1) (vector-ref sv 2) (vector-ref sv 3) (vector-ref sv 4))
           (walk e))))
      ((pair? x) (append (walk (car x)) (walk (cdr x))))
      ((vector? x) (apply append (map walk (vector->list x))))
      (else '()))))

(let ((doc (parse "(f 'x (g . y))\n[h #(1 z)]")))
  (test-equal "syntax objects carry their own spans"
    '(((f (quote x) (g . y)) 1 0 1 14)
      (f 1 1 1 2)
      ((quote x) 1 3 1 5)
      (x 1 4 1 5)
      ((g . y) 1 6 1 13)
      (g 1 7 1 8)
      (y 1 11 1 12))
    (syntax-spans (cst-form-syntax (car (cst-document-forms doc)))))
  (test-equal "a vector in brackets"
    '(((h #(1 z)) 2 0 2 10)
      (h 2 1 2 2)
      (#(1 z) 2 3 2 9)
      (1 2 5 2 6)
      (z 2 7 2 8))
    (syntax-spans (cst-form-syntax (cadr (cst-document-forms doc))))))

;; An unclosed list still has syntax objects for what it holds.
(let ((doc (parse "(foo (bar baz")))
  (test-equal "unclosed list"
    '(((foo (bar baz)) 1 0 1 13)
      (foo 1 1 1 4)
      ((bar baz) 1 5 1 13)
      (bar 1 6 1 9)
      (baz 1 10 1 13))
    (syntax-spans (cst-form-syntax (car (cst-document-forms doc))))))

;; Malformed input never raises, and the forms around it survive.
(define (error-codes doc) (map cst-error-code (cst-document-errors doc)))
(test-assert "unclosed datum comment does not raise"
  (cst-document? (parse "#;(foo")))
(test-assert "datum comment before a bracket does not raise"
  (cst-document? (parse "#;(a ]")))
(test-assert "datum comment with nothing after it does not raise"
  (cst-document? (parse "(a #;)")))
(let ((doc (parse "(a ')")))
  (test-equal "an abbreviation does not take the closing paren"
    1 (length (cst-document-forms doc)))
  (test-assert "its missing operand is reported"
    (memq 'read/missing-operand (error-codes doc))))
(test-equal "an abbreviation skips whitespace to its operand"
  '(x)
  (map (lambda (f) (cadr (syntax->datum (cst-form-syntax f))))
    (cst-document-forms (parse "' x"))))

;; A token that reaches the end of the text ends there, not a column after it.
(define (last-token-end text)
  (let* ((doc (parse text))
         (tokens '()))
    (for-each
      (lambda (f)
        (for-each (lambda (t) (set! tokens (cons t tokens))) (cst-form-prelude f))
        (cst-walk (cst-form-node f) (lambda (t) (when (cst-token? t) (set! tokens (cons t tokens))))))
      (cst-document-forms doc))
    (for-each (lambda (t) (set! tokens (cons t tokens))) (cst-document-trailing doc))
    (let ((t (car (list-sort (lambda (a b)
                               (or (> (cst-token-end-line a) (cst-token-end-line b))
                                 (and (= (cst-token-end-line a) (cst-token-end-line b))
                                   (> (cst-token-end-col a) (cst-token-end-col b)))))
                   tokens))))
      (list (cst-token-end-line t) (cst-token-end-col t)))))
(test-equal "a comment at the end of the text" '(1 5) (last-token-end "; eof"))
(test-equal "an unclosed block comment" '(1 7) (last-token-end "#| open"))
(test-equal "an unclosed datum comment" '(1 6) (last-token-end "#;(foo"))
(test-equal "a comment on the last line" '(2 3) (last-token-end "(a)\n; x"))

;; The reader of the compiler's datums: abbreviations name what they stand for, and one in the
;; tail of a dotted list is that tail.
(define (datum-of text) (map (lambda (f) (syntax->datum (cst-form-syntax f))) (cst-document-forms (parse text))))
(test-equal "#' is syntax" '((syntax a)) (datum-of "#'a"))
(test-equal "#` and #, are quasisyntax and unsyntax"
  '((quasisyntax (a (unsyntax b) (unsyntax-splicing c)))) (datum-of "#`(a #,b #,@c)"))
(test-equal "an unquote in the tail" '((a unquote x)) (datum-of "(a . ,x)"))
(test-equal "a quote in the tail" '((a quote x)) (datum-of "(a . 'x)"))
(test-equal "no misplaced dot for it" '() (map cst-error-code (cst-document-errors (parse "`(a . ,(f x))"))))
(test-equal "its syntax has the spans"
  '((1 6 1 12))
  (let ((tail (cdr (syntax-expression (cadr (syntax-expression (cst-form-syntax (car (cst-document-forms (parse "`(a . ,(f x))"))))))))))
    (list (let ((sv (syntax-sourcev tail))) (list (vector-ref sv 1) (vector-ref sv 2) (vector-ref sv 3) (vector-ref sv 4))))))


;; ---------------------------------------------------------------------------------------
;; Tokens that read others, line comments, warnings and directives
;; ---------------------------------------------------------------------------------------

(define (token-list text)
  (let* ((doc (parse text)) (acc '()))
    (define (add! n) (when (cst-token? n) (set! acc (cons n acc))))
    (for-each
      (lambda (f)
        (for-each (lambda (n) (cst-walk n add!)) (cst-form-prelude f))
        (cst-walk (cst-form-node f) add!))
      (cst-document-forms doc))
    (for-each (lambda (n) (cst-walk n add!)) (cst-document-trailing doc))
    (map (lambda (t)
           (list (cst-token-kind t) (cst-token-start-line t) (cst-token-start-col t)
             (cst-token-end-line t) (cst-token-end-col t)))
      (reverse acc))))

(test-equal "a datum comment and a directive start where they are"
  '((datum-comment 1 6 1 13) (directive 1 20 1 25))
  (filter (lambda (t) (memq (car t) '(datum-comment directive)))
    (token-list "(foo) #;(bar) (baz) #!eof\n")))

(test-equal "a line comment ends where its text does, and its line break is whitespace"
  '((comment 1 0 1 4) (whitespace 1 4 2 0) (whitespace 2 3 2 4) (comment 2 4 2 8))
  (filter (lambda (t) (memq (car t) '(comment whitespace)))
    (token-list "; hi\n(x) ; yo")))

(test-equal "the line break of a comment that ends a CRLF line"
  '((comment 1 0 1 4) (whitespace 1 4 2 0))
  (filter (lambda (t) (memq (car t) '(comment whitespace)))
    (token-list "; hi\r\n(x)")))

(let ((doc (parse "\"a\\qb\" #\\foo")))
  (test-equal "a warning spans the token it is in"
    '((1 0 1 6) (1 7 1 12))
    (map (lambda (e)
           (list (cst-error-start-line e) (cst-error-start-col e)
             (cst-error-end-line e) (cst-error-end-col e)))
      (filter (lambda (e) (eq? (cst-error-code e) 'read/lexical)) (cst-document-errors doc)))))

(let* ((old (parse "#!fold-case\n(Foo)\n(Bar)\n"))
       (new (cst-read-document-incremental old "#!fold-case\n(Foo)\n(Bar)\n(Baz)\n" "test.scm"))
       (doc (or new (parse "#!fold-case\n(Foo)\n(Bar)\n(Baz)\n"))))
  (test-equal "forms after a fold-case directive are read with it, in an incremental parse too"
    '((foo) (bar) (baz))
    (map (lambda (f) (syntax->datum (cst-form-syntax f))) (cst-document-forms doc))))

(test-end "capy cst-reader")
