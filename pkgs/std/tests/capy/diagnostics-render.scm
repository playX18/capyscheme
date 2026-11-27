(import (rnrs)
  (srfi 64)
  (capy)
  (core conditions)
  (core diagnostics)
  (capy diagnostics render))

(test-begin "capy diagnostics render")

;; The expected text: each argument is a line.
(define (lines . ls)
  (apply string-append (map (lambda (l) (string-append l "\n")) ls)))

(define (span l c el ec) (make-span "app.scm" l c el ec #f #f))

;; ---------------------------------------------------------------------------------------
;; Single line, with a help and a note
;; ---------------------------------------------------------------------------------------

(test-equal "single-line label, help and note"
  (lines
    "error[unbound-variable]: unbound variable `lenght`"
    " --> app.scm:2:4"
    "  |"
    "2 |   (lenght xs))"
    "  |    ^^^^^^ not bound in this scope"
    "  |"
    "  = help: a binding with a similar name exists: `length`"
    "  = note: in this expansion of `my-loop` (util.scm:3:2)"
    "")
  (diagnostic->string
    (make-diagnostic 'error 'unbound-variable "unbound variable `lenght`" (span 2 3 2 9)
      (list (make-label (span 2 3 2 9) 'primary "not bound in this scope"))
      (list "in this expansion of `my-loop` (util.scm:3:2)")
      (list "a binding with a similar name exists: `length`"))
    "(define (main xs)\n  (lenght xs))\n"))

(test-equal "a primary span without labels is underlined"
  (lines
    "warning[unused]: unused import"
    " --> app.scm:1:2"
    "  |"
    "1 | (import (foo))"
    "  |  ^^^^^^"
    "")
  (diagnostic->string
    (make-diagnostic 'warning 'unused "unused import" (span 1 1 1 7))
    "(import (foo))\n"))

(test-equal "a diagnostic without a position"
  (lines
    "error[read/unclosed]: cannot continue"
    "  = note: nothing to point at"
    "")
  (diagnostic->string
    (make-diagnostic 'error 'read/unclosed "cannot continue" #f '() (list "nothing to point at"))))

;; ---------------------------------------------------------------------------------------
;; Multi-line span with a secondary label
;; ---------------------------------------------------------------------------------------

(test-equal "multi-line span with a secondary label"
  (lines
    "error[bad-form]: bad let"
    " --> app.scm:2:3"
    "  |"
    "1 |   (define (f x)"
    "  |    ------ in this definition"
    "2 | /   (let ((y 1))"
    "3 | |     (g x"
    "4 | |        y)))"
    "  | |__________^ this form is wrong"
    "")
  (diagnostic->string
    (make-diagnostic 'error 'bad-form "bad let" (span 2 2 4 10)
      (list (make-label (span 2 2 4 10) 'primary "this form is wrong")
        (make-label (span 1 1 1 7) 'secondary "in this definition")))
    "(define (f x)\n  (let ((y 1))\n    (g x\n       y)))\n"))

(test-equal "long spans are elided"
  (lines
    "error[long]: long"
    " --> app.scm:1:1"
    "  |"
    "1 | / (a"
    "2 | |  b"
    "..."
    "7 | |  g)"
    "  | |___^ all of it"
    "")
  (diagnostic->string
    (make-diagnostic 'error 'long "long" (span 1 0 7 4)
      (list (make-label (span 1 0 7 4) 'primary "all of it")))
    "(a\n b\n c\n d\n e\n f\n g)\n"))

;; ---------------------------------------------------------------------------------------
;; Several labels on a line, tabs, code points
;; ---------------------------------------------------------------------------------------

(test-equal "two labels on a line, tabs expanded"
  (lines
    "warning[tabs]: tabs"
    " --> app.scm:1:3"
    "  |"
    "1 |     (foo    bar baz)"
    "  |      ^^^     - y"
    "  |      |"
    "  |      x"
    "")
  (diagnostic->string
    (make-diagnostic 'warning 'tabs "tabs" (span 1 2 1 5)
      (list (make-label (span 1 2 1 5) 'primary "x")
        (make-label (span 1 7 1 8) 'secondary "y")))
    "\t(foo\tbar baz)\n"))

(test-equal "columns are code points"
  (lines
    "error[e]: m"
    " --> app.scm:1:6"
    "  |"
    "1 | (\x3bb;\x3bb;\x3bb; x)"
    "  |      ^"
    "")
  (diagnostic->string
    (make-diagnostic 'error 'e "m" (span 1 5 1 6)
      (list (make-label (span 1 5 1 6) 'primary "")))
    "(\x3bb;\x3bb;\x3bb; x)\n"))

;; ---------------------------------------------------------------------------------------
;; Conditions
;; ---------------------------------------------------------------------------------------

(define use-sv (vector "app.scm" 3 2 3 15 #f #f 'read '()))
(define macro-sv (vector "util.scm" 1 4 1 9 #f #f 'macro (list use-sv)))

(let* ((form (datum->syntax #f '(oops) macro-sv))
       (c (condition
            (make-syntax-violation form #f)
            (make-who-condition 'my-loop)
            (make-message-condition "bad loop")
            (make-expansion-trace-condition
              (list (list (cons 'macro 'my-loop) (cons 'use-site use-sv))))))
       (d (condition->diagnostic c)))
  (test-equal "macro-origin spans are re-pointed at the use site"
    '(3 2 3 15)
    (let ((p (diagnostic-primary d)))
      (list (span-line p) (span-col p) (span-end-line p) (span-end-col p))))
  (test-equal "the file is the use site's" "app.scm" (span-file (diagnostic-primary d)))
  (test-equal "the code falls back to the who condition" 'my-loop (diagnostic-code d))
  (test-equal "macro note"
    (lines
      "error[my-loop]: bad loop"
      " --> app.scm:3:3"
      "  |"
      "3 |   (my-loop 1 2)"
      "  |   ^^^^^^^^^^^^^"
      "  |"
      "  = note: the error originates in a macro expansion; the span is the macro use site"
      "  = note: in this expansion of `my-loop` (app.scm:3:3)"
      "")
    (diagnostic->string d "\n\n  (my-loop 1 2)\n")))

(let* ((sv (vector "app.scm" 1 4 1 7 5 8 'read '()))
       (sub (datum->syntax #f 'bar sv))
       (form (datum->syntax #f '(foo bar) (vector "app.scm" 1 0 1 8 1 9 'read '())))
       (c (condition (make-syntax-violation form sub)
            (make-message-condition "bad subform")
            (make-diagnostic-info-condition 'bad-subform '() '("a note") '("a help") '())))
       (d (condition->diagnostic c)))
  (test-equal "the diagnostic-info code wins" 'bad-subform (diagnostic-code d))
  (test-equal "the subform is primary" '(4 7 5 8)
    (let ((p (diagnostic-primary d)))
      (list (span-col p) (span-end-col p) (span-start-byte p) (span-end-byte p))))
  (test-equal "the form is a secondary label" '(secondary)
    (map label-style (cdr (diagnostic-labels d))))
  (test-equal "notes and helps" '(("a note") ("a help"))
    (list (diagnostic-notes d) (diagnostic-helps d))))

(let ((d (guard (e (#t (condition->diagnostic e)))
           (error 'compile "unbound variable" 'foo))))
  (test-equal "who is the code" 'compile (diagnostic-code d))
  (test-equal "irritants are part of the message" "unbound variable: foo" (diagnostic-message d))
  (test-equal "no position" #f (diagnostic-primary d))
  (test-equal "severity" 'error (diagnostic-severity d)))

;; ---------------------------------------------------------------------------------------
;; Plain data, summary
;; ---------------------------------------------------------------------------------------

(let ((d (make-diagnostic 'error 'x "m" (span 1 0 1 1)
           (list (make-label (span 1 0 1 1) 'primary "t")) '("n") '("h")
           (list (make-suggestion "use y" (span 1 0 1 1) "y" 'machine-applicable)))))
  (test-assert "a diagnostic is a diagnostic" (diagnostic? d))
  (test-equal "it survives write and read"
    d
    (call-with-port (open-string-input-port
                      (call-with-string-output-port (lambda (p) (write d p))))
      read)))

(test-equal "summary with errors and a warning"
  "error: could not compile (app main) due to 3 previous errors; 1 warning emitted"
  (diagnostics-summary-line "(app main)" 3 1))
(test-equal "summary with one error"
  "error: could not compile `x` due to 1 previous error"
  (diagnostics-summary-line "`x`" 1 0))
(test-equal "summary with warnings only"
  "warning: `x` generated 2 warnings"
  (diagnostics-summary-line "`x`" 0 2))
(test-equal "no summary" #f (diagnostics-summary-line "`x`" 0 0))

(test-end "capy diagnostics render")
