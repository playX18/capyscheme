;;; Compares the libmaps of two toolchain directories.
;;;
;;;   capy-rt -s scripts/compare-libmaps.scm -- stage-1/capy.libmap stage-2/capy.libmap

(import (rnrs) (capy libmap))

(define (generated? key) (and (pair? key) (>= (length key) 3) (equal? (list (car key) (cadr key) (caddr key)) '(capy c gen))))

(define (entries libmap)
  (list-sort (lambda (a b) (string<? (format-key (car a)) (format-key (car b))))
    (filter (lambda (entry) (not (generated? (car entry)))) (libmap-entries libmap))))

(define (format-key key) (call-with-string-output-port (lambda (p) (write key p))))

(define (key-set libmap)
  (map car (entries libmap)))

(let* ([args (cdr (command-line))]
       [a (read-libmap (car args))]
       [b (read-libmap (cadr args))]
       [ea (entries a)]
       [eb (entries b)]
       [bad '()])
  (define (note! what key) (set! bad (cons (string-append what " " (format-key key)) bad)))
  (for-each
    (lambda (entry)
      (let ([other (assoc (car entry) eb)])
        (cond
          [(not other) (note! "only in the first:" (car entry))]
          [(not (equal? entry other)) (note! "entry differs:" (car entry))])))
    ea)
  (for-each
    (lambda (entry) (unless (assoc (car entry) ea) (note! "only in the second:" (car entry))))
    eb)
  (unless (equal? (libmap-views a) (libmap-views b)) (set! bad (cons "the views differ" bad)))
  (unless (equal? (libmap-compiler-id a) (libmap-compiler-id b)) (set! bad (cons "the compiler ids differ" bad)))
  (for-each (lambda (line) (display line) (newline)) (reverse bad))
  (exit (if (null? bad) 0 1)))
