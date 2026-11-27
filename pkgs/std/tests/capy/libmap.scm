(import (rnrs) (srfi 64) (only (capy) microsecond) (capy libmap))

(define dir (string-append "/tmp/capy-libmap-test-" (number->string (microsecond))))
(define path (string-append dir ".libmap"))

(define view
  (make-package-view 'app '((app *))
    (list (list 'util '((util)) '(((util) . (my util)))))
    #f))

(define entries
  (list (list '(%pkg util util) "/src/util.scm" "/out/util.fasl" 0 "hash" #f)))

(test-begin "capy libmap")

(let ([libmap (make-libmap entries (list view))])
  (test-equal "entries" entries (libmap-entries libmap))
  (test-assert "view by package" (libmap-view libmap 'app))
  (test-equal "no view" #f (libmap-view libmap 'other))
  (write-libmap-file! path libmap)
  (let ([read (read-libmap path)])
    (test-equal "round trip: entries" entries (libmap-entries read))
    (test-equal "round trip: compiler id" (libmap-compiler-id libmap) (libmap-compiler-id read))
    (let ([v (libmap-view read 'app)])
      (test-equal "package of a view" 'app (view-package v))
      (test-equal "own library" '(|#%pkg| app app main) (view-library-key v '(app main)))
      (test-equal "renamed dependency" '(|#%pkg| util util) (view-library-key v '(my util)))
      (test-equal "std library" '(rnrs base) (view-library-key v '(rnrs base))))))

(test-error "not a libmap" (begin (call-with-output-file dir (lambda (p) (display "x" p))) (read-libmap dir)))
(test-assert "no temporary file left" (not (file-exists? (string-append path ".tmp"))))

(test-end)
