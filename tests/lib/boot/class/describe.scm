(import (srfi 64)
        (capy))

(test-begin "boot class describe helpers")

(test-group "describe helpers"
  (define-class describe-record ()
    ((name #:init-value 'capy)
     empty
     (%secret #:init-value 'hidden)))
  (define record (make describe-record))

  (test-assert "describe is a generic"
    (generic? describe))
  (test-assert "describe-slots is a generic"
    (generic? describe-slots))
  (test-equal "describe-details starts false"
    (describe-details)
    #f)
  (test-equal "describe-slots prints visible slots"
    (with-output-to-string
      (lambda ()
        (describe-slots record)))
    "slots:\n  name: capy\n  empty: #<unbound>\n")
  (test-assert "describe includes class name and slots"
    (let ([out (with-output-to-string
                 (lambda ()
                   (describe record)))])
      (and (string-contains out "describe-record")
           (string-contains out "slots:")
           (string-contains out "name: capy"))))
  (test-equal "describe-details setter returns old value"
    (describe-details #t)
    #f)
  (test-equal "describe-details exposes hidden slots"
    (with-output-to-string
      (lambda ()
        (describe-slots record)))
    "slots:\n  name: capy\n  empty: #<unbound>\n  %secret: hidden\n")
  (test-equal "describe-details can be restored"
    (describe-details #f)
    #t)
  (test-error "describe-details rejects non-booleans"
    &assertion-violation
    (describe-details 'yes)))

(test-end "boot class describe helpers")
