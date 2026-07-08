(import (scheme base)
  (scheme read)
  (srfi 64)
  (capy))

(test-begin "boot source spans")

(define (alist-ref alist key)
  (let ((entry (assq key alist)))
    (and entry (cdr entry))))

(let* ((stx (read-syntax (open-input-string "(a b)")))
       (src (syntax-sourcev stx)))
  (test-assert "reader source is a rich vector"
    (and (vector? src) (>= (vector-length src) 5)))
  (test-equal "reader source keeps legacy file slot" (vector-ref src 0) "*bytevector*")
  (test-equal "reader source keeps legacy line slot" (vector-ref src 1) 1)
  (test-equal "reader source keeps legacy column slot" (vector-ref src 2) 0)
  (test-equal "reader source records end line" (vector-ref src 3) 1)
  (test-equal "reader source records end column" (vector-ref src 4) 5))

(let* ((src (vector "legacy.scm" 7 2))
       (stx (datum->syntax #f 'x src))
       (roundtrip (syntax-sourcev stx)))
  (test-assert "legacy source vector is accepted"
    (and (vector? roundtrip) (= (vector-length roundtrip) 3)))
  (test-equal "legacy source line is preserved" (vector-ref roundtrip 1) 7)
  (test-equal "legacy source column is preserved" (vector-ref roundtrip 2) 2))

(let* ((src (vector "rich.scm" 2 1 2 8 #f #f 'read '()))
       (stx (datum->syntax #f 'x src))
       (roundtrip (syntax-sourcev stx))
       (props (syntax-source stx)))
  (test-equal "rich source vector is preserved" roundtrip src)
  (test-equal "syntax-source exposes end line" (alist-ref props 'end-line) 2)
  (test-equal "syntax-source exposes end column" (alist-ref props 'end-column) 8)
  (test-equal "syntax-source omits empty related spans" (assq 'related-spans props) #f))

(test-end "boot source spans")
