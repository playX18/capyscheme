(define F 42.42)

(define x (* (float-significand F) (expt 2 (float-exponent F))))
(format #t "x = ~a~%" (exact->inexact x))