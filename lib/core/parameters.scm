
(library (core parameters)

  (export make-parameter parameterize current-input-port
          current-output-port current-error-port)

  (import (core intrinsics)
          (only (core primitives) make-parameter procedure? current-input-port
                current-output-port current-error-port))

;;; With the SRFI-39 and R7RS semantics, we have to bypass a call to the
;;; conversion procedure, which is done by passing the no-conversion symbol
;;; as an extra argument.  That extra argument is recognized only by real
;;; parameters, so we have to be careful.

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((p1 e1) ...) b1 b2 ...)
     (letrec-syntax 
         ((parameterize-aux
           (... (syntax-rules ()
                  ((parameterize-aux (t ...) ((p0 e0) x ...) body1 body2 ...)
                   (let ((tempE e0)
                         (tempP p0)
                         (first-time? #t))
                     (parameterize-aux ((tempE tempP first-time?) t ...) 
                                       (x ...) 
                                       body1 body2 ...)))
                  ((parameterize-aux ((tE tP first-time?) ...) ()
                    body1 body2 ...)
                   (let-syntax ((swap!
                                 (syntax-rules ()
                                   ((swap! var param)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (param tmp)))
                                   ((swap! var param flag)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (if #f
                                          (param tmp flag)
                                          (param tmp)))))))
                     (dynamic-wind
                      (lambda ()
                        (begin
                         (if first-time?
                             (swap! tE tP)
                             (swap! tE tP 'no-conversion))
                         (set! first-time? #f))
                        ...)
                      (lambda ()
                        body1 body2 ...)
                      (lambda ()
                        (swap! tE tP 'no-conversion) ...))))))))
       (parameterize-aux () ((p1 e1) ...) b1 b2 ...))))))

;;; eof
