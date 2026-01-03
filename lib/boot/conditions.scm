

(define &condition
  (let* ([rtd (make-record-type-descriptor '&condition #f #f #f #f '#())]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&condition rtd rcd)))

(define (compound-condition-component obj) (tuple-ref obj 1))

(define condition
  (lambda components
    (tuple
      'type:condition
      (apply append
        (map (lambda (component) (simple-conditions component)) components)))))

(define (compound-condition? obj)
  (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:condition)))

(define (simple-condition? obj)
  (and (record? obj) (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj))))

(define (condition? obj)
  (or (simple-condition? obj) (compound-condition? obj)))

(define (simple-conditions c)
  (if (simple-condition? c)
    (list c)
    (if (compound-condition? c)
      (compound-condition-component c)
      #f)))

(define (condition-predicate rtd)
  (lambda (obj)
    (if (simple-condition? obj)
      (rtd-ancestor? rtd (record-rtd obj))
      (if (compound-condition? obj)
        (any1 (lambda (c) (rtd-ancestor? rtd (record-rtd c))) (compound-condition-component obj))
      #f))))

(define (condition-accessor rtd proc)
  (define wrong-type
    (lambda (rtd obj)
      (assertion-violation
        "condition accessor"
        (format #f "expected condition of a subtype of ~a, but got ~a" rtd obj) rtd obj)))

  (or (rtd-ancestor? (record-type-rtd &condition) rtd)
      (assertion-violation
        'condition-accessor
        (format #f "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd) rtd proc))
  (lambda (obj)

    (if (simple-condition? obj)
      (begin
        (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj))
        (proc obj))
      (if (compound-condition? obj)
        (let ([res (any1 (lambda (c) (and (rtd-ancestor? rtd (record-rtd c)) c)) (compound-condition-component obj))])
          (if res (proc res) (wrong-type rtd obj)))
        (wrong-type rtd obj)))))



(define &message
  (let ([rtd (make-record-type-descriptor '&message (record-type-rtd &condition) #f #f #f '#((immutable message)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&message rtd rcd))))

(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))

(define &source
  (let ([rtd (make-record-type-descriptor '&source (record-type-rtd &condition) #f #f #f '#((immutable file) (immutable line) (immutable column)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&source rtd rcd))))

(define make-source-condition (record-constructor (record-type-rcd &source)))
(define source-condition? (condition-predicate (record-type-rtd &source)))
(define condition-source-file (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 0)))
(define condition-source-line (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 1)))
(define condition-source-column (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 2)))

(define (make-condition-uid) #f)


(define (print-condition exn p)
    (define (print-syntax form subform)
        (define form-src (if (syntax? form) (syntax-sourcev form) #f))
        (define subform-src (if (and subform (syntax? subform)) (syntax-sourcev subform) #f))
        (define (fmt-source src)
          (define file (vector-ref src 0))
          (define line (vector-ref src 1))
          (define col  (vector-ref src 2))
          (format p "~a:~a:~a" file line col))
        (format p "~a" (syntax->datum form))
        (when form-src 
          (format p "~%       in ")
          (fmt-source form-src))
        (when subform
          (format p "~a" (syntax->datum subform))
          (when subform-src 
              (format p "~%     in")
              (fmt-source subform-src))))

    (cond 
        [(condition? exn)
            (let ([c* (simple-conditions exn)])
                (format p "The condition has ~a components:~%" (length c*))
                (do ([i 1 (+ 1 i)]
                    [c* c* (cdr c*)])
                    [(null? c*)]
                    (let* ([c (car c*)]
                        [rtd (record-rtd c)])
                        (format p " ~a. " i)
                        (let ([supress-type
                                (and (eq? (record-type-parent rtd)
                                    (record-type-rtd &condition))
                                (let ((name (symbol->string (record-type-name rtd)))
                                        (fields (record-type-field-names rtd)))
                                    (and (not (eqv? 0 (string-length name)))
                                        (char=? (string-ref name 0) #\&)
                                        (fx>? (vector-length fields) 0)
                                        (string=? (substring name 1 (string-length name))
                                                (symbol->string (vector-ref fields 0))))))])
                            (if supress-type 
                                (put-char p #\&)
                                (let loop ([rtd rtd])
                                    (format p "~a" (record-type-name rtd))
                                    (cond 
                                        [(record-type-parent rtd) => 
                                            (lambda (parent)
                                                (unless (eq? parent (record-type-rtd &condition))
                                                (format p " ")
                                                (loop parent)))])))
                            (let loop ([rtd rtd])
                                (do ([f* (record-type-field-names rtd)]
                                    [i 0 (+ i 1)])
                                    [(= i (vector-length f*))
                                        (cond [(record-type-parent rtd) => loop])]
                                    (unless (and supress-type (eqv? i 0))
                                        (format p "~%     "))
                                    (format p "~a: " (vector-ref f* i))
                                    (let ([x ((record-accessor rtd i) c)])
                                        (cond 
                                            [(and (eq? rtd (record-type-rtd &syntax)))
                                                (print-syntax (syntax-violation-form c) (syntax-violation-subform c))]
                                            [(and (eq? rtd (record-type-rtd &irritants))
                                                (pair? x)
                                                (list? x))
                                                (display "(" p)
                                                (write (car x) p)
                                                (for-each 
                                                    (lambda (x)
                                                    (display "\n                 " p)
                                                    (write x p))
                                                    (cdr x))
                                                (display ")" p)]
                                            [else (write x p)]))))))
                            (newline p)))]
                    [else 
                        (format p "A non-condition object was raised:~%~s" exn)]))