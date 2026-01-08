(define (warning who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-warning)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)
              (make-marks-condition marks)))))
      #f)
    #f))

(define (assertion-violation who message . irritants)
  (define stk (shadow-stack))

  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-assertion-violation)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)
              (make-marks-condition (current-continuation-marks))))))
      (raise message))
    (raise who)))

(define (unwrap-syntax expr)
  (let loop ([lst expr])
    (cond
      [(pair? lst)
        (let ([a (loop (car lst))] [d (loop (cdr lst))])
          (cond
            [(and (eq? a (car lst)) (eq? d (cdr lst))) lst]
            [else (cons a d)]))]
      [(identifier? lst) lst]
      [(syntax? lst) (loop (syntax-expression lst))]
      [(vector? lst) (list->vector (map loop (vector->list lst)))]
      [else lst])))

(define (syntax-violation who message form . subform)
  (define (get-who-from-form form)
    (define obj (if (syntax? form) (unwrap-syntax form) form))
    (cond
      [(identifier? obj) (make-who-condition (syntax-expression obj))]
      [(and (pair? obj) (identifier? (car obj))) (make-who-condition (syntax-expression (car obj)))]
      [else #f]))

  (if (or (not who) (string? who) (symbol? who) (identifier? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-syntax-violation form (if (pair? subform) (car subform) #f))
              (if who
                (make-who-condition who)
                (get-who-from-form form))
              (make-message-condition message)))))
      (assertion-violation 'syntax-violation "expected string as message" message))
    (assertion-violation 'syntax-violation "expected string or symbol or #f as who" who)))

(define (error who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-error)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)
              (make-marks-condition marks)))))
      #f)
    #f))
(define (implementation-restriction-violation who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-implementation-restriction-violation)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)
              (make-marks-condition marks)))))
      #f)
    #f))

(define (undefined-violation who . message)
  (define marks (current-continuation-marks))
  (raise
    (apply
      condition
      (filter
        values
        (list
          (make-undefined-violation)
          (make-marks-condition marks)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))))

(define (%make-undefined-violation who . message)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (apply
      condition
      (filter
        values
        (list
          (make-undefined-violation)
          (make-marks-condition marks)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))
    #f))

(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-filename-error filename)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))
            (make-marks-condition marks)))))))

(define raise-i/o-error
  (lambda (who message . irritants)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-error)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))
            (make-marks-condition marks)))))))

(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (apply constructor options)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and port (make-i/o-port-error port))
            (make-irritants-condition (cons* port options))
            (make-marks-condition marks)))))))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (apply constructor options)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? options) (make-irritants-condition options))
            (make-marks-condition marks)))))))

(define raise-i/o-read-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-read-error who message port)))

(define raise-i/o-write-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-write-error who message port)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-file-is-read-only-error who message port)))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise-misc-i/o-error-with-port make-i/o-invalid-position-error who message port position)))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))
(define %make-io-error
  (lambda (who message . irritants)
    (define marks (current-continuation-marks))
    (if (or (not who) (string? who) (symbol? who))
      (if (string? message)
        (apply
          condition
          (filter
            values
            (list
              (make-i/o-error)
              (and who (make-who-condition who))
              (make-message-condition message)
              (and (pair? irritants) (make-irritants-condition irritants))
              (make-marks-condition marks))))
        #f)
      #f)))

(define (%make-assertion-violation who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-assertion-violation)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
      #f)
    #f))

(define (%make-error who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-error)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
      #f)
    #f))

(define (%make-implementation-restriction-violation who message . irritants)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-implementation-restriction-violation)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
      #f)
    #f))

(define (lexical-violation who . message)
  (raise
    (apply
      condition
      (filter
        values
        (list
          (make-lexical-violation)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))))

(define %make-lexical-violation
  (lambda (who . message)
    (if (or (not who) (string? who) (symbol? who))
      (apply
        condition
        (filter
          values
          (list
            (make-lexical-violation)
            (and who (make-who-condition who))
            (and (pair? message) (make-message-condition (car message))))))
      #f)))
