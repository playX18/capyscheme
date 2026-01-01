(library (core log)
    (export 
        *raw-log*
        *raw-log/src*
        log:none
        log:error
        log:warn
        log:info
        log:debug
        log:trace

        log:max-level
        log:set-max-level!
        log:max
        log:set-logger
        *simple-logger*
        log:)
    (import (core primitives))

(define-syntax log:
  (lambda (stx)
    (define (convert-log-level level)

      (syntax-case level (error warn info debug trace)
        [error (datum->syntax stx log:error)]
        [warn (datum->syntax stx log:warn)]
        [info (datum->syntax stx log:info)]
        [debug (datum->syntax stx log:debug)]
        [trace (datum->syntax stx log:trace)]
        [_ (syntax-violation 'log "invalid log level" level)]))

    (define (get-filename x)
      (let ((src (syntax-source x)))
        (cond 
          ((vector? src) (vector-ref src 0))
          ((and src (assq-ref src 'filename)) (assq-ref src 'filename))
          (else #f)
        )))
    (define (get-line x)
      (let ((src (syntax-source x)))
        (cond 
          ((vector? src) (vector-ref src 1))
          ((and src (assq-ref src 'line)) (assq-ref src 'line))
          (else #f)
        )))

    (define (current-module-name-quote)
        (datum->syntax stx (module-name (current-module))))

    (syntax-case stx (target error warn info debug trace) 
      [(_ (target trgt) (module mod) lvl arg arg* ...)
        #`(*raw-log/src* 
           #,(convert-log-level #'lvl)
           'trgt
           'mod
           #,(get-filename #'lvl)
           #,(get-line #'lvl)
           arg 
           arg* ...)]

      [(_ (target trgt) level arg arg* ...)
         #`(*raw-log/src* 
           #,(convert-log-level #'level)
           'trgt
           '#,(current-module-name-quote)
           #,(get-filename #'lvl)
           #,(get-line #'lvl)
           arg 
           arg* ...)]
      
      [(_ level arg arg* ...)
        #`(*raw-log/src* 
            #,(convert-log-level #'level)
            #f
            '#,(current-module-name-quote)
            #,(get-filename #'level)
            #,(get-line #'level)
            arg arg* ...)]))))