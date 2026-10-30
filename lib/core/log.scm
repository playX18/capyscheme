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
    log:set-logger!
    *simple-logger*
    log:)
  (import (core primitives)
    (only (rnrs syntax-case) with-syntax)
    (only (capy) assq-ref env-key expansion-env syntax-source))

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
            (else #f))))
      (define (get-line x)
        (let ((src (syntax-source x)))
          (cond
            ((vector? src) (vector-ref src 1))
            ((and src (assq-ref src 'line)) (assq-ref src 'line))
            (else #f))))

      (define (expansion-key-quote)
        (datum->syntax stx (env-key (expansion-env))))

      (syntax-case stx (target error warn info debug trace)
        [(_ (target trgt) (module mod) lvl arg arg* ...)
          (with-syntax ([level (convert-log-level #'lvl)]
                        [file (get-filename #'lvl)]
                        [line (get-line #'lvl)])
            #'(*raw-log/src* level 'mod 'trgt file line arg arg* ...))]

        [(_ (target trgt) level arg arg* ...)
          (with-syntax ([lvl (convert-log-level #'level)]
                        [mname (expansion-key-quote)]
                        [file (get-filename #'level)]
                        [line (get-line #'level)])
            #'(*raw-log/src* lvl 'mname 'trgt file line arg arg* ...))]

        [(_ level arg arg* ...)
          (with-syntax ([lvl (convert-log-level #'level)]
                        [mname (expansion-key-quote)]
                        [file (get-filename #'level)]
                        [line (get-line #'level)])
            #'(*raw-log/src* lvl #f 'mname file line arg arg* ...))]))))
