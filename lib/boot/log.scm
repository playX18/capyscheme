;; Simple logging facility inspired by log crate
(define log:none  0)
(define log:error 1)
(define log:warn  2)
(define log:info  3)
(define log:debug 4)
(define log:trace 5)

(define log:max-level #f)
(define log:set-max-level! #f)
(define (log:max) log:trace)
(define log:set-logger! #f)
(define *raw-log* #f)
(define *raw-log/src* #f)
(define (*log-time* thunk level module target msg . args)
    (define start (current-second))
    (receive ans (thunk)
        (let ([end (current-second)])
            (*raw-log* level target module "~a (took ~a seconds)" (apply format #f msg args) (- end start))
            (apply values ans))))
(define *simple-logger* #f)

(let () 
    (define max-log-level-filter (make-parameter 0))
    (define logger (make-parameter (lambda (op arg)
        #f)))

    (define <log:metadata> 
        (let* ([rtd (make-record-type-descriptor '<log:metadata> #f #f #f #f '#(
            (immutable level)
            (immutable target)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '<log:metadata> rtd rcd)))
    (define make-log-metadata (record-constructor (record-type-rcd <log:metadata>)))
    (define log-metadata-level (record-accessor (record-type-rtd <log:metadata>) 0))
    (define log-metadata-target (record-accessor (record-type-rtd <log:metadata>) 1))
    (define log-metadata? (record-predicate (record-type-rtd <log:metadata>)))

    (define <log:record> 
        (let* ([rtd (make-record-type-descriptor '<log:record> #f #f #f #f '#(
            (immutable metadata)
            (immutable fmt)
            (immutable args)
            (immutable module)
            (immutable file)
            (immutable line)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '<log:record> rtd rcd)))
    (define make-log-record (record-constructor (record-type-rcd <log:record>)))
    (define log-record-metadata (record-accessor (record-type-rtd <log:record>) 0))
    (define log-record-fmt (record-accessor (record-type-rtd <log:record>) 1))
    (define log-record-args (record-accessor (record-type-rtd <log:record>) 2))
    (define log-record-module (record-accessor (record-type-rtd <log:record>) 3))
    (define log-record-file (record-accessor (record-type-rtd <log:record>) 4))
    (define log-record-line (record-accessor (record-type-rtd <log:record>) 5))
    (define log-record? (record-predicate (record-type-rtd <log:record>)))

    (set! log:set-logger!
        (lambda (f)
            (logger f)))
    
    (set! log:set-max-level!
        (lambda (level)
            (set! *log-level* level)
            (max-log-level-filter level)))
    (set! log:max-level (lambda () (max-log-level-filter)))

    (set! *raw-log/src* 
        (lambda (level module target file line fmt . args)
            (define metadata (make-log-metadata level target))
            (define record (make-log-record metadata fmt args module file line))
            (when (<= level (max-log-level-filter))
                ((logger) 'log record))))

    (set! *raw-log* 
        (lambda (level module target fmt . args)
            (define file #f)
            (define line #f)
            (define metadata (make-log-metadata level target))
            (define record (make-log-record metadata fmt args module file line))
            (when (<= level (max-log-level-filter))
                ((logger) 'log record))))

    (set! *simple-logger* 
        (lambda (op arg)
        (cond 
            [(eq? op 'log)
                (let* ([record arg]
                       [metadata (log-record-metadata record)]
                       [level (log-metadata-level metadata)]
                       [target (log-metadata-target metadata)]
                       [fmt (log-record-fmt record)]
                       [args (log-record-args record)]
                       [module (log-record-module record)]
                       [file (log-record-file record)]
                       [line (log-record-line record)])
                    (define level-str
                        (cond 
                            [(= level log:error) "ERROR"]
                            [(= level log:warn)  "WARN "]
                            [(= level log:info)  "INFO "]
                            [(= level log:debug) "DEBUG"]
                            [(= level log:trace) "TRACE"]
                            [else (format #f "LVL~a" level)]))
                    (format (current-output-port) ";; ~a" level-str)
                    (if target (format (current-output-port) " ~a" target))
                    (if module (format (current-output-port) "~a@~a" (if target "" " ") module))
                    (format (current-output-port) ": ")
                    (apply format (current-output-port) fmt args)
                    (newline (current-output-port))
                    (flush-output-port (current-output-port))
                    )]
            [(eq? op 'enabled) #t]
            [(eq? op 'flush) #f])))
)