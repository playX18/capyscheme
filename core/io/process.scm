;; Subprocess communication. Based ON ChezScheme.

(library (core io process)
    (export
        process
        open-process-ports
        system)
    (import 
        (core primitives) 
        (core control)
        (core foreign)
        (core foreign-library))

    (define system 
        (let ([fn (foreign-library-function 
            #f 
            "system"
            int 
            `(*))])
        (lambda (cmd)
            (unless (string? cmd)
                (error 'system "expected string for command" cmd))
            (let ([r (fn (bytevector->pointer (string->utf8/nul cmd)))])
                (when (< r 0)
                    (receive (err str) (errno/string)
                        (raise-i/o-error 'system (format #f "system call failed: ~a" str) err)))
                r))))

    (define (subprocess-port who what fd pid b-mode maybe-transcoder)
        (unless (buffer-mode? b-mode)
            (error 'subprocess-port "invalid buffer mode" b-mode))
        (let ([name (format #f "pid ~s ~a" pid what)])
            (let ([bp (if (eq? what 'stdin)
                (open-binary-fd-output-port name fd b-mode)
                (open-binary-fd-input-port name fd b-mode))])
                (if maybe-transcoder 
                    (transcoded-port bp maybe-transcoder)
                    bp ))))

    (define (process s)
        (unless (string? s)
            (error 'process "expected string for command" s))
        (apply 
            (lambda (ifd ofd pid)
                (list 
                    (subprocess-port 'process 'stdout ofd pid 'block (native-transcoder))
                    (subprocess-port 'process 'stdin ifd pid 'block (native-transcoder))
                    pid))
            (%process-spawn s #f)))

    (define open-process-ports 
        (case-lambda 
            [(s)
                (apply (lambda (ifd efd ofd pid)
                    (list 
                        (subprocess-port 'process 'stdout ofd pid 'block (native-transcoder))
                        (subprocess-port 'process 'stdin ifd pid 'block (native-transcoder))
                        (subprocess-port 'process 'stderr efd pid 'block (native-transcoder))
                        pid))
                    (%process-spawn s #t))]))
    
)