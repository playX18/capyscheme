(library (core foreign-library)
    (export 
        load-foreign-library
        foreign-library?
        foreign-library-pointer
        foreign-library-function
        foreign-library-handle
        ->foreign-library)
    (import (core primitives)
            (core foreign)
            (core records)
            (core optargs)
            (core files)
            (core match)
            (capy))

    (define-record-type 
        (<foreign-library> make-foreign-library foreign-library?)
        (fields 
            filename
            (mutable handle 
                     foreign-library-handle 
                     set-foreign-library-handle!)))

    (define system-library-extensions (list (dll-suffix)))

    (define (has-extension? head exts)
        (match exts 
            (() #f)
            ((ext . exts)
                (or (string-contains head ext)
                    (has-extension? head exts)))))

    (define (is-integer-string? str)
        (and
            (not (string-null? str))
            (let ((n (string-length str)))
                (let lp ((i 0))
                    
                    (let ((c (string-ref str i)))
                        (if (or (char<? c #\0) (char>? c #\9))
                            #f
                            (if (= i (- n 1))
                                #t
                                (lp (+ i 1)))))))))
    (define (file-exists-with-extension head extensions)
        (if (has-extension? head extensions)
            (and (file-exists? head) head)
            (let lp ((exts extensions))
                (match exts 
                    (() #f)
                    ((ext . exts)
                        (let ((head (string-append head ext)))
                            (if (file-exists? head)
                                head 
                                (lp exts))))))))
    
    (define (file-exists-in-path-with-extension basename path exts)
        (match path 
            (() #f)
            ((dir . path)
                (or (file-exists-with-extension (in-vicinity dir basename) exts)
                    (file-exists-in-path-with-extension basename path exts)))))
    
    (define path-separator #\:)

    (define (parse-path var)
        (define e (getenv var))

        (cond 
            [(not e) #f]
            [(= (string-length e) 0) '()]
            [else  (string-split e path-separator)]))

    (define system-dynlib-path (make-parameter (or (parse-path "DYLD_LIBRARY_PATH")
                                                    (parse-path "LD_LIBRARY_PATH")
                                                    (parse-path "PATH")
                                                    '())))
    (define (default-search-path)
        '("./"))

    (define (load-foreign-library . opts)
        
        (let-optionals opts ((filename #f) 
                             (extensions system-library-extensions)
                             (search-path (default-search-path))
                             (search-system-paths? #t)
                             (lazy? #t)
                             (global? #f))
            (define (error-not-found)
                (error 'load-foreign-library 
                        (format #f "Could not find foreign library '~a'" filename)))
            (define flags
                (logior (if lazy? RTLD_LAZY RTLD_NOW)
                        (if global? RTLD_GLOBAL RTLD_LOCAL)))
            (define (dlopen* name)
                (dlopen name flags))
            (make-foreign-library 
                filename 
                (cond
                    ((not filename)
                        (dlopen* #f))
                    ((or (absolute-file-name? filename)
                         (string-any file-name-separator? filename))
                        (cond 
                            ((file-exists-with-extension filename extensions)
                                => dlopen*)
                            (else (error-not-found))))
                    ((file-exists? filename) => dlopen*)
                    ((file-exists-in-path-with-extension filename search-path extensions)
                        => dlopen*)
                    (search-system-paths? 
                        (if (or (null? extensions) (has-extension? filename extensions))
                            (dlopen* filename)
                            (let lp ((extensions extensions))
                                (match extensions 
                                    ((extension)
                                        (dlopen* (string-append filename extension)))
                                    ((extension . extensions)
                                        (or (with-exception-handler 
                                                (lambda (exn) #f)
                                                (lambda () (dlopen* (string-append filename extension))))
                                            (lp extensions)))))))
                    (else (error-not-found))))))   
    (define (->foreign-library lib)
        (if (foreign-library? lib)
            lib 
            (load-foreign-library lib)))

    (define (foreign-library-pointer lib name)
        (let ([handle (foreign-library-handle (->foreign-library lib))])
            (dlsym handle name)))
    (define (foreign-library-function lib name . args) 
        (let-optionals args 
            ([return-type void]
             [arg-types '()]
             [variadic? #f])
            (let ([ptr (foreign-library-pointer lib name)])
                (pointer->procedure return-type ptr arg-types variadic?)))))
