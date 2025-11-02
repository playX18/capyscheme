#!r6rs 
(library (crossterm tty)
    (export tty?
            enable-raw-mode
            disable-raw-mode
            with-raw-mode
            window-size)
    (import 
        (core foreign) 
        (core foreign-library) 
        (rnrs)
        (libc termios)
        (core threading))

(define isatty 
    (let ([fn (foreign-library-function 
        #f 
        "isatty"
        int 
        `(,int))])
        
    (lambda (port)
        (= (fn (port-fd port)) 1))))

(define (tty? port)
    (and (port? port)
         (isatty port)))


(define original-mode (make-parameter #f))
(define original-mode-lock (make-mutex))
(define (enable-raw-mode)
    (with-mutex original-mode-lock 
        (when (not (original-mode))
            (let* ([fd (port-fd (current-input-port))]
                   [t (tcgetattr fd)])
                (original-mode t)
                (let ([raw (cfmakeraw t)])
                    (tcsetattr fd TCSANOW raw))))))

(define (disable-raw-mode)
    (with-mutex original-mode-lock
        (cond 
            [(original-mode) 
                => (lambda (t)
                    (define fd (port-fd (current-input-port)))
                    (tcsetattr fd TCSANOW t)
                    #t)]
            [else #f])))

(define-syntax with-raw-mode 
    (syntax-rules () 
        ((_ body ...)
            (dynamic-wind 
                enable-raw-mode 
                (lambda () body ...)
                disable-raw-mode))))

(define (window-size)
    (let* ([fd (port-fd (current-input-port))]
           [ws (get-winsize fd)])
        (values (winsize-cols ws) (winsize-rows ws))))



)