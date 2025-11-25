;; termios bindings 

(define-library (libc termios)
    (import 
        (core foreign)
        (core foreign-library)
        (scheme base)
        (capy))
        
    (cond-expand 
        [macos (include "termios-macos.scm")]
        [else  (include "termios-linux.scm")]))