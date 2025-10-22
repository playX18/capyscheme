;; termios bindings 

(define-library (libc termios)
    (import 
        (core foreign)
        (core foreign-library)
        (scheme base))

    (cond-expand 
        [macos (include "termios-macos.scm")]
        [else  (include "termios-linux.scm")]))