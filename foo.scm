(import (scheme base)) 
(define self (dlopen #f 0))
(define sym-puts (dlsym self "puts"))
(define puts (pointer->procedure 0 sym-puts '(*)))

(define bv (bytevector-append (string->utf8 "hello, world!") #vu8(0)))
(define ptr (bytevector->pointer bv))
(puts ptr)