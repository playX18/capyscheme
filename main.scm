(import (core threading))

(define t (call-with-new-thread 
    (lambda ()
        (format #t "Hello from new thread!~%")
        42)))


(define val (join-thread t))
(format #t "Joined thread returned value: ~a~%" val)