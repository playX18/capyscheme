;; Collection of utilities for parallel computing
#!r6rs 
(library (common par)
    (export parmap)
    (import (core threading) (srfi 1))



    (define (parmap n proc lst)
        "Parallel map using N threads over lst1 ... lstN"
        (define results (make-vector (length lst)))
       
        (define chunksize (ceiling (/ (length lst) n)))
        
        (define chunks 
          (let loop ((l lst) (acc '()))
            (if (null? l)
                (reverse acc)
                (loop (drop l chunksize)
                      (cons (take l chunksize) acc)))))

        (define (worker chunk start-index)
          (format #t "Worker starting for index ~a with chunk of size ~a~%" start-index (length chunk))
          (for-each 
            (lambda (item idx)
              (vector-set! results (+ start-index idx) (proc item)))
            chunk
            (iota (length chunk))))
        
        (define threads 
            (let loop ((chs chunks) (start 0) (ths '()))
                (if (null? chs)
                    ths 
                    (loop 
                        (cdr chs)
                        (+ start (length (car chs)))
                        (cons
                            (call-with-new-thread 
                                (lambda () (worker (car chs) start)))
                            ths)))))
        
        (for-each join-thread threads)
        (vector->list results)))