(library (core sorting)

  (export list-sort
    vector-sort
    vector-sort!)

  (import (core primitives))

  (define vector-sort
    (lambda (proc vect)
      (let ((lst (vector->list vect)))
        (let ((lst2 (list-sort proc lst)))
          (cond ((eq? lst lst2) vect)
            (else
              (list->vector lst2)))))))

  (define vector-sort!
    (lambda (proc vect)
      (let* ((n (vector-length vect)) (work (make-vector (+ (div n 2) 1))))

        (define simple-sort!
          (lambda (first last)
            (let loop1 ((i first))
              (cond ((< i last)
                     (let ((m (vector-ref vect i)) (k i))
                       (let loop2 ((j (+ i 1)))
                         (cond ((<= j last)
                                (if (proc (vector-ref vect j) m)
                                  (begin
                                    (set! m (vector-ref vect j))
                                    (set! k j)))
                                (loop2 (+ j 1)))
                           (else
                             (vector-set! vect k (vector-ref vect i))
                             (vector-set! vect i m)
                             (loop1 (+ i 1)))))))))))

        (define sort!
          (lambda (first last)
            (cond ((> (- last first) 10)
                   (let ((middle (div (+ first last) 2)))
                     (sort! first middle)
                     (sort! (+ middle 1) last)
                     (let loop ((i first) (p2size 0))
                       (cond ((> i middle)
                              (let loop ((p1 (+ middle 1)) (p2 0) (p3 first))
                                (cond ((and (<= p1 last) (< p2 p2size))
                                       (cond ((proc (vector-ref work p2) (vector-ref vect p1))
                                              (vector-set! vect p3 (vector-ref work p2))
                                              (loop p1 (+ p2 1) (+ p3 1)))
                                         (else
                                           (vector-set! vect p3 (vector-ref vect p1))
                                           (loop (+ p1 1) p2 (+ p3 1)))))
                                  (else
                                    (let loop ((s2 p2) (d3 p3))
                                      (cond ((< s2 p2size)
                                             (vector-set! vect d3 (vector-ref work s2))
                                             (loop (+ s2 1) (+ d3 1)))))))))
                         (else
                           (vector-set! work p2size (vector-ref vect i))
                           (loop (+ i 1) (+ p2size 1)))))))
              (else
                (simple-sort! first last)))))

        (sort! 0 (- n 1)))))) ;[end]
