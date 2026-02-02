(library (capy binary-heap)
  (export make-heap
          make-heap-numeric
          heap?
          heap-push!
          heap-pop!
          heap-peek
          heap-size
          heap-empty?
          heap-for-each)
  (import (rnrs) (capy))

  ;; Define the record type for the heap
  (define-record-type heap
    (fields (mutable data) 
            (mutable size) 
            (immutable compare))
    (protocol
     (lambda (new)
       (lambda (compare initial-capacity)
         (new (make-vector initial-capacity) 0 compare)))))

  (define (make-heap-numeric . ascending?)
    "Create a numberic binary heap. By default, it's a min-heap."
    (let ((compare (if (or (null? ascending?) (car ascending?))
                       (lambda (a b) (< a b))
                       (lambda (a b) (> a b)))))
      (make-heap compare 16)))

  ;; Internal Helpers
  (define (parent i) (div (- i 1) 2))
  (define (left-child i) (+ (* 2 i) 1))
  (define (right-child i) (+ (* 2 i) 2))

  (define (vector-swap! vec i j)
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)))

  ;; Resizes the internal vector when capacity is reached.
  (define (heap-grow! h)
    (let* ((old-vec (heap-data h))
           (old-len (vector-length old-vec))
           (new-vec (make-vector (* old-len 2))))
      (vector-copy! old-vec 0 new-vec 0 old-len)
      (heap-data-set! h new-vec)))


  (define (heap-sift-up! h index)
    (let ((vec (heap-data h))
          (comp (heap-compare h))
          (p (parent index)))
      (when (and (> index 0)
                 (comp (vector-ref vec index) (vector-ref vec p)))
        (vector-swap! vec index p)
        (heap-sift-up! h p))))

  (define (heap-sift-down! h index)
    (let* ((vec (heap-data h))
           (size (heap-size h))
           (comp (heap-compare h))
           (l (left-child index))
           (r (right-child index))
           (target index))
      (let* ((target (if (and (< l size) 
                              (comp (vector-ref vec l) (vector-ref vec target)))
                         l target))
             (target (if (and (< r size) 
                              (comp (vector-ref vec r) (vector-ref vec target)))
                         r target)))
        (when (not (= target index))
          (vector-swap! vec index target)
          (heap-sift-down! h target)))))

  ;; Public API
  (define (heap-empty? h)
    (zero? (heap-size h)))

  (define (heap-push! h val)
    (let ((size (heap-size h))
          (vec (heap-data h)))
      (when (= size (vector-length vec))
        (heap-grow! h))
      (let ((current-vec (heap-data h))) ; Get updated vec if grown
        (vector-set! current-vec size val)
        (heap-size-set! h (+ size 1))
        (heap-sift-up! h size))))

  (define (heap-pop! h)
    (if (heap-empty? h)
        (assertion-violation 'heap-pop! "heap is empty")
        (let* ((vec (heap-data h))
               (top (vector-ref vec 0))
               (last-idx (- (heap-size h) 1)))
          (vector-set! vec 0 (vector-ref vec last-idx))
          (heap-size-set! h last-idx)
          (unless (zero? last-idx)
            (heap-sift-down! h 0))
          top)))

  (define (heap-peek h)
    (if (heap-empty? h)
        (assertion-violation 'heap-peek "heap is empty")
        (vector-ref (heap-data h) 0)))

  (define (heap-for-each h proc)
    (let ((vec (heap-data h))
          (size (heap-size h)))
      (let loop ((i 0))
        (when (< i size)
          (proc (vector-ref vec i))
          (loop (+ i 1)))))))