(library (capy deque)
  (export
    make-deque
    deque
    deque-capacity
    deque-length
    deque-push-back!
    deque-push-front!
    deque-pop-back!
    deque-pop-front!
    deque-ref
    deque-set!
    deque->vector
    vector->deque
    list->deque
    deque-append!)
  (import (rnrs)
          (core struct)
          (only (scheme base) vector-copy)
          (only (scheme r5rs) modulo)
          (only (capy) register-tuple-printer! print))

  (define-struct %deque 
    (buf head len))

  (register-tuple-printer! 
    'type:%deque
    (lambda (obj port quote?)
      (display "#<deque " port)
      (let ((len (deque-length obj))
            (last (- (deque-length obj) 1)))
        (let loop ((i 0))
          (when (< i len)
            (print (deque-ref obj i) port quote?)
            (when (< i last)
              (display " " port))
            (loop (+ i 1)))))
      (display ">" port)))


  (define (make-deque capacity)
    (let ((initial-capacity (if (>= capacity 1)
                                capacity
                                8)))
      (make-%deque (make-vector initial-capacity) 0 0)))

  (define (deque . args)
    (let ((dq (make-deque (max 8 (length args)))))
      (for-each (lambda (v) (deque-push-back! dq v)) args)
      dq))

  (define (grow! dq)
    (let* ((old-buf (%deque-buf dq))
           (old-cap (vector-length old-buf))
           (new-cap (* 2 old-cap))
           (new-buf (make-vector new-cap)))
      ;; Copy elements linearly to the new buffer
      (do ((i 0 (+ i 1)))
          ((= i (deque-len dq)))
        (vector-set! new-buf i
                     (vector-ref old-buf
                                 (modulo (+ (deque-head dq) i) old-cap))))
      (deque-buf-set! dq new-buf)
      (deque-head-set! dq 0)))
  (define (deque-capacity dq)
    (vector-length (%deque-buf dq)))
  (define (deque-length dq)
    (%deque-len dq))

  (define (deque-push-back! dq value)
    (when (= (%deque-len dq) (deque-capacity dq))
      (grow! dq))
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (len (%deque-len dq))
           (cap (vector-length buf))
           (tail-index (modulo (+ head len) cap)))
      (vector-set! buf tail-index value)
      (%deque-len-set! dq (+ len 1))))
  
  (define (deque-push-front! dq value)
    (when (= (%deque-len dq) (deque-capacity dq))
      (grow! dq))
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (len (%deque-len dq))
           (cap (vector-length buf))
           (new-head (modulo (+ head (- cap 1)) cap)))
      (vector-set! buf new-head value)
      (%deque-head-set! dq new-head)
      (%deque-len-set! dq (+ len 1))))
  
  (define (deque-pop-back! dq)
    (unless (> (%deque-len dq) 0)
      (error 'deque-pop-back! "empty deque"))
    
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (len (%deque-len dq))
           (cap (vector-length buf))
           (tail-index (modulo (+ head (- len 1)) cap))
           (value (vector-ref buf tail-index)))
      (%deque-len-set! dq (- len 1))
      value))
  
  (define (deque-pop-front! dq)
    (unless (> (%deque-len dq) 0)
      (error 'deque-pop-front! "empty deque"))
    
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (len (%deque-len dq))
           (cap (vector-length buf))
           (value (vector-ref buf head))
           (new-head (modulo (+ head 1) cap)))
      (%deque-head-set! dq new-head)
      (%deque-len-set! dq (- len 1))
      value))
  
  (define (deque-ref dq index)
    (unless (and (>= index 0) (< index (%deque-len dq)))
      (error 'deque-ref "index out of bounds"))
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (cap (vector-length buf))
           (real-index (modulo (+ head index) cap)))
      (vector-ref buf real-index)))
  
  (define (deque-set! dq index value)
    (unless (and (>= index 0) (< index (%deque-len dq)))
      (error 'deque-set! "index out of bounds"))
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (cap (vector-length buf))
           (real-index (modulo (+ head index) cap)))
      (vector-set! buf real-index value)))
      
  (define (deque->vector dq)
    (let* ((buf (%deque-buf dq))
           (head (%deque-head dq))
           (len (%deque-len dq))
           (cap (vector-length buf))
           (result (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) result)
        (vector-set! result i
                     (vector-ref buf
                                 (modulo (+ head i) cap))))))

  (define (vector->deque vec)
    (let* ((len (vector-length vec))
           (dq (make-deque len)))
      (do ((i 0 (+ i 1)))
          ((= i len) dq)
        (deque-push-back! dq (vector-ref vec i)))))

  (define (list->deque lst)
    (let ((dq (make-deque (max 8 (length lst)))))
      (for-each (lambda (v) (deque-push-back! dq v)) lst)
      dq))

  (define (deque-append! dq1 dq2)
    (do ((i 0 (+ i 1)))
        ((= i (deque-length dq2)))
      (deque-push-back! dq1 (deque-ref dq2 i)))))

