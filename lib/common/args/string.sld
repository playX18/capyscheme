(define-library (args string)
  (export string-prefix?
          string-index
          string-cursor-start
          string-cursor-end
          string-cursor=?
          string-cursor->index
          string-contains
          string-every
          string-for-each
          string-join
          string-pad-right)
  (import (except (scheme base) string-for-each))
  (begin
    (define (string-prefix? prefix s)
      (let ((prefix-len (string-length prefix))
            (s-len (string-length s)))
        (and (>= s-len prefix-len)
             (string=? prefix (substring s 0 prefix-len)))))

    (define (string-index s pred-or-char)
      (let ((pred (if (char? pred-or-char)
                    (lambda (ch) (char=? ch pred-or-char))
                    pred-or-char))
            (len (string-length s)))
        (let loop ((i 0))
          (cond
            ((= i len) len)
            ((pred (string-ref s i)) i)
            (else (loop (+ i 1)))))))

    (define (string-cursor-start s) 0)

    (define (string-cursor-end s) (string-length s))

    (define (string-cursor=? a b) (= a b))

    (define (string-cursor->index s cursor) cursor)

    (define (string-contains haystack needle)
      (let ((h-len (string-length haystack))
            (n-len (string-length needle)))
        (let loop ((i 0))
          (cond
            ((= n-len 0) 0)
            ((> (+ i n-len) h-len) #f)
            ((string=? (substring haystack i (+ i n-len)) needle) i)
            (else (loop (+ i 1)))))))

    (define (string-every pred s)
      (let ((len (string-length s)))
        (let loop ((i 0))
          (or (= i len)
              (and (pred (string-ref s i))
                   (loop (+ i 1)))))))

    (define (string-for-each proc s)
      (let ((len (string-length s)))
        (let loop ((i 0))
          (unless (= i len)
            (proc (string-ref s i))
            (loop (+ i 1))))))

    (define (string-join strings delimiter)
      (let loop ((items strings)
                 (out ""))
        (cond
          ((null? items) out)
          ((string=? out "") (loop (cdr items) (car items)))
          (else (loop (cdr items) (string-append out delimiter (car items)))))))

    (define (make-padding count)
      (let loop ((n count)
                 (out ""))
        (if (= n 0)
          out
          (loop (- n 1) (string-append out " ")))))

    (define (string-pad-right s len)
      (let ((s-len (string-length s)))
        (if (>= s-len len)
          s
          (string-append s (make-padding (- len s-len))))))))
