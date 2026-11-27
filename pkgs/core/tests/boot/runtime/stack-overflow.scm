(import (srfi 64)
        (capy)
        (rnrs conditions)
        (rnrs exceptions))

(test-begin "stack overflow")

(define (deep n)
  (if (= n 0) 0 (+ 1 (deep (- n 1)))))

(define (runaway n) (+ 1 (runaway n)))

(test-group "deep but finite recursion"
  (test-equal "a million non-tail frames" 1000000 (deep 1000000)))

(test-group "runaway recursion"
  (test-assert "raises a condition that can be caught"
    (guard (e [(assertion-violation? e) #t])
      (runaway 0)
      #f))
  (test-assert "the message names the overflow"
    (guard (e [(message-condition? e)
                (and (string? (condition-message e))
                     (string=? (condition-message e) "stack overflow"))])
      (runaway 0)
      #f))
  (test-equal "the thread goes on afterwards" 100000 (deep 100000))
  (test-assert "and the limit applies again to a second descent"
    (guard (e [(assertion-violation? e) #t])
      (runaway 0)
      #f)))

(test-end "stack overflow")
