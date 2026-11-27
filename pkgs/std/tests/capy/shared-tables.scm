(import (rnrs)
        (srfi 64)
        (capy concurrency))

(test-begin "shared tables under parallel mutation")

(define (spawn n body)
  (let loop ([i 0] [acc '()])
    (if (= i n)
      (reverse acc)
      (loop (+ i 1) (cons (go (lambda () (body i))) acc)))))

(define (join-all threads) (for-each thread-join threads))

;; The symbol table: the same names interned from many threads give one symbol each, and
;; distinct names stay distinct.
(test-group "symbol interning"
  (let* ([threads 8] [names 2000]
         [name-of (lambda (k) (string-append "shared-sym-" (number->string k)))]
         [results (make-vector threads #f)])
    (join-all
      (spawn threads
        (lambda (t)
          (let ([row (make-vector names #f)])
            (do ([k 0 (+ k 1)]) ((= k names))
              (vector-set! row k (string->symbol (name-of k))))
            (vector-set! results t row)))))
    (test-assert "identical names are eq? across threads"
      (let check ([k 0])
        (or (= k names)
            (and (let ([first (vector-ref (vector-ref results 0) k)])
                   (let each ([t 1])
                     (or (= t threads)
                         (and (eq? first (vector-ref (vector-ref results t) k))
                              (each (+ t 1))))))
                 (check (+ k 1))))))
    (test-equal "names round-trip" (name-of 1234)
      (symbol->string (vector-ref (vector-ref results 3) 1234))))
  (let* ([threads 8] [each 1500]
         [out (make-vector threads '())])
    (join-all
      (spawn threads
        (lambda (t)
          (do ([k 0 (+ k 1)]) ((= k each))
            (vector-set! out t
              (cons (string->symbol (string-append "own-" (number->string t) "-" (number->string k)))
                    (vector-ref out t)))))))
    (test-equal "distinct names stay distinct"
      (* threads each)
      (length (let dedupe ([all (apply append (vector->list out))] [seen '()])
                (cond [(null? all) seen]
                      [(memq (car all) seen) (dedupe (cdr all) seen)]
                      [else (dedupe (cdr all) (cons (car all) seen))]))))))

;; A plain hashtable mutated by many threads is not synchronized: entries may be lost, but the
;; process must survive and the table must stay usable. The concurrent variant loses nothing.
(test-group "plain hashtable"
  (let ([table (make-eqv-hashtable)] [threads 8] [each 2000])
    (join-all
      (spawn threads
        (lambda (t)
          (do ([k 0 (+ k 1)]) ((= k each))
            (hashtable-set! table (+ (* t each) k) k)))))
    (test-assert "no more entries than were written" (<= (hashtable-size table) (* threads each)))
    (hashtable-set! table 'probe 1)
    (test-equal "still usable afterwards" 1 (hashtable-ref table 'probe #f))))

(test-group "concurrent hash table"
  (let ([table (make-concurrent-hash-table)] [threads 8] [each 1000])
    (join-all
      (spawn threads
        (lambda (t)
          (do ([k 0 (+ k 1)]) ((= k each))
            (concurrent-hash-table-set! table (+ (* t each) k) k)))))
    (test-equal "nothing is lost" (* threads each) (concurrent-hash-table-size table))))

(test-end "shared tables under parallel mutation")
