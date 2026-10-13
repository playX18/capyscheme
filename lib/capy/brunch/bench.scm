;; Vendored from playX18/brunch-scm
;;
;; BSD 3-Clause License
;;
;; Copyright (c) 2025, Adel Prokurov <adel.prokurov@gmail.com>
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-library (capy brunch bench)
  (import (capy brunch math)
          (capy brunch stats)
          (scheme base)
          (scheme time))
  (export make-bench
          bench-run
          bench-spacer? 
          bench-stats
          make-benches
          benches-add
          benches-finish
          benches-finish-csv
          bench-spacer
          %make-bench
          default-timeout
          default-samples
          )
(begin
  (define default-samples (make-parameter 100))
  (define default-timeout (make-parameter (* 60 5.0)))

  (define-record-type <bench>
    (%make-bench name samples timeout stats)
    bench?
    (name bench-name set-bench-name!)
    (samples bench-samples set-bench-samples!)
    (timeout bench-timeout set-bench-timeout!)
    (stats bench-stats set-bench-stats!))
  
  (define (make-bench name . samples?)
    (define samples (if (null? samples?)
                        (default-samples)
                        (car samples?)))
    (%make-bench name
                 samples
                 (default-timeout)
                 '()))
          
  (define bench-spacer (%make-bench "" 0 0.0 '()))
  (define (bench-spacer? b)
    (eq? b bench-spacer))

  (define (bench-run b proc)
    (cond 
      ((bench-spacer? b) b)
      (else 
        
        (let ((now (current-second))
              (samples (bench-samples b)))
          (let loop ((times '()) (i 0))
            (cond 
              ((< i samples)
                (let ((now2 (current-second))) 
                  (call-with-values proc 
                    (lambda res 
                      (define elapsed (- (current-second) now2))
                      (define total-elapsed (- (current-second) now))
                      (unless (> total-elapsed (bench-timeout b))
                        (loop (cons elapsed times) (+ i 1)))))))
              (else 
                (set-bench-stats! b (make-stats times))
                b)))))))
  
  (define-record-type <benches>
    (%make-benches benches)
    benches?
    (benches benches-benches set-benches-benches!))
  (define no-change "\x1b;[2m---\x1b;[0m")
  (define (make-benches)
    (%make-benches '()))

  (define (benches-add benches b)
    (%make-benches (cons b (benches-benches benches))))
  
  (define (benches-finish benches)
    (define port (current-error-port))

    (let loop ((summary (list #f
                          #("\x1b;[1;95mMethod" "Mean" "Samples\x1b;[0m" "\x1b;[1;95mChange\x1b;[0m")))
               (bs (benches-benches benches)))
      
      (cond 
        ((null? bs)
          (format-summary (reverse summary) port))
        (else 
          (let ((b (car bs)))
            (cond 
              ((bench-spacer? b)
                (loop (cons #f summary) (cdr bs)))
              (else 
               ; (define time (stats-nice-mean (bench-stats b)))
                (let-values (((time) (stats-nice-mean (bench-stats b)))
                             ((valid total) (stats-samples (bench-stats b))))
                  (define p (open-output-string))
                  (write-string "\x1b;[2m" p)
                  (write-string (number->string valid) p)
                  (write-string "\x1b;[0;35m" p)
                  (write-string "/" p)
                  (write-string "\x1b;[0;2m" p)
                  (write-string (number->string total) p)
                  (write-string "\x1b;[0m" p)
                  
                  (loop (cons (vector (bench-name b)
                                      time
                                      (get-output-string p)
                                      no-change)
                          summary)
                        (cdr bs))))))))))

  (define (benches-finish-csv benches file)
    (write-string "Method,Mean,Samples\n" file)
    (let loop ((bs (benches-benches benches)))
      (cond 
        ((null? bs) '())
        (else 
          (let ((b (car bs)))
            (cond 
              ((bench-spacer? b)  
                (loop (cdr bs)))
              (else 
                (let ((time (number->string (stats-mean (bench-stats b)))))
                  (let-values (((valid total) (stats-samples (bench-stats b))))
                    (write-string (bench-name b) file)
                    (write-string "," file)
                    (write-string time file)
                    (write-string "," file)
                    (write-string (number->string valid) file)
                    (write-string "/" file)
                    (write-string (number->string total) file)
                    ;(write-string "," file)
                    (if (not (null? (cdr bs))) (write-string "\n" file))
                    (loop (cdr bs)))))))))))

  (define (format-summary table port)
    (let-values (((w1 w2 w3 w4) (lens table)))
      (define width (+ w1 w2 w3 8))
      (define pad-len (max w1 w2 w3))
      (define pad (make-string pad-len #\space))
      (define spacer 
        (let ((spacer-p (open-output-string)))
          (write-string "\x1b;[35m" spacer-p)
          (let loop ((i 0))
            (when (< i width)
              (write-char #\- spacer-p)
              (loop (+ i 1))))
          (write-string "\x1b;[0m\n" spacer-p)
          (get-output-string spacer-p)))
      
      (set! w4 0)
      
    
      (let loop ((rows table))
        (cond 
          ((null? rows) 
            (flush-output-port port)
            '())
          ((not (car rows))
            (write-string spacer port)
            (write-string "\n" port)
            (loop (cdr rows)))
          (else 
            (let* ((row (car rows))
                   (v1 (vector-ref row 0))
                   (v2 (vector-ref row 1))
                   (v3 (vector-ref row 2))
                   (v4 (vector-ref row 3))
                   (c1 (get-width v1))
                   (c2 (get-width v2))
                   (c3 (get-width v3))
                   (c4 (get-width v4)))
              (write-string v1 port)
              (write-string (substring pad 0 (- w1 c1)) port)
              (write-string "    " port)
              (write-string (substring pad 0 (- w2 c2)) port)
              (write-string v2 port)

              (write-string "    " port)
              (write-string (substring pad 0 (- w3 c3)) port)
              (write-string v3 port)

              ;(write-string "    " port)
              ;(write-string (substring pad 0 (- w4 c4)) port)
              ;(write-string v4 port)
              (write-string "\n" port)
              (loop (cdr rows))))))))

  (define (lens table)
    (let loop ((w1 0) (w2 0) (w3 0) (w4 0) (rows table))
      
      (cond 
        ((null? rows)
          (values w1 w2 w3 w4))
        ((not (car rows))
          (loop w1 w2 w3 w4 (cdr rows)))
        (else 
          (let* ((row (car rows))
                 (v1 (get-width (vector-ref row 0)))
                 (v2 (get-width (vector-ref row 1)))
                 (v3 (get-width (vector-ref row 2)))
                 (v4 (get-width (vector-ref row 3))))
            (loop (max w1 v1)
                  (max w2 v2)
                  (max w3 v3)
                  (max w4 v4)
                  (cdr rows)))))))

  
  (define (get-width src)
    (define len (string-length src))
    (let loop ((ansi? #f) (i 0) (w 0))
      (cond 
        ((< i len)
          (let ((c (string-ref src i)))
            (cond 
              (ansi? 
                (loop (not (memq c '(#\m #\A #\K))) (+ i 1) w))
              ((char=? c #\x1b)
                (loop #t (+ i 1) w))
              (else (loop ansi? (+ i 1) (+ w 1))))))
        (else w))))))