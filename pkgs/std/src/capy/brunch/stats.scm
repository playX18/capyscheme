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

(define-library (capy brunch stats)
  (import (scheme base) (capy brunch math))
  (export make-stats stats-nice-mean stats-samples stats-mean)

(begin 
  (define-record-type <stats> 
    (%make-stats total valid deviation mean)
    stats?
    (total stats-total set-stats-total!)
    (valid stats-valid set-stats-valid!)
    (deviation stats-deviation set-stats-deviation!)
    (mean stats-mean set-stats-mean!))

  (define (make-stats times)
    (define total (length times))
    (define calc (abacus-from-list times))
    ;(abacus-prune-outliers! calc)
    (define valid (abacus-len calc))
    (define mean (abacus-mean calc))
    (define deviation (abacus-deviation calc))

    (%make-stats total
                 valid
                 deviation
                 mean))

  (define (stats-deviant? stats other)
    (define lo (+ (* (stats-deviation stats) -2.0) (stats-mean stats)))
    (define hi (+ (* (stats-deviation stats) 2.0) (stats-mean stats)))
    #f)

  (define (round-n x n)
    (let ((factor (expt 10 n)))
      (/ (round (* x factor)) factor)))
  (define (stats-nice-mean stats)
    (let-values (((mean unit)
      (cond 
        ((< (stats-mean stats) 0.000001) (values (* (stats-mean stats) 1000000000.0) "ns"))
        ((< (stats-mean stats) 0.001) (values (* (stats-mean stats) 1000000.0) "µs"))
        ((< (stats-mean stats) 1.0) (values (* (stats-mean stats) 1000.0) "ms"))
        (else (values (stats-mean stats) "s")))))
      (define port (open-output-string))
      (write-string "\x1b;[01;m" port)
      (write-string (number->string (round-n mean 2)) port)
      (write-string " " port)
      (write-string unit port)
      (write-string "\x1b;[0m" port)
      (get-output-string port)))
  (define (stats-samples stats) 
    (values (stats-valid stats) (stats-total stats)))))