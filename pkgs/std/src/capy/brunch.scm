;; Vendored from playX18/brunch-scm, adapted from (brunch ...) to
;; (capy brunch ...). Original: https://github.com/playX18/brunch-scm
;; Otherwise verbatim; do not restyle.
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

(define-library (capy brunch)
  (import (capy brunch bench) 
          (scheme base)
          (scheme file))
  (export benches bench bench-spacer)
    
(begin 
  (define-syntax benches 
    (syntax-rules () 
      ((_ (bname filename) bench ...)
        (define (bname)
          (define bs (make-benches))
          (define port (current-error-port))
          (write-string "\x1b;[1;38;5;199mStarting:\x1b;[0m Running benchmark(s). Stand by!\n\n" port)
          (flush-output-port port)
          
          (begin 
            (write-string "\x1b;[1;34M•\x1b;[0m" port)
            (flush-output-port port)
            
            (set! bs (benches-add bs bench))) ...
          (write-string "\n\n" port)
          (benches-finish bs)
          (if (file-exists? filename)
            (delete-file filename))
          (call-with-output-file filename
            (lambda (file)
              (benches-finish-csv bs file)))))
      ((_ bname bench ...)
        (define (bname)
          (define bs (make-benches))
          (define port (current-error-port))
          (write-string "\x1b;[1;38;5;199mStarting:\x1b;[0m Running benchmark(s). Stand by!\n\n" port)
          (flush-output-port port)
          
          (begin 
            (write-string "\x1b;[1;34M•\x1b;[0m" port)
            (flush-output-port port)
            
            (set! bs (benches-add bs bench))) ...
          (write-string "\n\n" port)
          (benches-finish bs)))))

  
  (define-syntax bench-aux 
    (syntax-rules (samples timeout begin)
      ((_ (name s t b) (samples v) rest ...)
        (bench-aux (name v t b) rest ...))
      ((_ (name s t b) (timeout v) rest ...)
        (bench-aux (name s v b) rest ...))
      ((_ (name s t b) (begin . body) rest ...)
        (bench-aux (name s t body) rest ...))
      ((_ (name s t b))
        (bench-run (%make-bench name s t #f) (lambda () . b)))))
  (define-syntax bench
    (syntax-rules ()
      ((_ name . rest)
        (bench-aux (name (default-samples) (default-timeout) #f) . rest))))
))