(library (capy pkg diag)
  (export parse-compiler-output
    read-diagnostic-file
    write-diagnostic-file!
    count-severity
    report-diagnostics!
    report-summary!)
  (import (rnrs)
    (only (capy) current-directory)
    (core diagnostics)
    (capy diagnostics json)
    (capy diagnostics render)
    (srfi 180)
    (capy pkg ui))

  (define (split-lines text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text))
          (reverse (if (> i start) (cons (substring text start i) out) out))]
        [(char=? (string-ref text i) #\newline)
          (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (join-lines lines)
    (let loop ([lines lines] [out ""])
      (if (null? lines)
        out
        (loop (cdr lines) (string-append out (if (string=? out "") "" "\n") (car lines))))))

  (define (parse-compiler-output text)
    (let loop ([lines (split-lines text)] [ds '()] [rest '()])
      (cond
        [(null? lines) (values (reverse ds) (join-lines (reverse rest)))]
        [(and (> (string-length (car lines)) 0) (char=? (string-ref (car lines) 0) #\{)
            (json-string->diagnostic (car lines)))
          => (lambda (d) (loop (cdr lines) (cons d ds) rest))]
        [else (loop (cdr lines) ds (cons (car lines) rest))])))

  (define (read-diagnostic-file path)
    (if (file-exists? path)
      (guard (e [#t '()])
        (let-values ([(ds rest) (parse-compiler-output (call-with-input-file path get-string-all))])
          ds))
      '()))

  (define (write-diagnostic-file! path ds)
    (cond
      [(pair? ds)
        (call-with-output-file path
          (lambda (port)
            (for-each
              (lambda (d)
                (put-string port (diagnostic->json-string d))
                (newline port))
              ds)))]
      [(file-exists? path) (delete-file path)]))

  (define (count-severity ds severity)
    (let loop ([ds ds] [n 0])
      (cond
        [(null? ds) n]
        [(eq? (diagnostic-severity (car ds)) severity) (loop (cdr ds) (+ n 1))]
        [else (loop (cdr ds) n)])))


  (define (string-prefix? prefix s)
    (and (>= (string-length s) (string-length prefix))
      (string=? (substring s 0 (string-length prefix)) prefix)))

  (define (relative-file file)
    (let* ([cwd (current-directory)]
           [dir (if (char=? (string-ref cwd (- (string-length cwd) 1)) #\/)
                 cwd
                 (string-append cwd "/"))])
      (if (and (string? file) (string-prefix? dir file))
        (substring file (string-length dir) (string-length file))
        file)))

  (define (map-span s proc)
    (and s
      (let ([v (list->vector (vector->list s))])
        (vector-set! v 0 (proc (vector-ref v 0)))
        v)))

  (define (map-files d proc)
    (diagnostic-with d
      'primary (map-span (diagnostic-primary d) proc)
      'labels (map (lambda (l)
                    (make-label (map-span (label-span l) proc) (label-style l) (label-text l)))
               (diagnostic-labels d))
      'suggestions (map (lambda (s)
                         (make-suggestion (suggestion-message s) (map-span (suggestion-span s) proc)
                           (suggestion-replacement s) (suggestion-applicability s)))
                    (diagnostic-suggestions d))
      'related (map (lambda (r) (map-files r proc)) (diagnostic-related d))))

  (define (report-diagnostics! ui ds)
    (cond
      [(eq? (ui-message-format ui) 'json)
        (let ([port (current-output-port)])
          (for-each
            (lambda (d)
              (put-string port (diagnostic->json-string d))
              (newline port))
            ds)
          (flush-output-port port))]
      [else
        (let ([port (ui-port ui)])
          (for-each
            (lambda (d) (render-diagnostic (map-files d relative-file) (ui-painter ui) port))
            ds)
          (flush-output-port port))]))

  (define (report-summary! ui package errors warnings)
    (when (eq? (ui-message-format ui) 'json)
      (let ([port (current-output-port)])
        (json-write
          (list (cons 'kind "summary")
            (cons 'package package)
            (cons 'errors errors)
            (cons 'warnings warnings)
            (cons 'success (= errors 0)))
          port)
        (newline port)
        (flush-output-port port)))
    (render-diagnostics-summary (string-append "`" package "`") errors warnings
      (ui-painter ui) (ui-port ui))
    (flush-output-port (ui-port ui))))
