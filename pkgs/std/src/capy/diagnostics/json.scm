;;; Diagnostics as JSON, the form the compiler hands them to the driver in
;;; (`capy-rt --compile --error-format json`) and `capy build --message-format json` prints them:
;;; one object per diagnostic, on one line.
;;;
;;;   {"kind":"diagnostic","severity":"error","code":"unbound-variable",
;;;    "message":"unbound variable `lenght`",
;;;    "primary":{"file":"src/app.scm","line":12,"column":5,"end_line":12,"end_column":11,
;;;               "start_byte":240,"end_byte":246},
;;;    "labels":[{"span":{...},"style":"primary","text":"not bound in this scope"}],
;;;    "notes":[],"helps":[],"suggestions":[],"related":[]}
;;;
;;; Lines and columns are 1-based (columns count code points), byte offsets are 0-based offsets
;;; into the UTF-8 text, and ends are exclusive. `json->diagnostic` is the inverse of
;;; `diagnostic->json`, so a diagnostic survives the trip through a pipe.

(library (capy diagnostics json)
  (export diagnostic->json
    diagnostic->json-string
    json->diagnostic
    json-string->diagnostic)
  (import (rnrs)
    (core diagnostics)
    (srfi 180))

  (define (nullable x) (if x x 'null))

  (define (span->json s)
    (if s
      (list
        (cons 'file (let ([f (span-file s)]) (if (string? f) f 'null)))
        (cons 'line (span-line s))
        (cons 'column (+ (span-col s) 1))
        (cons 'end_line (span-end-line s))
        (cons 'end_column (+ (span-end-col s) 1))
        (cons 'start_byte (nullable (span-start-byte s)))
        (cons 'end_byte (nullable (span-end-byte s))))
      'null))

  (define (symbol->json x) (if (symbol? x) (symbol->string x) 'null))

  (define (diagnostic->json d)
    (list
      (cons 'kind "diagnostic")
      (cons 'severity (symbol->json (diagnostic-severity d)))
      (cons 'code (symbol->json (diagnostic-code d)))
      (cons 'message (diagnostic-message d))
      (cons 'primary (span->json (diagnostic-primary d)))
      (cons 'labels
        (list->vector
          (map (lambda (l)
                (list (cons 'span (span->json (label-span l)))
                  (cons 'style (symbol->json (label-style l)))
                  (cons 'text (label-text l))))
            (diagnostic-labels d))))
      (cons 'notes (list->vector (diagnostic-notes d)))
      (cons 'helps (list->vector (diagnostic-helps d)))
      (cons 'suggestions
        (list->vector
          (map (lambda (s)
                (list (cons 'message (suggestion-message s))
                  (cons 'span (span->json (suggestion-span s)))
                  (cons 'replacement (suggestion-replacement s))
                  (cons 'applicability (symbol->json (suggestion-applicability s)))))
            (diagnostic-suggestions d))))
      (cons 'related (list->vector (map diagnostic->json (diagnostic-related d))))))

  (define (diagnostic->json-string d)
    (call-with-string-output-port
      (lambda (port) (json-write (diagnostic->json d) port))))

  ;; ---------------------------------------------------------------------------------------
  ;; Reading
  ;; ---------------------------------------------------------------------------------------

  (define (field obj name)
    (let ([p (assq name obj)])
      (and p (not (eq? (cdr p) 'null)) (cdr p))))

  (define (field-list obj name)
    (let ([v (field obj name)])
      (if (vector? v) (vector->list v) '())))

  (define (json->span x)
    (and (pair? x)
      (let ([line (field x 'line)] [column (field x 'column)])
        (and (exact-integer? line) (exact-integer? column)
          (make-span (field x 'file)
            line (- column 1)
            (or (field x 'end_line) line)
            (- (or (field x 'end_column) (+ column 1)) 1)
            (field x 'start_byte)
            (field x 'end_byte))))))

  (define (string->symbol* s) (and (string? s) (string->symbol s)))

  (define (json->diagnostic obj)
    (make-diagnostic
      (or (string->symbol* (field obj 'severity)) 'error)
      (string->symbol* (field obj 'code))
      (or (field obj 'message) "")
      (json->span (field obj 'primary))
      (map (lambda (l)
            (make-label (json->span (field l 'span))
              (or (string->symbol* (field l 'style)) 'primary)
              (or (field l 'text) "")))
        (field-list obj 'labels))
      (field-list obj 'notes)
      (field-list obj 'helps)
      (map (lambda (s)
            (make-suggestion (or (field s 'message) "")
              (json->span (field s 'span))
              (or (field s 'replacement) "")
              (or (string->symbol* (field s 'applicability)) 'maybe-incorrect)))
        (field-list obj 'suggestions))
      (map json->diagnostic (field-list obj 'related))))

  ;; The diagnostic written in the line TEXT, or #f when TEXT is not a diagnostic object.
  (define (json-string->diagnostic text)
    (guard (e [#t #f])
      (let ([obj (call-with-port (open-string-input-port text) json-read)])
        (and (pair? obj)
          (equal? (field obj 'kind) "diagnostic")
          (json->diagnostic obj))))))
