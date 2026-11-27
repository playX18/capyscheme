;;; Diagnostics
;;;
;;; A diagnostic is a vector of values that display message produced by compiler during compilation:
;;;
;;;   #(#%diagnostic severity code message primary labels notes helps suggestions related)
;;;
;;;   severity     error | warning | note | hint
;;;   code         a symbol such as `unbound-variable` or `read/unclosed`
;;;   message      a string
;;;   primary      a span, or #f
;;;   labels       list of (span style text); style is `primary` or `secondary`
;;;   notes, helps lists of strings
;;;   suggestions  list of (message span replacement applicability); applicability is
;;;                `machine-applicable` or `maybe-incorrect`
;;;   related      list of diagnostics
;;;
;;; A span is #(file line col end-line end-col start-byte end-byte). Lines are 1-based, columns
;;; are 0-based code points and the byte offsets are 0-based UTF-8 offsets into the text (or #f
;;; when unknown). The end is exclusive.
;;;
;;; The text renderer is `(capy diagnostics render)` in pkgs/std.

(library (core diagnostics)
  (export make-span span? span-file span-line span-col span-end-line span-end-col
    span-start-byte span-end-byte
    sourcev->span
    make-label label-span label-style label-text
    make-suggestion suggestion-message suggestion-span suggestion-replacement
    suggestion-applicability
    make-diagnostic diagnostic?
    diagnostic-severity diagnostic-code diagnostic-message diagnostic-primary
    diagnostic-labels diagnostic-notes diagnostic-helps diagnostic-suggestions
    diagnostic-related
    diagnostic-with
    condition->diagnostic)
  (import (capy)
    (core conditions))

  ;; ---------------------------------------------------------------------------------------
  ;; Spans, labels, suggestions
  ;; ---------------------------------------------------------------------------------------

  (define (make-span file line col end-line end-col start-byte end-byte)
    (vector file line col end-line end-col start-byte end-byte))
  (define (span? x)
    (and (vector? x) (= (vector-length x) 7) (exact-integer? (vector-ref x 1))))
  (define (span-file s) (vector-ref s 0))
  (define (span-line s) (vector-ref s 1))
  (define (span-col s) (vector-ref s 2))
  (define (span-end-line s) (vector-ref s 3))
  (define (span-end-col s) (vector-ref s 4))
  (define (span-start-byte s) (vector-ref s 5))
  (define (span-end-byte s) (vector-ref s 6))

  (define (sourcev-slot sv i)
    (and (< i (vector-length sv)) (vector-ref sv i)))

  ;; The span of a source vector (see `syntax-sourcev`), or #f when SV carries no position.
  (define (sourcev->span sv)
    (and (vector? sv)
      (>= (vector-length sv) 3)
      (exact-integer? (vector-ref sv 1))
      (exact-integer? (vector-ref sv 2))
      (let* ([line (vector-ref sv 1)]
             [col (vector-ref sv 2)]
             [end-line (let ([l (sourcev-slot sv 3)]) (if (exact-integer? l) l line))]
             [end-col (let ([c (sourcev-slot sv 4)])
                        (if (exact-integer? c) c (+ col 1)))])
        (make-span (vector-ref sv 0) line col end-line end-col
          (let ([b (sourcev-slot sv 5)]) (and (exact-integer? b) b))
          (let ([b (sourcev-slot sv 6)]) (and (exact-integer? b) b))))))

  (define (make-label span style text) (list span style text))
  (define (label-span l) (car l))
  (define (label-style l) (cadr l))
  (define (label-text l) (caddr l))

  (define (make-suggestion message span replacement applicability)
    (list message span replacement applicability))
  (define (suggestion-message s) (car s))
  (define (suggestion-span s) (cadr s))
  (define (suggestion-replacement s) (caddr s))
  (define (suggestion-applicability s) (cadddr s))

  ;; ---------------------------------------------------------------------------------------
  ;; Diagnostics
  ;; ---------------------------------------------------------------------------------------

  ;; (make-diagnostic severity code message primary [labels notes helps suggestions related])
  (define (make-diagnostic severity code message primary . rest)
    (define (opt i) (if (> (length rest) i) (list-ref rest i) '()))
    (vector '|#%diagnostic| severity code message primary
      (opt 0) (opt 1) (opt 2) (opt 3) (opt 4)))

  (define (diagnostic? x)
    (and (vector? x) (= (vector-length x) 10) (eq? (vector-ref x 0) '|#%diagnostic|)))
  (define (diagnostic-severity d) (vector-ref d 1))
  (define (diagnostic-code d) (vector-ref d 2))
  (define (diagnostic-message d) (vector-ref d 3))
  (define (diagnostic-primary d) (vector-ref d 4))
  (define (diagnostic-labels d) (vector-ref d 5))
  (define (diagnostic-notes d) (vector-ref d 6))
  (define (diagnostic-helps d) (vector-ref d 7))
  (define (diagnostic-suggestions d) (vector-ref d 8))
  (define (diagnostic-related d) (vector-ref d 9))

  ;; A copy of D with the field NAME (severity, code, message, primary, labels, notes, helps,
  ;; suggestions or related) replaced; further NAME VALUE pairs may follow.
  (define (diagnostic-with d name value . more)
    (define i
      (case name
        [(severity) 1] [(code) 2] [(message) 3] [(primary) 4] [(labels) 5]
        [(notes) 6] [(helps) 7] [(suggestions) 8] [(related) 9]
        [else (error 'diagnostic-with "unknown field" name)]))
    (let ([v (vector-copy d)])
      (vector-set! v i value)
      (if (null? more)
        v
        (apply diagnostic-with v more))))

  ;; ---------------------------------------------------------------------------------------
  ;; Conditions
  ;; ---------------------------------------------------------------------------------------

  (define (->text x)
    (cond
      [(string? x) x]
      [(syntax? x) (format #f "~s" (syntax->datum x))]
      [else (format #f "~s" x)]))

  (define (sourcev-origin sv) (sourcev-slot sv 7))
  (define (sourcev-related sv) (or (sourcev-slot sv 8) '()))

  ;; Macro-introduced code carries the use site in its related spans. Returns (values sv
  ;; macro?): the source vector of the outermost use site, and whether SV was repointed.
  (define (repoint-macro sv)
    (let loop ([sv sv] [moved? #f])
      (if (and (vector? sv)
           (eq? (sourcev-origin sv) 'macro)
           (pair? (sourcev-related sv))
           (vector? (car (sourcev-related sv))))
        (loop (car (sourcev-related sv)) #t)
        (values sv moved?))))

  (define (syntax-sv x)
    (and (syntax? x)
      (let ([sv (syntax-sourcev x)])
        (and (vector? sv) (>= (vector-length sv) 3) (vector-ref sv 1) sv))))

  (define (span->string s)
    (format #f "~a:~a:~a" (or (span-file s) "<unknown>") (span-line s) (+ (span-col s) 1)))

  (define (frame-note frame)
    (let ([name (assq 'macro frame)]
          [use (and (assq 'use-site frame) (sourcev->span (cdr (assq 'use-site frame))))])
      (string-append "in this expansion of `"
        (if name (format #f "~a" (cdr name)) "macro")
        "`"
        (if use (string-append " (" (span->string use) ")") ""))))

  (define (who->code who)
    (cond
      [(symbol? who) who]
      [(string? who) (string->symbol who)]
      [(syntax? who) (let ([d (syntax->datum who)]) (and (symbol? d) d))]
      [else #f]))

  ;; Any raised object as a diagnostic. Conditions with `&diagnostic-info` keep its code,
  ;; labels, notes, helps and suggestions; for the others the code comes from `&who`, or from the
  ;; kind of condition.
  (define (condition->diagnostic c)
    (cond
      [(not (condition? c))
        (make-diagnostic 'error 'raise (format #f "uncaught object: ~s" c) #f)]
      [else (condition->diagnostic* c)]))

  (define (condition->diagnostic* c)
    (define info? (diagnostic-info-condition? c))
    (define syn? (syntax-violation? c))
    (define severity (if (and (warning? c) (not (serious-condition? c))) 'warning 'error))
    (define form-sv (and syn? (syntax-sv (syntax-violation-form c))))
    (define sub-sv (and syn? (syntax-sv (syntax-violation-subform c))))
    (define trace (and (expansion-trace? c) (condition-expansion-trace c)))
    (define source-sv
      (cond
        [(or sub-sv form-sv) #f]
        [(source-condition? c) (vector (condition-source-file c)
                                (condition-source-line c)
                                (condition-source-column c))]
        [else #f]))
    (define fallback-sv
      (and (not (or sub-sv form-sv source-sv))
        (pair? trace)
        (let ([use (assq 'use-site (car (last-pair trace)))])
          (and use (cdr use)))))
    (define primary-raw (or sub-sv form-sv source-sv fallback-sv))
    (define secondary-raw (and sub-sv form-sv (not (eq? sub-sv form-sv)) form-sv))
    (define notes '())
    (define (note! s) (set! notes (cons s notes)))
    (define-values (primary-sv repointed?)
      (if primary-raw (repoint-macro primary-raw) (values #f #f)))
    (define primary (and primary-sv (sourcev->span primary-sv)))
    (when repointed?
      (note! "the error originates in a macro expansion; the span is the macro use site"))
    (let ([secondary (and secondary-raw
                      (let-values ([(sv _) (repoint-macro secondary-raw)])
                        (sourcev->span sv)))])
      (when (pair? trace)
        (for-each (lambda (f) (note! (frame-note f))) trace))
      (let* ([code (or (and info? (condition-diagnostic-code c))
                    (and (who-condition? c) (who->code (condition-who c)))
                    (cond
                      [syn? 'syntax-error]
                      [(lexical-violation? c) 'read/lexical]
                      [(assertion-violation? c) 'assertion]
                      [(undefined-violation? c) 'undefined-variable]
                      [else 'error]))]
             [message (let ([m (if (message-condition? c)
                                 (->text (condition-message c))
                                 "error")])
                        (if (and (not syn?) (irritants-condition? c)
                             (pair? (condition-irritants c)))
                          (string-append m ": "
                            (let loop ([xs (condition-irritants c)] [out ""])
                              (if (null? xs)
                                out
                                (loop (cdr xs)
                                  (string-append out (if (string=? out "") "" " ")
                                    (format #f "~s" (let ([x (car xs)])
                                                      (if (syntax? x) (syntax->datum x) x))))))))
                          m))]
             [info-labels (if info? (condition-diagnostic-labels c) '())]
             [labels (append
                       (if primary (list (make-label primary 'primary "")) '())
                       (if secondary
                         (list (make-label secondary 'secondary "in this form"))
                         '())
                       info-labels)])
        (make-diagnostic severity code message primary labels
          (append (if info? (condition-diagnostic-notes c) '()) (reverse notes))
          (if info? (condition-diagnostic-helps c) '())
          (if info? (condition-diagnostic-suggestions c) '())
          '()))))

  (define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l))
  )
