(define &condition
  (let* ([rtd (make-record-type-descriptor '&condition #f #f #f #f '#())]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&condition rtd rcd)))

(define (compound-condition-component obj) (tuple-ref obj 1))

(define condition
  (lambda components
    (tuple
      'type:condition
      (apply append
        (map (lambda (component) (simple-conditions component)) components)))))

(define (compound-condition? obj)
  (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:condition)))

(define (simple-condition? obj)
  (and (record? obj) (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj))))

(define (condition? obj)
  (or (simple-condition? obj) (compound-condition? obj)))

(define (simple-conditions c)
  (if (simple-condition? c)
    (list c)
    (if (compound-condition? c)
      (compound-condition-component c)
      #f)))

(define (condition-predicate rtd)
  (lambda (obj)
    (if (simple-condition? obj)
      (rtd-ancestor? rtd (record-rtd obj))
      (if (compound-condition? obj)
        (any1 (lambda (c) (rtd-ancestor? rtd (record-rtd c))) (compound-condition-component obj))
        #f))))

(define (condition-accessor rtd proc)
  (define wrong-type
    (lambda (rtd obj)
      (assertion-violation
        "condition accessor"
        (format #f "expected condition of a subtype of ~a, but got ~a" rtd obj)
        rtd
        obj)))

  (or (rtd-ancestor? (record-type-rtd &condition) rtd)
    (assertion-violation
      'condition-accessor
      (format #f "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd)
      rtd
      proc))
  (lambda (obj)

    (if (simple-condition? obj)
      (begin
        (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj))
        (proc obj))
      (if (compound-condition? obj)
        (let ([res (any1 (lambda (c) (and (rtd-ancestor? rtd (record-rtd c)) c)) (compound-condition-component obj))])
          (if res (proc res) (wrong-type rtd obj)))
        (wrong-type rtd obj)))))

(define &message
  (let ([rtd (make-record-type-descriptor '&message (record-type-rtd &condition) #f #f #f '#((immutable message)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&message rtd rcd))))

(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))

(define &source
  (let ([rtd (make-record-type-descriptor '&source (record-type-rtd &condition) #f #f #f '#((immutable file) (immutable line) (immutable column)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&source rtd rcd))))

(define make-source-condition (record-constructor (record-type-rcd &source)))
(define source-condition? (condition-predicate (record-type-rtd &source)))
(define condition-source-file (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 0)))
(define condition-source-line (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 1)))
(define condition-source-column (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 2)))

(define (condition-sourcev exn)
  (define (valid-sourcev? src)
    (and (vector? src)
      (>= (vector-length src) 3)
      (vector-ref src 1)
      (vector-ref src 2)))
  (define (syntax-sourcev* obj)
    (and (syntax? obj)
      (let ((src (syntax-sourcev obj)))
        (and (valid-sourcev? src) src))))
  (cond
    [(and (condition? exn) (source-condition? exn))
      (vector (condition-source-file exn)
        (condition-source-line exn)
        (condition-source-column exn))]
    [(and (condition? exn) (syntax-violation? exn))
      (or (syntax-sourcev* (syntax-violation-subform exn))
        (syntax-sourcev* (syntax-violation-form exn))
        #f)]
    [else #f]))

;; Convert a source vector ([file line col ...]) to an &source condition, or #f.
(define (sourcev->source-condition src)
  (if (and (vector? src) (>= (vector-length src) 3))
    (make-source-condition (vector-ref src 0) (vector-ref src 1) (vector-ref src 2))
    #f))

(define (make-condition-uid) #f)

;; Render the source line of FILE at LINE (1-based) with a caret under column
;; COL (0-based), extended to END-COL when the span lies on the same line.
;; For a span that continues past LINE, the caret covers the rest of LINE and
;; the last line of the span is rendered underneath. Prints to port P; returns
;; #t when a line was printed, #f otherwise.
(define (render-source-line p file line col end-line end-col)
  (define (ref src index default)
    (if (and (vector? src) (< index (vector-length src)))
      (vector-ref src index)
      default))
  (define (caret-run start end len)
    (display (make-string (min start len) #\space) p)
    (display (make-string (max 1 (- (min end len) (min start len))) #\^) p)
    (newline p))
  (if (and (string? file) (exact-integer? line) (exact-integer? col)
       (>= line 1) (>= col 0))
    (if (file-exists? file)
      (call-with-input-file
        file
        (lambda (in)
          (let skip ([n (- line 1)])
            (if (> n 0)
              (if (eof-object? (get-line in))
                #f
                (skip (- n 1)))
              (let ([text (get-line in)])
                (if (eof-object? text)
                  #f
                  (let* ([len (string-length text)]
                         [caret-start (min col len)]
                         [same-line? (and (exact-integer? end-line)
                                      (= end-line line)
                                      (exact-integer? end-col)
                                      (> end-col caret-start))]
                         [multi-line? (and (exact-integer? end-line)
                                       (> end-line line))]
                         [caret-end (cond
                                      [same-line? (min end-col len)]
                                      [multi-line? len]
                                      [else (+ caret-start 1)])])
                    (display text p)
                    (newline p)
                    (caret-run caret-start caret-end len)
                    (if multi-line?
                      ;; render the last line of the span too
                      (let skip-end ([n (- end-line line 1)])
                        (if (> n 0)
                          (if (eof-object? (get-line in))
                            #t
                            (skip-end (- n 1)))
                          (let ([last (get-line in)])
                            (if (eof-object? last)
                              #t
                              (let ([last-len (string-length last)])
                                (display last p)
                                (newline p)
                                (caret-run 0
                                  (if (exact-integer? end-col)
                                    (min end-col last-len)
                                    last-len)
                                  last-len)
                                #t)))))
                      #t))))))))
      #f)
    #f))


;; taken from loko: https://gitlab.com/weinholt/loko/-/blob/master/runtime/control.sls#L296
;; When set to #f, print-condition prints only the summary line and source
;; location; the full component dump is available on demand.
(define print-condition-verbose? (make-parameter #t))

(define (print-condition exn p)
  (define (print-summary)
    (when (condition? exn)
      (display (if (warning? exn) "warning" "error") p)
      (when (who-condition? exn)
        (format p ": ~a" (condition-who exn)))
      (when (message-condition? exn)
        (format p ": ~a" (condition-message exn)))
      (when (irritants-condition? exn)
        (format p ": ~s" (condition-irritants exn)))
      (newline p)))
  (define (sourcev-ref src index default)
    (if (and (vector? src) (< index (vector-length src)))
      (vector-ref src index)
      default))
  (define (fmt-source src)
    (define file (sourcev-ref src 0 #f))
    (define line (sourcev-ref src 1 #f))
    (define col (sourcev-ref src 2 #f))
    (define end-line (sourcev-ref src 3 #f))
    (define end-col (sourcev-ref src 4 #f))
    (cond
      [(and file line col end-line end-col)
        (format p "~a:~a:~a-~a:~a" file line col end-line end-col)]
      [(and file line col)
        (format p "~a:~a:~a" file line col)]
      [else (write src p)]))
  (define (alist-ref alist key)
    (let ((entry (and (pair? alist) (assq key alist))))
      (and entry (cdr entry))))
  (define (print-datum p x)
    ;; Convert any datum that may embed syntax objects (a &syntax form can be
    ;; either a syntax object or a raw list of syntax objects, depending on
    ;; the caller) into a plain datum before printing.
    (define (convert x)
      (cond
        [(syntax? x) (syntax->datum x)]
        [(pair? x) (cons (convert (car x)) (convert (cdr x)))]
        [(vector? x) (list->vector (map convert (vector->list x)))]
        [else x]))
    (format p "~a" (convert x)))
  (define (print-expansion-frame frame index)
    (let ((name (alist-ref frame 'macro))
          (use-source (alist-ref frame 'use-site))
          (transformer-source (alist-ref frame 'transformer-site))
          (use-form (alist-ref frame 'use-form)))
      (format p "~%       ~a. " index)
      (if name
        (format p "while expanding ~a" name)
        (format p "while expanding macro"))
      (when use-source
        (format p " at ")
        (fmt-source use-source))
      (when use-form
        (format p "~%          in: ")
        (print-datum p use-form))
      (when transformer-source
        (format p "~%          transformer defined at ")
        (fmt-source transformer-source))))
  (define (print-expansion-trace frames)
    (if (pair? frames)
      (let loop ([frames frames] [index 1])
        (unless (null? frames)
          (print-expansion-frame (car frames) index)
          (loop (cdr frames) (+ index 1))))
      (write frames p)))
  (define (print-syntax form subform)
    (define form-src (if (syntax? form) (syntax-sourcev form) #f))
    (define subform-src (if (and subform (syntax? subform)) (syntax-sourcev subform) #f))
    (print-datum p form)
    (when form-src
      (format p "~%       in ")
      (fmt-source form-src))
    (when subform
      (print-datum p subform)
      (when subform-src
        (format p "~%     in ")
        (fmt-source subform-src))))
  ;; The expansion trace is (innermost ... outermost); the outermost frame is
  ;; the user's macro invocation. Its `use-site` source vector carries the
  ;; full span of the call, which is the most useful location to point at.
  (define (last-pair ls)
    (if (and (pair? ls) (pair? (cdr ls)))
      (last-pair (cdr ls))
      ls))
  (define (invocation-sourcev exn)
    (and (expansion-trace? exn)
      (let ((frames (condition-expansion-trace exn)))
        (and (pair? frames)
          (alist-ref (car (last-pair frames)) 'use-site)))))

  (cond
    [(condition? exn)
      (print-summary)
      (when (syntax-violation? exn)
        (let ((form (syntax-violation-form exn))
              (subform (syntax-violation-subform exn)))
          (when form
            (format p "in: ")
            (print-datum p form)
            (newline p))
          (when subform
            (format p "in: ")
            (print-datum p subform)
            (newline p))))
      (let ((src (or (invocation-sourcev exn) (condition-sourcev exn))))
        (when src
          (format p "At: ")
          (fmt-source src)
          (newline p)
          (render-source-line p
            (sourcev-ref src 0 #f)
            (sourcev-ref src 1 #f)
            (sourcev-ref src 2 #f)
            (sourcev-ref src 3 #f)
            (sourcev-ref src 4 #f))))
      (let ((trace (and (expansion-trace? exn) (condition-expansion-trace exn))))
        (when (pair? trace)
          (format p "Expansion trace:")
          (print-expansion-trace trace)
          (newline p)))
      (when (print-condition-verbose?)
        (let ([c* (simple-conditions exn)])
        (format p "The condition has ~a components:~%" (length c*))
        (do ([i 1 (+ 1 i)]
             [c* c* (cdr c*)])
          [(null? c*)]
          (let* ([c (car c*)]
                 [rtd (record-rtd c)])
            (format p " ~a. " i)
            (let ([supress-type
                    (and (eq? (record-type-parent rtd)
                          (record-type-rtd &condition))
                      (let ((name (symbol->string (record-type-name rtd)))
                            (fields (record-type-field-names rtd)))
                        (and (not (eqv? 0 (string-length name)))
                          (char=? (string-ref name 0) #\&)
                          (fx>? (vector-length fields) 0)
                          (string=? (substring name 1 (string-length name))
                            (symbol->string (vector-ref fields 0))))))])
              (if supress-type
                (put-char p #\&)
                (let loop ([rtd rtd])
                  (format p "~a" (record-type-name rtd))
                  (cond
                    [(record-type-parent rtd) =>
                      (lambda (parent)
                        (unless (eq? parent (record-type-rtd &condition))
                          (format p " ")
                          (loop parent)))])))
              (let loop ([rtd rtd])
                (do ([f* (record-type-field-names rtd)]
                     [i 0 (+ i 1)])
                  [(= i (vector-length f*))
                    (cond [(record-type-parent rtd) => loop])]
                  (unless (and supress-type (eqv? i 0))
                    (format p "~%     "))
                  (format p "~a: " (vector-ref f* i))
                  (let ([x ((record-accessor rtd i) c)])
                    (cond
                      [(and (eq? rtd (record-type-rtd &syntax)) (eqv? i 0))
                        (print-syntax (syntax-violation-form c) (syntax-violation-subform c))]
                      [(and (eq? rtd (record-type-rtd &syntax)) (eqv? i 1))
                        (values)]
                      [(and (eq? rtd (record-type-rtd &expansion-trace)) (eqv? i 0))
                        (print-expansion-trace x)]
                      [(and (eq? rtd (record-type-rtd &irritants))
                          (pair? x)
                          (list? x))
                        (display "(" p)
                        (write (car x) p)
                        (for-each
                          (lambda (x)
                            (display "\n                 " p)
                            (write x p))
                          (cdr x))
                        (display ")" p)]
                      [else (write x p)]))))))
          (newline p))))]
    [else
      (format p "A non-condition object was raised:~%~s" exn)]))
