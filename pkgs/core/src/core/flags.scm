;;; A small command line flag parser, for the entry points of the runtime (boot/cli.scm).
;;;
;;; A parser is a list of specs, made with `flag`, `option`, `multi-option` and `separator`:
;;;
;;;   (flag "r7rs" "Run in R7RS mode")                       --r7rs
;;;   (flag "help" "Show help" 'abbr "h")                    --help, -h
;;;   (option "script" "Run FILE" "FILE" 'abbr "s")          --script=FILE, --script FILE, -s FILE, -sFILE
;;;   (multi-option "load-path" "Add DIR" "DIR" 'abbr "L")   given any number of times, values may be
;;;                                                          comma separated
;;;
;;; Further attributes follow the positional arguments as 'name value pairs: `'abbr` (one
;;; character), `'aliases` (more long names), `'allowed` (the values an option takes) and
;;; `'hidden` (#t: left out of the usage text).
;;;
;;; `(parse-flags specs args)` reads ARGS: options may come anywhere, `--` ends them, and anything
;;; that is not an option is a positional argument. It returns a flags object; `flag-ref` gives
;;; the value of a flag (#t or #f), an option (a string or #f) or a multi-option (a list in the
;;; order given), `flags-rest` the positional arguments. A mistake is an error whose message
;;; says what is wrong.

(library (core flags)
  (export flag
    option
    multi-option
    separator
    parse-flags
    flag-ref
    flags-rest
    flags-usage)
  (import (core primitives)
    (core lists)
    (only (capy) format))

  (define (string-index str ch)
    (let loop ([i 0])
      (cond
        [(= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))

  ;; spec: (kind name help value-help attributes); a separator is (separator text)
  (define (flag name help . attributes) (list 'flag name help #f attributes))
  (define (option name help value-help . attributes) (list 'option name help value-help attributes))
  (define (multi-option name help value-help . attributes)
    (list 'multi name help value-help attributes))
  (define (separator text) (list 'separator text))

  (define (spec-kind spec) (car spec))
  (define (spec-name spec) (cadr spec))
  (define (spec-help spec) (caddr spec))
  (define (spec-value-help spec) (cadddr spec))
  (define (spec-attribute spec key)
    (let loop ([attributes (car (cddddr spec))])
      (cond
        [(or (null? attributes) (null? (cdr attributes))) #f]
        [(eq? (car attributes) key) (cadr attributes)]
        [else (loop (cddr attributes))])))
  (define (option-spec? spec) (not (eq? (spec-kind spec) 'separator)))
  (define (takes-value? spec) (memq (spec-kind spec) '(option multi)))

  (define (flags-error fmt . args)
    (error #f (apply format #f fmt args)))

  (define (find-long specs name)
    (find (lambda (spec)
           (and (option-spec? spec)
             (or (string=? (spec-name spec) name)
               (member name (or (spec-attribute spec 'aliases) '())))))
      specs))

  (define (find-abbr specs c)
    (find (lambda (spec)
           (and (option-spec? spec) (equal? (spec-attribute spec 'abbr) c)))
      specs))

  (define (check-allowed spec value shown)
    (let ([allowed (spec-attribute spec 'allowed)])
      (when (and allowed (not (member value allowed)))
        (flags-error "'~a' is not an allowed value for option '~a'." value shown))))

  (define (split-commas str)
    (let loop ([start 0] [i 0] [out '()])
      (cond
        [(= i (string-length str)) (reverse (cons (substring str start i) out))]
        [(char=? (string-ref str i) #\,) (loop (+ i 1) (+ i 1) (cons (substring str start i) out))]
        [else (loop start (+ i 1) out)])))

  (define (letter-or-digit? ch)
    (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))))

  (define (name-char? ch)
    (or (letter-or-digit? ch) (char=? ch #\-) (char=? ch #\_)))

  (define (string-every pred str)
    (let loop ([i 0])
      (or (= i (string-length str))
        (and (pred (string-ref str i)) (loop (+ i 1))))))

  (define (string-prefix?* prefix str)
    (and (>= (string-length str) (string-length prefix))
      (string=? (substring str 0 (string-length prefix)) prefix)))

  ;; flags: (results . rest), the results an alist of (name . value), newest first
  (define (parse-flags specs args)
    (define results '())
    (define rest '())

    (define (store! spec value shown)
      (let ([name (spec-name spec)])
        (case (spec-kind spec)
          [(multi)
            (let ([values (split-commas value)])
              (for-each (lambda (v) (check-allowed spec v shown)) values)
              (let ([old (assoc name results)])
                (set! results
                  (cons (cons name (append (if old (cdr old) '()) values))
                    (remove (lambda (entry) (string=? (car entry) name)) results)))))]
          [(option)
            (check-allowed spec value shown)
            (set! results (cons (cons name value) results))]
          [else (set! results (cons (cons name #t) results))])))

    ;; Reads the arguments ARGS; returns when they are used up.
    (define (loop args)
      (cond
        [(null? args) #t]
        [else
          (let ([arg (car args)])
            (cond
              [(string=? arg "--") (set! rest (append rest (cdr args)))]
              [(and (> (string-length arg) 2) (string-prefix?* "--" arg)) (loop (long-option arg (cdr args)))]
              [(and (> (string-length arg) 1) (char=? (string-ref arg 0) #\-)
                 (letter-or-digit? (string-ref arg 1)))
                (loop (short-options arg (cdr args)))]
              [else
                (set! rest (append rest (list arg)))
                (loop (cdr args))]))]))

    ;; --name, --name=value, --name value; returns the arguments that remain
    (define (long-option arg more)
      (let* ([eq (string-index arg #\=)]
             [name (substring arg 2 (or eq (string-length arg)))]
             [value (and eq (substring arg (+ eq 1) (string-length arg)))])
        (unless (string-every name-char? name)
          (flags-error "Invalid option name '--~a'." name))
        (let ([spec (find-long specs name)])
          (unless spec (flags-error "Unknown option '--~a'." name))
          (cond
            [(not (takes-value? spec))
              (when value (flags-error "Flag option '~a' does not take a value." name))
              (store! spec #t name)
              more]
            [value (store! spec value name) more]
            [(null? more) (flags-error "Option '--~a' requires a value, but none was provided." name)]
            [else (store! spec (car more) name) (cdr more)]))))

    ;; -x, -xVALUE, -x VALUE, -abc (flags); returns the arguments that remain
    (define (short-options arg more)
      (let* ([letters (let scan ([i 1])
                       (if (and (< i (string-length arg)) (letter-or-digit? (string-ref arg i)))
                         (scan (+ i 1))
                         i))]
             [first (substring arg 1 2)]
             [spec (find-abbr specs first)])
        (unless spec (flags-error "Unknown option '-~a'." first))
        (cond
          [(takes-value? spec)
            (cond
              [(> (string-length arg) 2) (store! spec (substring arg 2 (string-length arg)) first) more]
              [(null? more) (flags-error "Option '-~a' requires a value, but none was provided." first)]
              [else (store! spec (car more) first) (cdr more)])]
          [else
            (unless (= letters (string-length arg))
              (flags-error "Flag option '-~a' cannot be combined with other options." first))
            (let each ([i 1])
              (when (< i letters)
                (let* ([c (substring arg i (+ i 1))] [s (find-abbr specs c)])
                  (unless s (flags-error "Unknown option '-~a'." c))
                  (unless (not (takes-value? s)) (flags-error "Option '-~a' is not a flag." c))
                  (store! s #t c)
                  (each (+ i 1)))))
            more])))

    (loop args)
    (for-each
      (lambda (spec)
        (when (and (eq? (spec-kind spec) 'multi) (not (assoc (spec-name spec) results)))
          (set! results (cons (cons (spec-name spec) '()) results))))
      specs)
    (cons results rest))

  (define (flag-ref flags name)
    (let ([entry (assoc name (car flags))])
      (and entry (cdr entry))))

  (define (flags-rest flags) (cdr flags))

  ;; The usage text for SPECS, without a final newline: one line per visible option, the values
  ;; of an option that takes only some on the line after it.
  (define (flags-usage specs)
    (define (left spec)
      (string-append (spec-long spec)
        (if (and (takes-value? spec) (spec-value-help spec))
          (string-append "=<" (spec-value-help spec) ">")
          "")))
    (define (spec-long spec) (string-append "--" (spec-name spec)))
    (define visible
      (filter (lambda (spec) (and (option-spec? spec) (not (spec-attribute spec 'hidden)))) specs))
    (define width
      (+ 4 (fold-left max 0 (map (lambda (spec) (string-length (left spec))) visible))))
    (define (pad str n)
      (if (>= (string-length str) n) str (string-append str (make-string (- n (string-length str)) #\space))))
    (trim-newline
      (apply string-append
      (map (lambda (spec)
            (cond
              [(eq? (spec-kind spec) 'separator) (string-append "\n" (spec-name spec) "\n")]
              [(spec-attribute spec 'hidden) ""]
              [else
                (string-append
                  (let ([abbr (spec-attribute spec 'abbr)])
                    (if abbr (string-append "-" abbr " ") "   "))
                  (pad (left spec) width)
                  (spec-help spec)
                  "\n"
                  (let ([allowed (spec-attribute spec 'allowed)])
                    (if allowed
                      (string-append (make-string (+ 3 width) #\space)
                        "[" (join-strings allowed ", ") "]\n")
                      "")))]))
        specs))))

  (define (trim-newline str)
    (if (and (> (string-length str) 0) (char=? (string-ref str (- (string-length str) 1)) #\newline))
      (substring str 0 (- (string-length str) 1))
      str))

  (define (join-strings strings sep)
    (cond
      [(null? strings) ""]
      [else
        (fold-left (lambda (acc s) (string-append acc sep s)) (car strings) (cdr strings))])))
