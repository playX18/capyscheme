(define-library (capy args grammar)
  (import (scheme base)
          (only (srfi 1) fold)
          (capy args option)
          (capy args help optional))
  (cond-expand
    ((library (srfi 130))
      (import (srfi 130)))
    (else
      (import (only (capy args string)
                    string-join
                    string-pad-right))))
  (export
    make-grammar
    make-grammar-builder
    grammar*
    define-grammar
    grammar?
    grammar-builder?
    grammar-builder-allow-trailing?
    grammar-builder-allow-anything?
    grammar-builder-operations
    grammar-builder-allow-trailing
    grammar-builder-allow-anything
    grammar-builder-add-option
    grammar-builder-add-flag
    grammar-builder-add-multi-option
    grammar-builder-add-command
    grammar-builder-add-separator
    grammar-builder-default-command
    grammar-builder-build
    grammar-options grammar-options-set!
    grammar-commands grammar-commands-set!
    grammar-aliases grammar-aliases-set!
    grammar-default-command grammar-default-command-set!
    grammar-options-and-separators grammar-options-and-separators-set!
    grammar-allow-trailing? grammar-allow-trailing?-set!
    grammar-allow-anything? grammar-allow-anything?-set!
    grammar-add-option!
    grammar-add-flag!
    grammar-add-multi-option!
    grammar-find-by-name-or-alias
    grammar-find-by-abbr
    grammar-add-command!
    grammar-add-separator!
    grammar-usage
    str-split)

  (begin
    (define (plist-ref args key default)
      (let loop ((args args))
        (cond
          ((null? args) default)
          ((not (pair? (cdr args))) default)
          ((eq? (car args) key) (cadr args))
          (else (loop (cddr args))))))

    (define-record-type <grammar>
      (%make-grammar
        options
        commands
        aliases
        default-command
        options-and-separators
        allow-trailing?
        allow-anything?
        %usage)
      grammar?
      (options grammar-options grammar-options-set!)
      (commands grammar-commands grammar-commands-set!)
      (aliases grammar-aliases grammar-aliases-set!)
      (default-command grammar-default-command grammar-default-command-set!)
      (options-and-separators grammar-options-and-separators grammar-options-and-separators-set!)
      (allow-trailing? grammar-allow-trailing? grammar-allow-trailing?-set!)
      (allow-anything? grammar-allow-anything? grammar-allow-anything?-set!)
      (%usage %grammar-usage %grammar-usage-set!)) ; usage is populated with usage string once its generated

    (define-record-type <grammar-builder>
      (%make-grammar-builder
        allow-trailing?
        allow-anything?
        operations)
      grammar-builder?
      (allow-trailing? grammar-builder-allow-trailing?)
      (allow-anything? grammar-builder-allow-anything?)
      (operations grammar-builder-operations))

    (define (make-grammar . args)
      (let-optionals args ((allow-trailing? #t))
        (%make-grammar
          '()
          '()
          '()
          #f
          '()
          allow-trailing?
          #f
          #f)))

    (define (make-grammar-builder . args)
      (let-optionals args ((allow-trailing? #t))
        (%make-grammar-builder allow-trailing? #f '())))

    (define (grammar-builder-cons builder operation)
      (%make-grammar-builder
        (grammar-builder-allow-trailing? builder)
        (grammar-builder-allow-anything? builder)
        (cons operation (grammar-builder-operations builder))))

    (define (grammar-builder-allow-anything builder allow-anything?)
      (%make-grammar-builder
        (grammar-builder-allow-trailing? builder)
        allow-anything?
        (grammar-builder-operations builder)))

    (define (grammar-builder-allow-trailing builder allow-trailing?)
      (%make-grammar-builder
        allow-trailing?
        (grammar-builder-allow-anything? builder)
        (grammar-builder-operations builder)))

    (define (grammar-builder-add-separator builder separator)
      (grammar-builder-cons builder (list 'separator separator)))

    (define (grammar-builder-add-command builder command . subgrammar?)
      (grammar-builder-cons builder (list 'command command subgrammar?)))

    (define (grammar-builder-default-command builder command)
      (grammar-builder-cons builder (list 'default-command command)))

    (define (grammar-builder-add-flag builder name . args)
      (grammar-builder-cons builder (list 'flag name args)))

    (define (grammar-builder-add-option builder name . args)
      (grammar-builder-cons builder (list 'option name args)))

    (define (grammar-builder-add-multi-option builder name . args)
      (grammar-builder-cons builder (list 'multi-option name args)))

    (define (grammar-builder-build builder)
      (define grammar (make-grammar (grammar-builder-allow-trailing? builder)))
      (grammar-allow-anything?-set! grammar (grammar-builder-allow-anything? builder))
      (for-each
        (lambda (operation)
          (case (car operation)
            ((separator)
              (grammar-add-separator! grammar (cadr operation)))
            ((command)
              (let* ((name (cadr operation))
                     (subgrammar? (car (cddr operation)))
                     (subgrammar (if (null? subgrammar?)
                                   (make-grammar)
                                   (let ((candidate (car subgrammar?)))
                                     (if (grammar-builder? candidate)
                                       (grammar-builder-build candidate)
                                       candidate)))))
                (grammar-add-command! grammar name subgrammar)))
            ((default-command)
              (grammar-default-command-set! grammar (cadr operation)))
            ((flag)
              (apply grammar-add-flag! grammar (cadr operation) (car (cddr operation))))
            ((option)
              (apply grammar-add-option! grammar (cadr operation) (car (cddr operation))))
            ((multi-option)
              (apply grammar-add-multi-option! grammar (cadr operation) (car (cddr operation))))))
        (reverse (grammar-builder-operations builder)))
      grammar)

    (define-syntax grammar*
      (syntax-rules ()
        ((_ clause ...)
          (grammar-builder-build
            (grammar*-clauses (make-grammar-builder) clause ...)))))

    (define-syntax define-grammar
      (syntax-rules ()
        ((_ name clause ...)
          (define name (grammar* clause ...)))))

    (define-syntax grammar*-clauses
      (syntax-rules (allow-trailing allow-anything separator flag option multi-option command subcommand default-command)
        ((_ builder)
          builder)
        ((_ builder (allow-trailing value) rest ...)
          (grammar*-clauses
            (grammar-builder-allow-trailing builder value)
            rest ...))
        ((_ builder (allow-anything value) rest ...)
          (grammar*-clauses
            (grammar-builder-allow-anything builder value)
            rest ...))
        ((_ builder (separator text) rest ...)
          (grammar*-clauses
            (grammar-builder-add-separator builder text)
            rest ...))
        ((_ builder (flag name arg ...) rest ...)
          (grammar*-clauses
            (grammar-builder-add-flag builder name arg ...)
            rest ...))
        ((_ builder (option name arg ...) rest ...)
          (grammar*-clauses
            (grammar-builder-add-option builder name arg ...)
            rest ...))
        ((_ builder (multi-option name arg ...) rest ...)
          (grammar*-clauses
            (grammar-builder-add-multi-option builder name arg ...)
            rest ...))
        ((_ builder (command name) rest ...)
          (grammar*-clauses
            (grammar-builder-add-command builder name)
            rest ...))
        ((_ builder (command name subgrammar) rest ...)
          (grammar*-clauses
            (grammar-builder-add-command builder name subgrammar)
            rest ...))
        ((_ builder (subcommand name) rest ...)
          (grammar*-clauses
            (grammar-builder-add-command builder name)
            rest ...))
        ((_ builder (subcommand name subgrammar) rest ...)
          (grammar*-clauses
            (grammar-builder-add-command builder name subgrammar)
            rest ...))
        ((_ builder (default-command name) rest ...)
          (grammar*-clauses
            (grammar-builder-default-command builder name)
            rest ...))))

    (define (grammar-clear-usage! grammar)
      (%grammar-usage-set! grammar #f))

    (define (grammar-add-separator! grammar separator)
      (unless (string? separator)
        (error "Separator must be a string" separator))
      (grammar-clear-usage! grammar)
      (grammar-options-and-separators-set! grammar
        (cons separator (grammar-options-and-separators grammar))))

    (define (grammar-add-command! grammar command . subgrammar?)
      (when (assoc command (grammar-commands grammar))
        (error "Command already exists in grammar" command))
      (let ((subgrammar (if (null? subgrammar?)
                            (make-grammar)
                            (car subgrammar?))))
        (unless (grammar? subgrammar)
          (error "Grammar for command must be of type <grammar>" subgrammar))
        (grammar-clear-usage! grammar)
        (grammar-commands-set! grammar
          (cons (cons command subgrammar) (grammar-commands grammar)))
        subgrammar))

    (define (grammar-add-flag! grammar name . args)
      (add-option! grammar
        'name: name
        'abbr: (plist-ref args 'abbr: #f)
        'help: (plist-ref args 'help: #f)
        'defaults-to: (plist-ref args 'defaults-to: #f)
        'negatable?: (plist-ref args 'negatable?: #t)
        'callback: (plist-ref args 'callback: #f)
        'type: 'flag
        'hide?: (plist-ref args 'hide?: #f)
        'hide-negated-usage?: (plist-ref args 'hide-negated-usage?: #f)
        'aliases: (plist-ref args 'aliases: '())))

    (define (grammar-add-option! grammar name . args)
      (add-option! grammar
        'name: name
        'abbr: (plist-ref args 'abbr: #f)
        'help: (plist-ref args 'help: #f)
        'value-help: (plist-ref args 'value-help: #f)
        'allowed: (plist-ref args 'allowed: #f)
        'allowed-help: (plist-ref args 'allowed-help: #f)
        'defaults-to: (plist-ref args 'defaults-to: #f)
        'callback: (plist-ref args 'callback: #f)
        'type: 'single
        'mandatory?: (plist-ref args 'mandatory?: #f)
        'hide?: (plist-ref args 'hide?: #f)
        'aliases: (plist-ref args 'aliases: '())))

    (define (grammar-add-multi-option! grammar name . args)
      (add-option! grammar
        'name: name
        'abbr: (plist-ref args 'abbr: #f)
        'help: (plist-ref args 'help: #f)
        'value-help: (plist-ref args 'value-help: #f)
        'allowed: (plist-ref args 'allowed: #f)
        'allowed-help: (plist-ref args 'allowed-help: #f)
        'defaults-to: (plist-ref args 'defaults-to: '())
        'callback: (plist-ref args 'callback: #f)
        'type: 'multi
        'split-commas?: (plist-ref args 'split-commas?: #t)
        'hide?: (plist-ref args 'hide?: #f)
        'aliases: (plist-ref args 'aliases: '())))



    (define (grammar-find-by-name-or-alias grammar name)

      (let ((alias (assoc name (grammar-aliases grammar))))
        (let ((name (if alias (cdr alias) name)))
          (cond
            ((assoc name (grammar-options grammar)) => cdr)
            (else #f)))))

    (define (grammar-find-by-abbr grammar abbr)
      (let loop ((options (grammar-options grammar)))
        (cond
          ((null? options) #f)
          ((equal? (option-abbr (cdr (car options))) abbr)
           (car options))
          (else (loop (cdr options))))))

    (define (grammar-default-for grammar name)
      (let ((value (grammar-find-by-name-or-alias grammar name)))
        (cond
          ((not value) (error "no such option in grammar" name))
          (else (option-defaults-to (cdr value))))))



    (define (add-option! grammar . args)
      (let ((name (plist-ref args 'name: #f))
            (abbr (plist-ref args 'abbr: #f))
            (help (plist-ref args 'help: #f))
            (value-help (plist-ref args 'value-help: #f))
            (allowed (plist-ref args 'allowed: #f))
            (allowed-help (plist-ref args 'allowed-help: #f))
            (defaults-to (plist-ref args 'defaults-to: #f))
            (negatable? (plist-ref args 'negatable?: #f))
            (callback (plist-ref args 'callback: #f))
            (type (plist-ref args 'type: 'single))
            (split-commas? (plist-ref args 'split-commas?: #f))
            (mandatory? (plist-ref args 'mandatory?: #f))
            (hide? (plist-ref args 'hide?: #f))
            (hide-negated-usage? (plist-ref args 'hide-negated-usage?: #f))
            (aliases (plist-ref args 'aliases: '())))
        (for-each (lambda (name)
                    (when (grammar-find-by-name-or-alias grammar name)
                      (error "Option already exists in grammar" name)))
          (cons name aliases))
        (when abbr
          (when (grammar-find-by-abbr grammar abbr)
            (error "Option with abbreviation already exists in grammar" abbr)))

        (when (and mandatory? defaults-to)
          (error "Option cannot be both mandatory and have a default value" name))

        (when (and (not negatable?) hide-negated-usage?)
          (error "Option cannot have hide-negated-usage? set if negatable? is false" name))

        (let ((option (make-option
                        name
                        'abbr: abbr
                        'help: help
                        'value-help: value-help
                        'allowed: allowed
                        'allowed-help: allowed-help
                        'defaults-to: defaults-to
                        'negatable?: negatable?
                        'callback: callback
                        'type: type
                        'split-commas?: split-commas?
                        'mandatory?: mandatory?
                        'hide?: hide?
                        'hide-negated-usage?: hide-negated-usage?
                        'aliases: aliases)))

          (grammar-clear-usage! grammar)
          (grammar-options-set! grammar
            (cons (cons name option) (grammar-options grammar)))
          (grammar-options-and-separators-set! grammar
            (cons option (grammar-options-and-separators grammar)))
          (grammar-aliases-set! grammar
            (append
              (map (lambda (alias) (cons alias name)) aliases)
              (grammar-aliases grammar)))
          option)))

    (define (generate-usage options-and-separators line-length)
      (define column-count 3)
      (define buffer (open-output-string))
      (define current-column 0)
      (define (calculate-column-width)
        (let loop ((ops options-and-separators)
                   (abbr 0)
                   (title 0))
          (cond
            ((null? ops) (list abbr (+ 4 title)))
            ((not (option? (car ops))) (loop (cdr ops) abbr title))
            ((option-hide? (car ops)) (loop (cdr ops) abbr title))
            (else
              (let* ((option (car ops))
                     (abbr (max abbr (string-length (abbreviation option))))
                     (title (max title (+ (string-length (long-option option))
                                          (string-length (mandatory-option option)))))
                     (title* (if (not (option-allowed-help option))
                                title
                                (fold (lambda (entry title)
                                  (max title (string-length (allowed-title option (car entry)))))
                                 title
                                 (option-allowed-help option)))))
                (loop (cdr ops) abbr title*))))))
      (define (abbreviation opt)
        (cond
          ((not (option-abbr opt)) "")
          (else (string-append "-" (option-abbr opt) " "))))
      (define (long-option opt)
        (define result
          (cond
            ((and (option-negatable? opt) (not (option-hide-negated-usage? opt)))
             (string-append "--[no-]" (option-name opt)))
            (else
              (string-append "--" (option-name opt)))))
        (cond
          ((option-value-help opt)
           (string-append result "=<" (option-value-help opt) ">"))
          (else result)))
      (define (mandatory-option opt)
        (cond
          ((option-mandatory? opt) "(*)")
          (else "")))

      (define (allowed-title option allowed)
        (define default? (cond
          ((list? (option-defaults-to option))
           (member (option-defaults-to option) allowed))
          (else (equal? (option-defaults-to option) allowed))))

        (define result (string-append "      [" allowed "]"))
        (if default?
            (string-append result " (default)")
            result))

      (define column-widths (calculate-column-width))
      (define newlines-needed 0)
      (define (write-separator separator)
        (if (not (zero? (string-length (get-output-string buffer))))
          (write-string "\n\n" buffer))
        (write-string separator buffer)
        (set! newlines-needed 1))
      (define (newline)
        (set! newlines-needed (+ 1 newlines-needed))
        (set! current-column 0))

      (define (%write column text)
        (define lines (str-split text #\newline))

        (for-each (lambda (line)
          (%write-line column line))
          lines))
      (define (%write-line column line)
        (let loop ()
          (when (> newlines-needed 0)
            (write-string "\n" buffer)
            (set! newlines-needed (- newlines-needed 1))
            (loop)))

        (let loop ()
          (unless (= current-column column)
            (cond
              ((< current-column (- column-count 1))
                (write-string (make-string (list-ref column-widths current-column) #\space) buffer))
              (else
                (write-string "\n" buffer)))
            (set! current-column (modulo(+ 1 current-column) column-count))
            (loop)))

        (if (< column (length column-widths))
          (write-string (string-pad-right line (list-ref column-widths column)) buffer)
          (write-string line buffer))

        (set! current-column (modulo (+ 1 current-column) column-count))
        (when (= column (- column-count 1))
          (set! newlines-needed (+ 1 newlines-needed))))
      (define (build-allowed-list option)
        (define (default? x) (cond
          ((list? (option-defaults-to option))
           (member x (option-defaults-to option)))
          (else (equal? x (option-defaults-to option)))))
        (define allowed-buffer (open-output-string))
        (write-string "[" allowed-buffer)

        (let loop ((first #t) (allowed (option-allowed option)))
          (cond
            ((null? allowed)
              (write-string "]" allowed-buffer)
              (get-output-string allowed-buffer))
            (else
              (unless first (write-string ", " allowed-buffer))
              (write-string (car allowed) allowed-buffer)
              (if (default? (car allowed))
                (write-string " (default)" allowed-buffer))
              (loop #f (cdr allowed))))))
      (define (write-option option)
        (%write 0 (abbreviation option))
        (%write 1 (string-append (long-option option) (mandatory-option option)))

        (when (option-help option)
          (%write 2 (option-help option)))
        (cond
          ((option-allowed-help option)
            (newline)
            (for-each (lambda (entry)
                (%write 1 (allowed-title option (car entry)))
                (%write 2 (cdr entry)))

              (option-allowed-help option))
            (newline))
          ((option-allowed option)
            (%write 2 (build-allowed-list option)))
          ((option-flag? option)
            (if (eq? #t (option-defaults-to option))
              (%write 2 "(defaults to on)")))
          ((option-multi? option)
            (if (and (list? (option-defaults-to option))
                     (not (null? (option-defaults-to option))))
              (%write 2 (string-append "(defaults to "
                                      (string-join (map (lambda (x) x) (option-defaults-to option)) ", ")
                                      ")"))))
          ((option-defaults-to option)
            (%write 2 (string-append "(defaults to "
                                    (option-defaults-to option)
                                    ")")))))
      (for-each (lambda (item)
        (cond
          ((string? item) (write-separator item))
          ((and (option? item) (not (option-hide? item))) (write-option item))))
        options-and-separators)
      (get-output-string buffer))

    ;;; str-split : Apr 2006 Doug Hoyte, hcsw.org.
    ;;; ----
    ;;; Splits a string 'str into a list of strings
    ;;; that were separated by the delimiter character 'ch
    ;;; ----
    ;;; Efficient as possible given that we can't count on
    ;;; 'str being an immutable string.

    (define (str-split str ch)
      (let ((len (string-length str)))
        (letrec
          ((split
            (lambda (a b)
              (cond
                ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                  ((char=? ch (string-ref str b)) (if (= a b)
                    (split (+ 1 a) (+ 1 b))
                      (cons (substring str a b) (split b b))))
                    (else (split a (+ 1 b)))))))
                      (split 0 0))))

    (define (grammar-usage grammar)
      (if (%grammar-usage grammar)
        (%grammar-usage grammar)
        (let* ((options-and-separators (reverse (grammar-options-and-separators grammar)))
               (line-length 80)
               (usage (generate-usage options-and-separators line-length)))
          (%grammar-usage-set! grammar usage)
          usage)))))
