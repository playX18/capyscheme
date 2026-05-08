#!r6rs

(library (capy lsp analysis)
  (export analyze-document run-document-action)
  (import
    (capy)
    (rnrs)
    (rnrs io ports)
    (capy compiler tree-il)
    (capy compiler tree-il terms)
    (capy pretty-print)
    (capy session))

  (define max-syntax-diagnostics 64)
  (define current-analysis-path #f)
  (define current-analysis-text "")
  (define expand-action "capy.lsp.action.expand")

  (define (write-to-string obj)
    (let ((port (open-output-string)))
      (write obj port)
      (get-output-string port)))

  (define (condition->message exn)
    (let ((message
            (cond
              [(and (condition? exn) (message-condition? exn))
                (condition-message exn)]
              [else (write-to-string exn)])))
      (if (and (condition? exn)
           (irritants-condition? exn)
           (pair? (condition-irritants exn)))
        (string-append message ": " (write-to-string (condition-irritants exn)))
        message)))

  (define (json-nullable-string value)
    (if (string? value) value 'null))

  (define (json-nullable-version value)
    (cond
      [(or (string? value) (number? value)) value]
      [else 'null]))

  (define (string-join parts sep)
    (cond
      [(null? parts) ""]
      [else
        (let loop ((parts (cdr parts)) (out (car parts)))
          (if (null? parts)
            out
            (loop (cdr parts) (string-append out sep (car parts)))))]))

  (define (datum->name-string datum)
    (cond
      [(symbol? datum) (symbol->string datum)]
      [(string? datum) datum]
      [(number? datum) (number->string datum)]
      [(and (list? datum)
          (let loop ((xs datum))
            (or (null? xs)
              (and (or (symbol? (car xs)) (number? (car xs)))
                (loop (cdr xs))))))
        (string-join
          (map (lambda (part)
                (if (symbol? part)
                  (symbol->string part)
                  (number->string part)))
            datum)
          " ")]
      [else (write-to-string datum)]))

  (define (make-position line character)
    `((line . ,line)
      (character . ,character)))

  (define (make-range line character)
    `((start . ,(make-position line character))
      (end . ,(make-position line (+ character 1)))))

  (define (make-token-range line character length)
    `((start . ,(make-position line character))
      (end . ,(make-position line (+ character length)))))

  (define (zero-range)
    `((start . ,(make-position 0 0))
      (end . ,(make-position 0 0))))

  (define action-definitions
    `((,expand-action . "Expand")))

  (define (sourcev->range sourcev)
    (if (and (vector? sourcev)
         (>= (vector-length sourcev) 3)
         (number? (vector-ref sourcev 1))
         (number? (vector-ref sourcev 2)))
      (make-range (max 0 (- (vector-ref sourcev 1) 1)) (vector-ref sourcev 2))
      (zero-range)))

  (define (sourcev->line sourcev)
    (and (vector? sourcev)
      (>= (vector-length sourcev) 2)
      (number? (vector-ref sourcev 1))
      (max 0 (- (vector-ref sourcev 1) 1))))

  (define (sourcev->character sourcev)
    (and (vector? sourcev)
      (>= (vector-length sourcev) 3)
      (number? (vector-ref sourcev 2))
      (vector-ref sourcev 2)))

  (define (range-position range endpoint key)
    (let ((position (and (list? range) (assq endpoint range))))
      (and position
        (let ((entry (and (list? (cdr position)) (assq key (cdr position)))))
          (and entry (number? (cdr entry)) (cdr entry))))))

  (define (range-start range key)
    (range-position range 'start key))

  (define (range-end range key)
    (range-position range 'end key))

  (define (string-index-of text needle start)
    (let ((text-len (string-length text))
          (needle-len (string-length needle)))
      (let loop ((i (max 0 start)))
        (cond
          [(> (+ i needle-len) text-len) #f]
          [(string=? (substring text i (+ i needle-len)) needle) i]
          [else (loop (+ i 1))]))))

  (define (line-character->index text line character)
    (let ((len (string-length text)))
      (let loop ((i 0) (current-line 0) (current-character 0))
        (cond
          [(or (>= i len)
             (and (= current-line line) (= current-character character)))
            i]
          [(char=? (string-ref text i) #\newline)
            (loop (+ i 1) (+ current-line 1) 0)]
          [else
            (loop (+ i 1) current-line (+ current-character 1))]))))

  (define (index->line-character text index)
    (let loop ((i 0) (line 0) (character 0))
      (cond
        [(or (>= i index) (>= i (string-length text)))
          (values line character)]
        [(char=? (string-ref text i) #\newline)
          (loop (+ i 1) (+ line 1) 0)]
        [else
          (loop (+ i 1) line (+ character 1))])))

  (define (range->text text range)
    (let ((start-line (range-start range 'line))
          (start-character (range-start range 'character))
          (end-line (range-end range 'line))
          (end-character (range-end range 'character)))
      (and start-line
        start-character
        end-line
        end-character
        (let ((start (line-character->index text start-line start-character))
              (end (line-character->index text end-line end-character)))
          (and (<= start end)
            (substring text start end))))))

  (define (identifier-boundary? text index)
    (or (< index 0)
      (>= index (string-length text))
      (delimiter? (string-ref text index))))

  (define (identifier-token-at? text token index)
    (let ((end (+ index (string-length token))))
      (and (<= end (string-length text))
        (string=? (substring text index end) token)
        (identifier-boundary? text (- index 1))
        (identifier-boundary? text end))))

  (define (find-identifier-token text token start)
    (let loop ((index (string-index-of text token start)))
      (cond
        [(not index) #f]
        [(identifier-token-at? text token index) index]
        [else (loop (string-index-of text token (+ index 1)))])))

  (define (find-identifier-after-marker text token start marker)
    (let loop ((marker-index (string-index-of text marker start)))
      (cond
        [(not marker-index) #f]
        [(find-identifier-token text token (+ marker-index (string-length marker)))
          =>
          (lambda (token-index) token-index)]
        [else (loop (string-index-of text marker (+ marker-index 1)))])))

  (define (sourcev->binding-range sourcev name)
    (let ((line (sourcev->line sourcev))
          (character (sourcev->character sourcev))
          (token (symbol->string name)))
      (let* ((start (if (and line character)
                      (line-character->index current-analysis-text line character)
                      0))
             (index (or (find-identifier-after-marker current-analysis-text token start "(define (")
                      (find-identifier-after-marker current-analysis-text token start "(define ")
                      (find-identifier-after-marker current-analysis-text token start "(define-record-type ")
                      (find-identifier-token current-analysis-text token start))))
        (if index
          (call-with-values
            (lambda () (index->line-character current-analysis-text index))
            (lambda (line character)
              (make-token-range line character (string-length token))))
          (sourcev->range sourcev)))))

(define (make-diagnostic severity code message line character)
  `((source . "capy-lsp")
    (severity . ,severity)
    (code . ,code)
    (message . ,message)
    (range . ,(make-range line character))))

(define (make-diagnostic-at severity code message range)
  `((source . "capy-lsp")
    (severity . ,severity)
    (code . ,code)
    (message . ,message)
    (range . ,range)))

(define (syntax-error message line character)
  (make-diagnostic "error" "syntax" message line character))

(define (condition-range exn)
  (let ((src (and (condition? exn) (condition-sourcev exn))))
    (if src
      (sourcev->range src)
      (zero-range))))

(define (read-error-diagnostic exn)
  (make-diagnostic-at
    "error"
    "read"
    (string-append "read failed: " (condition->message exn))
    (condition-range exn)))

(define (expand-error-diagnostic exn)
  (make-diagnostic-at
    "error"
    "expand"
    (string-append "expand failed: " (condition->message exn))
    (condition-range exn)))

(define (delimiter? ch)
  (or (eof-object? ch)
    (char-whitespace? ch)
    (char=? ch #\()
    (char=? ch #\))
    (char=? ch #\[)
    (char=? ch #\])
    (char=? ch #\")
    (char=? ch #\;)))

(define (matching-close open)
  (case open
    [(#\() #\)]
    [(#\[) #\]]
    [else #\nul]))

(define (add-diagnostic diagnostics diagnostic)
  (if (< (length diagnostics) max-syntax-diagnostics)
    (cons diagnostic diagnostics)
    diagnostics))

(define (scan-syntax text)
  (define len (string-length text))
  (define (at i) (string-ref text i))
  (define (advance-line line character ch)
    (if (char=? ch #\newline)
      (values (+ line 1) 0)
      (values line (+ character 1))))
  (define (skip-line-comment i line character stack diagnostics)
    (let loop ((i i) (line line) (character character))
      (if (>= i len)
        (scan i line character stack diagnostics)
        (let ((ch (at i)))
          (call-with-values
            (lambda () (advance-line line character ch))
            (lambda (next-line next-character)
              (if (char=? ch #\newline)
                (scan (+ i 1) next-line next-character stack diagnostics)
                (loop (+ i 1) next-line next-character))))))))
  (define (skip-string i line character stack diagnostics start-line start-character)
    (let loop ((i i) (line line) (character character) (escaped? #f))
      (cond
        [(>= i len)
          (scan i
            line
            character
            stack
            (add-diagnostic
              diagnostics
              (syntax-error "unterminated string literal" start-line start-character)))]
        [else
          (let ((ch (at i)))
            (call-with-values
              (lambda () (advance-line line character ch))
              (lambda (next-line next-character)
                (cond
                  [escaped?
                    (loop (+ i 1) next-line next-character #f)]
                  [(char=? ch #\\)
                    (loop (+ i 1) next-line next-character #t)]
                  [(char=? ch #\")
                    (scan (+ i 1) next-line next-character stack diagnostics)]
                  [else
                    (loop (+ i 1) next-line next-character #f)]))))])))
  (define (skip-bar-symbol i line character stack diagnostics start-line start-character)
    (let loop ((i i) (line line) (character character) (escaped? #f))
      (cond
        [(>= i len)
          (scan i
            line
            character
            stack
            (add-diagnostic
              diagnostics
              (syntax-error "unterminated escaped symbol" start-line start-character)))]
        [else
          (let ((ch (at i)))
            (call-with-values
              (lambda () (advance-line line character ch))
              (lambda (next-line next-character)
                (cond
                  [escaped?
                    (loop (+ i 1) next-line next-character #f)]
                  [(char=? ch #\\)
                    (loop (+ i 1) next-line next-character #t)]
                  [(char=? ch #\|)
                    (scan (+ i 1) next-line next-character stack diagnostics)]
                  [else
                    (loop (+ i 1) next-line next-character #f)]))))])))
  (define (skip-block-comment i line character stack diagnostics start-line start-character)
    (let loop ((i i) (line line) (character character) (depth 1))
      (cond
        [(>= i len)
          (scan i
            line
            character
            stack
            (add-diagnostic
              diagnostics
              (syntax-error "unterminated block comment" start-line start-character)))]
        [(and (< (+ i 1) len)
            (char=? (at i) #\#)
            (char=? (at (+ i 1)) #\|))
          (loop (+ i 2) line (+ character 2) (+ depth 1))]
        [(and (< (+ i 1) len)
            (char=? (at i) #\|)
            (char=? (at (+ i 1)) #\#))
          (if (= depth 1)
            (scan (+ i 2) line (+ character 2) stack diagnostics)
            (loop (+ i 2) line (+ character 2) (- depth 1)))]
        [else
          (let ((ch (at i)))
            (call-with-values
              (lambda () (advance-line line character ch))
              (lambda (next-line next-character)
                (loop (+ i 1) next-line next-character depth))))])))
  (define (skip-character-literal i line character stack diagnostics)
    (let loop ((i i) (line line) (character character) (seen? #f))
      (if (>= i len)
        (scan i line character stack diagnostics)
        (let ((ch (at i)))
          (if (and seen? (delimiter? ch))
            (scan i line character stack diagnostics)
            (call-with-values
              (lambda () (advance-line line character ch))
              (lambda (next-line next-character)
                (loop (+ i 1) next-line next-character #t))))))))
  (define (close-paren ch line character stack diagnostics)
    (cond
      [(null? stack)
        (values
          stack
          (add-diagnostic
            diagnostics
            (syntax-error
              (string-append "unexpected " (string ch))
              line
              character)))]
      [else
        (let* ((entry (car stack))
               (open (car entry))
               (open-line (cadr entry))
               (open-character (caddr entry))
               (expected (matching-close open)))
          (if (char=? ch expected)
            (values (cdr stack) diagnostics)
            (values
              (cdr stack)
              (add-diagnostic
                diagnostics
                (syntax-error
                  (string-append
                    "mismatched "
                    (string ch)
                    ", expected "
                    (string expected)
                    " for opener at "
                    (number->string open-line)
                    ":"
                    (number->string open-character))
                  line
                  character)))))]))
  (define (finish stack diagnostics)
    (let loop ((stack stack) (diagnostics diagnostics))
      (if (null? stack)
        (reverse diagnostics)
        (let* ((entry (car stack))
               (open (car entry))
               (line (cadr entry))
               (character (caddr entry)))
          (loop
            (cdr stack)
            (add-diagnostic
              diagnostics
              (syntax-error
                (string-append "unclosed " (string open))
                line
                character)))))))
  (define (scan i line character stack diagnostics)
    (cond
      [(>= i len) (finish stack diagnostics)]
      [else
        (let ((ch (at i)))
          (cond
            [(char=? ch #\;)
              (skip-line-comment (+ i 1) line (+ character 1) stack diagnostics)]
            [(char=? ch #\")
              (skip-string (+ i 1) line (+ character 1) stack diagnostics line character)]
            [(char=? ch #\|)
              (skip-bar-symbol (+ i 1) line (+ character 1) stack diagnostics line character)]
            [(and (< (+ i 1) len)
                (char=? ch #\#)
                (char=? (at (+ i 1)) #\|))
              (skip-block-comment (+ i 2) line (+ character 2) stack diagnostics line character)]
            [(and (< (+ i 1) len)
                (char=? ch #\#)
                (char=? (at (+ i 1)) #\\))
              (skip-character-literal (+ i 2) line (+ character 2) stack diagnostics)]
            [(or (char=? ch #\() (char=? ch #\[))
              (scan
                (+ i 1)
                line
                (+ character 1)
                (cons (list ch line character) stack)
                diagnostics)]
            [(or (char=? ch #\)) (char=? ch #\]))
              (call-with-values
                (lambda () (close-paren ch line character stack diagnostics))
                (lambda (next-stack next-diagnostics)
                  (scan (+ i 1) line (+ character 1) next-stack next-diagnostics)))]
            [else
              (call-with-values
                (lambda () (advance-line line character ch))
                (lambda (next-line next-character)
                  (scan (+ i 1) next-line next-character stack diagnostics)))]))]))
  (scan 0 0 0 '() '()))

(define (read-forms text)
  (let ((port (open-input-string text)))
    (let loop ((forms '()))
      (guard (exn
              [else
                (values (reverse forms)
                  (list (read-error-diagnostic exn)))])
        (let ((form (read-syntax port)))
          (if (eof-object? form)
            (values (reverse forms) '())
            (loop (cons form forms))))))))

(define (expand-forms forms)
  (guard (exn
          [else
            (values '()
              (list (expand-error-diagnostic exn)))])
    (save-module-excursion
      (lambda ()
        (let loop ((forms forms) (out '()))
          (cond
            [(null? forms) (values (reverse out) '())]
            [else
              (loop (cdr forms)
                (cons (macroexpand (car forms) 'c '(compile load eval))
                  out))]))))))

(define (symbol-list? value)
  (and (list? value)
    (let loop ((value value))
      (or (null? value)
        (and (symbol? (car value))
          (loop (cdr value)))))))

(define (module-name-string name)
  (datum->name-string name))

(define (safe-module-filename name)
  (guard (exn [else #f])
    (if (symbol-list? name)
      (module-name->filename name)
      #f)))

(define (syntax-list stx)
  (syntax-case stx ()
    [() '()]
    [(x . xs) (cons #'x (syntax-list #'xs))]
    [_ '()]))

(define (lsp-absolute-path? path)
  (and (string? path)
    (> (string-length path) 0)
    (char=? (string-ref path 0) #\/)))

(define (lsp-dirname path)
  (let loop ((i (- (string-length path) 1)))
    (cond
      [(< i 0) #f]
      [(char=? (string-ref path i) #\/)
        (if (= i 0) "/" (substring path 0 i))]
      [else (loop (- i 1))])))

(define (syntax-filename stx)
  (let ((src (syntax-sourcev stx)))
    (and (vector? src)
      (>= (vector-length src) 1)
      (string? (vector-ref src 0))
      (vector-ref src 0))))

(define (include-path filename-stx)
  (let ((file (syntax->datum filename-stx)))
    (and (string? file)
      (cond
        [(lsp-absolute-path? file) file]
        [(and (syntax-filename filename-stx)
            (lsp-dirname (syntax-filename filename-stx)))
          =>
          (lambda (dir) (string-append dir "/" file))]
        [(and (string? current-analysis-path)
            (lsp-dirname current-analysis-path))
          =>
          (lambda (dir) (string-append dir "/" file))]
        [else file]))))

(define (read-include-forms filename-stx)
  (guard (exn [else '()])
    (let ((path (include-path filename-stx)))
      (if path
        (call-with-input-file
          path
          (lambda (port)
            (let loop ((forms '()))
              (let ((form (read-syntax port)))
                (if (eof-object? form)
                  (reverse forms)
                  (loop (cons (if (syntax? form)
                               form
                               (datum->syntax filename-stx form))
                         forms)))))))
        '()))))

(define (module-name-components? value)
  (and (list? value)
    (let loop ((xs value))
      (or (null? xs)
        (and (or (symbol? (car xs)) (number? (car xs)))
          (loop (cdr xs)))))))

(define (versioned-module-name? value)
  (and (list? value)
    (pair? value)
    (let loop ((xs value))
      (cond
        [(null? xs) #f]
        [(null? (cdr xs)) (list? (car xs))]
        [(or (symbol? (car xs)) (number? (car xs))) (loop (cdr xs))]
        [else #f]))))

(define (strip-module-version name)
  (if (versioned-module-name? name)
    (let loop ((xs name) (out '()))
      (if (null? (cdr xs))
        (reverse out)
        (loop (cdr xs) (cons (car xs) out))))
    name))

(define (colon-srfi-number sym)
  (and (symbol? sym)
    (let ((str (symbol->string sym)))
      (and (> (string-length str) 1)
        (char=? (string-ref str 0) #\:)
        (string->number (substring str 1))))))

(define (srfi-number-symbol n)
  (string->symbol
    (cond
      [(number? n) (string-append "srfi-" (number->string n))]
      [(symbol? n)
        (let ((colon-number (colon-srfi-number n))
              (text (symbol->string n)))
          (cond
            [colon-number (string-append "srfi-" (number->string colon-number))]
            [(string-prefix? "srfi-" text) text]
            [else (string-append "srfi-" text)]))]
      [else (string-append "srfi-" (datum->name-string n))])))

(define (normalize-module-name name)
  (let ((base (strip-module-version name)))
    (if (and (pair? base)
         (eq? (car base) 'srfi)
         (pair? (cdr base)))
      (cons 'srfi (cons (srfi-number-symbol (cadr base)) (cddr base)))
      base)))

(define (cond-expand-feature? feature)
  (memq feature %cond-expand-features))

(define (cond-expand-requirement? req)
  (cond
    [(and (pair? req) (eq? (car req) 'and))
      (let loop ((reqs (cdr req)))
        (or (null? reqs)
          (and (cond-expand-requirement? (car reqs))
            (loop (cdr reqs)))))]
    [(and (pair? req) (eq? (car req) 'or))
      (let loop ((reqs (cdr req)))
        (and (pair? reqs)
          (or (cond-expand-requirement? (car reqs))
            (loop (cdr reqs)))))]
    [(and (pair? req) (eq? (car req) 'not) (pair? (cdr req)))
      (not (cond-expand-requirement? (cadr req)))]
    [(and (pair? req) (eq? (car req) 'library) (pair? (cdr req)))
      (guard (exn [else #f])
        (resolve-r6rs-interface (cadr req))
        #t)]
    [(symbol? req) (cond-expand-feature? req)]
    [else #f]))

(define (select-cond-expand-decls clauses)
  (let loop ((clauses clauses))
    (cond
      [(null? clauses) '()]
      [else
        (let ((clause (syntax->datum (car clauses))))
          (cond
            [(and (pair? clause) (eq? (car clause) 'else))
              (syntax-list (datum->syntax (car clauses) (cdr clause)))]
            [(and (pair? clause) (cond-expand-requirement? (car clause)))
              (syntax-case (car clauses) ()
                [(_ decl ...) (syntax-list #'(decl ...))]
                [_ '()])]
            [else (loop (cdr clauses))]))])))

(define (partition-define-library-decls decls)
  (let loop ((decls decls) (exports '()) (imports '()) (code '()))
    (cond
      [(null? decls)
        (values (reverse exports) (reverse imports) (reverse code))]
      [else
        (let ((decl (car decls)))
          (let ((datum (syntax->datum decl)))
            (cond
              [(and (pair? datum) (eq? (car datum) 'export))
                (syntax-case decl ()
                  [(_ clause ...)
                    (loop (cdr decls)
                      (append (reverse (syntax->datum #'(clause ...))) exports)
                      imports
                      code)])]
              [(and (pair? datum) (eq? (car datum) 'import))
                (syntax-case decl ()
                  [(_ clause ...)
                    (loop (cdr decls)
                      exports
                      (append (reverse (syntax->datum #'(clause ...))) imports)
                      code)])]
              [(and (pair? datum) (eq? (car datum) 'begin))
                (syntax-case decl ()
                  [(_ expr ...)
                    (loop (cdr decls)
                      exports
                      imports
                      (cons `(begin ,@(map normalize-form (syntax-list #'(expr ...)))) code))])]
              [(and (pair? datum) (eq? (car datum) 'include))
                (syntax-case decl ()
                  [(_ filename ...)
                    (loop (cdr decls)
                      exports
                      imports
                      (cons `(begin ,@(map (lambda (filename)
                                            `(include ,(syntax->datum filename)))
                                       (syntax-list #'(filename ...))))
                        code))])]
              [(and (pair? datum) (eq? (car datum) 'include-ci))
                (syntax-case decl ()
                  [(_ filename ...)
                    (loop (cdr decls)
                      exports
                      imports
                      (cons `(begin ,@(map (lambda (filename)
                                            `(include-ci ,(syntax->datum filename)))
                                       (syntax-list #'(filename ...))))
                        code))])]
              [(and (pair? datum) (eq? (car datum) 'include-library-declarations))
                (syntax-case decl ()
                  [(_ filename ...)
                    (loop (append (apply append
                                   (map read-include-forms
                                     (syntax-list #'(filename ...))))
                           (cdr decls))
                      exports
                      imports
                      code)])]
              [(and (pair? datum) (eq? (car datum) 'cond-expand))
                (syntax-case decl ()
                  [(_ clause ...)
                    (loop (append (select-cond-expand-decls (syntax-list #'(clause ...)))
                           (cdr decls))
                      exports
                      imports
                      code)])]
              [else (loop (cdr decls) exports imports code)])))])))

(define (normalize-define-library form)
  (syntax-case form ()
    [(_ name decl ...)
      (call-with-values
        (lambda () (partition-define-library-decls (syntax-list #'(decl ...))))
        (lambda (exports imports code)
          `(library ,(normalize-module-name (syntax->datum #'name))
            (export ,@exports)
            (import ,@imports)
            ,@code)))]
    [_ (syntax->datum form)]))

(define (normalize-library form)
  (syntax-case form ()
    [(_ name (export espec ...) (import ispec ...) body ...)
      `(library ,(normalize-module-name (syntax->datum #'name))
        (export ,@(syntax->datum #'(espec ...)))
        (import ,@(syntax->datum #'(ispec ...)))
        ,@(map normalize-form (syntax-list #'(body ...))))]
    [_ (syntax->datum form)]))

(define (normalize-form form)
  (let ((datum (syntax->datum form)))
    (cond
      [(and (pair? datum) (eq? (car datum) 'define-library))
        (normalize-define-library form)]
      [(and (pair? datum) (eq? (car datum) 'library))
        (normalize-library form)]
      [(and (pair? datum) (eq? (car datum) 'begin))
        (syntax-case form ()
          [(_ body ...)
            `(begin ,@(map normalize-form (syntax-list #'(body ...))))]
          [_ datum])]
      [else datum])))

(define (normalize-forms forms)
  (map normalize-form forms))

(define (module-form? form)
  (let ((datum (syntax->datum form)))
    (and (pair? datum)
      (or (eq? (car datum) 'define-library)
        (eq? (car datum) 'library)))))

(define (expansion-forms forms datums)
  (let loop ((forms forms) (datums datums) (out '()))
    (cond
      [(or (null? forms) (null? datums)) (reverse out)]
      [(module-form? (car forms))
        (loop (cdr forms) (cdr datums) (cons (car datums) out))]
      [else
        (loop (cdr forms) (cdr datums) (cons (car forms) out))])))

(define (make-symbol name kind detail)
  (make-symbol-at name kind detail (zero-range)))

(define (make-symbol-at name kind detail range)
  (make-symbol-at/doc name kind detail range #f))

(define (make-symbol-at/doc name kind detail range documentation)
  `((name . ,name)
    (kind . ,kind)
    (detail . ,detail)
    (documentation . ,(json-nullable-string documentation))
    (range . ,range)
    (selectionRange . ,range)))

(define (make-completion name kind detail documentation)
  `((label . ,name)
    (kind . ,kind)
    (detail . ,detail)
    (documentation . ,(json-nullable-string documentation))))

(define (make-location uri range)
  `((uri . ,(json-nullable-string uri))
    (range . ,range)))

(define (make-reference uri name range definition-range)
  `((name . ,name)
    (uri . ,(json-nullable-string uri))
    (range . ,range)
    (definition . ,(make-location uri definition-range))))

(define (make-call-node uri name kind range)
  `((name . ,name)
    (kind . ,kind)
    (uri . ,(json-nullable-string uri))
    (range . ,range)))

(define (make-call-edge caller callee callee-kind range)
  `((caller . ,caller)
    (callee . ,callee)
    (calleeKind . ,callee-kind)
    (range . ,range)))

(define (make-import-completion name kind detail module-name file source-name documentation)
  `((label . ,name)
    (kind . ,kind)
    (detail . ,detail)
    (sourceModule . ,(json-nullable-string module-name))
    (sourceFile . ,(json-nullable-string file))
    (sourceName . ,(json-nullable-string source-name))
    (documentation . ,(json-nullable-string documentation))))

(define (string-all-digits? s start end)
  (let loop ((i start))
    (cond
      [(= i end) (< start i)]
      [(char-numeric? (string-ref s i)) (loop (+ i 1))]
      [else #f])))

(define (generated-suffix-start s)
  (let loop ((end (string-length s)) (segments 0))
    (let scan ((i (- end 1)))
      (cond
        [(< i 0) #f]
        [(char=? (string-ref s i) #\-)
          (if (string-all-digits? s (+ i 1) end)
            (if (= segments 1)
              i
              (loop i (+ segments 1)))
            #f)]
        [else (scan (- i 1))]))))

(define (clean-lexical-symbol name)
  (let* ((s (symbol->string name))
         (start (generated-suffix-start s))
         (base (if start (substring s 0 start) s)))
    (if (= (string-length base) 0)
      name
      (string->symbol base))))

(define (readable-name->symbol identity readable-name)
  (let ((datum (if (syntax? readable-name)
                (syntax-expression readable-name)
                readable-name)))
    (clean-lexical-symbol
      (cond
        [(symbol? datum) datum]
        [(symbol? identity) identity]
        [else 'lexical]))))

(define (collect-formal-locals identities readable-names detail source-symbols sourcev)
  (cond
    [(null? identities) '()]
    [(pair? identities)
      (append
        (collect-one-local (car identities) (car readable-names) detail source-symbols sourcev)
        (collect-formal-locals (cdr identities) (cdr readable-names) detail source-symbols sourcev))]
    [else
      (collect-one-local identities readable-names detail source-symbols sourcev)]))

(define (collect-binding-locals identities readable-names detail source-symbols sourcev)
  (let loop ((identities identities) (readable-names readable-names) (out '()))
    (if (or (null? identities) (null? readable-names))
      (reverse out)
      (loop
        (cdr identities)
        (cdr readable-names)
        (append (collect-one-local (car identities)
                 (car readable-names)
                 detail
                 source-symbols
                 sourcev)
          out)))))

(define (collect-fix-locals identities readable-names values source-symbols sourcev)
  (let loop ((identities identities) (readable-names readable-names) (values values) (out '()))
    (if (or (null? identities) (null? readable-names) (null? values))
      (reverse out)
      (let* ((identity (car identities))
             (readable-name (car readable-names))
             (value (car values))
             (name (readable-name->symbol identity readable-name)))
        (loop
          (cdr identities)
          (cdr readable-names)
          (cdr values)
          (if (source-symbol-visible? source-symbols name)
            (cons
              (make-symbol-at/doc
                (symbol->string name)
                (tree-il-define-kind value)
                (if (proc? value)
                  (or (tree-il-proc-signature (symbol->string name) value) "fix binding")
                  "fix binding")
                (sourcev->binding-range sourcev name)
                (tree-il-proc-documentation value))
              out)
            out))))))

(define (collect-valued-binding-locals identities readable-names values detail source-symbols sourcev)
  (let loop ((identities identities) (readable-names readable-names) (values values) (out '()))
    (if (or (null? identities) (null? readable-names) (null? values))
      (reverse out)
      (let* ((identity (car identities))
             (readable-name (car readable-names))
             (value (car values))
             (name (readable-name->symbol identity readable-name)))
        (loop
          (cdr identities)
          (cdr readable-names)
          (cdr values)
          (if (source-symbol-visible? source-symbols name)
            (cons
              (make-symbol-at/doc
                (symbol->string name)
                (tree-il-define-kind value)
                (if (proc? value)
                  (or (tree-il-proc-signature (symbol->string name) value) detail)
                  detail)
                (sourcev->binding-range sourcev name)
                (tree-il-proc-documentation value))
              out)
            out))))))

(define (source-symbol-visible? source-symbols name)
  (or (not (list? source-symbols))
    (memq name source-symbols)))

(define (collect-one-local identity readable-name detail source-symbols sourcev)
  (let ((name (readable-name->symbol identity readable-name)))
    (if (source-symbol-visible? source-symbols name)
      (list (make-symbol-at (symbol->string name)
             "variable"
             detail
             (sourcev->binding-range sourcev name)))
      '())))

(define (let-detail style)
  (string-append (symbol->string style) " binding"))

(define (extend-lexical-env identities readable-names detail source-symbols sourcev env)
  (cond
    [(null? identities) env]
    [(pair? identities)
      (extend-lexical-env
        (cdr identities)
        (cdr readable-names)
        detail
        source-symbols
        sourcev
        (extend-lexical-env (car identities)
          (car readable-names)
          detail
          source-symbols
          sourcev
          env))]
    [else
      (let ((name (readable-name->symbol identities readable-names)))
        (if (source-symbol-visible? source-symbols name)
          (cons (list identities name (sourcev->binding-range sourcev name) detail) env)
          env))]))

(define (lexical-env-ref env identity)
  (let loop ((env env))
    (cond
      [(null? env) #f]
      [(eq? (car (car env)) identity) (car env)]
      [else (loop (cdr env))])))

(define (collect-tree-il-references-from-term uri term source-symbols env)
  (define (collect-list terms env)
    (let loop ((terms terms) (out '()))
      (if (null? terms)
        (reverse out)
        (loop (cdr terms)
          (append (reverse (collect-tree-il-references-from-term
                            uri
                            (car terms)
                            source-symbols
                            env))
            out)))))
  (define (reference identity readable-name range)
    (let ((entry (lexical-env-ref env identity)))
      (if entry
        (let ((name (cadr entry))
              (definition-range (caddr entry)))
          (if (source-symbol-visible? source-symbols name)
            (list (make-reference uri
                   (symbol->string name)
                   range
                   definition-range))
            '()))
        '())))
  (cond
    [(or (constant? term)
        (void? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term))
      '()]
    [(lref? term)
      (reference (lref-sym term)
        (lref-name term)
        (sourcev->range (term-src term)))]
    [(lset? term)
      (append
        (reference (lset-sym term)
          (lset-name term)
          (sourcev->range (term-src term)))
        (collect-tree-il-references-from-term uri
          (lset-value term)
          source-symbols
          env))]
    [(module-set? term)
      (collect-tree-il-references-from-term uri (module-set-value term) source-symbols env)]
    [(toplevel-set? term)
      (collect-tree-il-references-from-term uri (toplevel-set-value term) source-symbols env)]
    [(toplevel-define? term)
      (collect-tree-il-references-from-term uri (toplevel-define-value term) source-symbols env)]
    [(if? term)
      (append
        (collect-tree-il-references-from-term uri (if-test term) source-symbols env)
        (collect-tree-il-references-from-term uri (if-then term) source-symbols env)
        (collect-tree-il-references-from-term uri (if-else term) source-symbols env))]
    [(let? term)
      (let ((body-env (extend-lexical-env (let-lhs term)
                       (let-ids term)
                       (let-detail (let-style term))
                       source-symbols
                       (term-src term)
                       env)))
        (append
          (collect-list (let-rhs term) env)
          (collect-tree-il-references-from-term uri (let-body term) source-symbols body-env)))]
    [(receive? term)
      (let ((consumer-env (extend-lexical-env (receive-vars term)
                           (receive-ids term)
                           "receive binding"
                           source-symbols
                           (term-src term)
                           env)))
        (append
          (collect-tree-il-references-from-term uri (receive-producer term) source-symbols env)
          (collect-tree-il-references-from-term uri
            (receive-consumer term)
            source-symbols
            consumer-env)))]
    [(fix? term)
      (let ((fix-env (extend-lexical-env (fix-lhs term)
                      (fix-ids term)
                      "fix binding"
                      source-symbols
                      (term-src term)
                      env)))
        (append
          (collect-list (fix-rhs term) fix-env)
          (collect-tree-il-references-from-term uri (fix-body term) source-symbols fix-env)))]
    [(proc? term)
      (collect-tree-il-references-from-term
        uri
        (proc-body term)
        source-symbols
        (extend-lexical-env (proc-args term)
          (proc-ids term)
          "lambda parameter"
          source-symbols
          (term-src term)
          env))]
    [(application? term)
      (append
        (collect-tree-il-references-from-term uri (application-operator term) source-symbols env)
        (collect-list (application-operands term) env))]
    [(primcall? term)
      (collect-list (primcall-args term) env)]
    [(values? term)
      (collect-list (values-values term) env)]
    [(sequence? term)
      (append
        (collect-tree-il-references-from-term uri (sequence-head term) source-symbols env)
        (collect-tree-il-references-from-term uri (sequence-tail term) source-symbols env))]
    [(wcm? term)
      (append
        (collect-tree-il-references-from-term uri (wcm-mark term) source-symbols env)
        (collect-tree-il-references-from-term uri (wcm-result term) source-symbols env))]
    [else '()]))

(define (collect-tree-il-references uri terms source-symbols)
  (let loop ((terms terms) (out '()))
    (if (null? terms)
      (reverse out)
      (loop (cdr terms)
        (append (reverse (collect-tree-il-references-from-term uri
                          (car terms)
                          source-symbols
                          '()))
          out)))))

(define (proc-name term fallback)
  (cond
    [(and (proc? term)
        (assq 'name (proc-meta term)))
      =>
      (lambda (entry)
        (let ((name (cdr entry)))
          (cond
            [(symbol? name) (symbol->string (clean-lexical-symbol name))]
            [(string? name) name]
            [else fallback])))]
    [else fallback]))

(define (call-target-name term env)
  (cond
    [(lref? term)
      (let ((entry (lexical-env-ref env (lref-sym term))))
        (if entry
          (values (symbol->string (cadr entry)) "local")
          (values (symbol->string
                   (readable-name->symbol (lref-sym term) (lref-name term)))
            "lexical")))]
    [(toplevel-ref? term)
      (values (datum->name-string (toplevel-ref-name term)) "toplevel")]
    [(module-ref? term)
      (values (string-append (module-name-string (module-ref-module term))
               ":"
               (datum->name-string (module-ref-name term)))
        "module")]
    [(primref? term)
      (values (datum->name-string (primref-prim term)) "primitive")]
    [else (values #f #f)]))

(define (collect-call-graph-from-term uri term source-symbols caller env)
  (define (collect-list terms caller env)
    (let loop ((terms terms) (out '()))
      (if (null? terms)
        (reverse out)
        (loop (cdr terms)
          (append (reverse (collect-call-graph-from-term
                            uri
                            (car terms)
                            source-symbols
                            caller
                            env))
            out)))))
  (define (proc-node name term)
    (if name
      (list (cons 'node
             (make-call-node uri name "function" (sourcev->range (term-src term)))))
      '()))
  (define (symbol-node symbol)
    (let ((name (and (list? symbol) (assq 'name symbol)))
          (range (and (list? symbol) (assq 'range symbol))))
      (if (and name range)
        (list (cons 'node
               (make-call-node uri (cdr name) "function" (cdr range))))
        '())))
  (define (symbol-nodes symbols)
    (let loop ((symbols symbols) (out '()))
      (if (null? symbols)
        (reverse out)
        (loop (cdr symbols) (append (reverse (symbol-node (car symbols))) out)))))
  (cond
    [(or (constant? term)
        (void? term)
        (lref? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term))
      '()]
    [(lset? term)
      (collect-call-graph-from-term uri (lset-value term) source-symbols caller env)]
    [(module-set? term)
      (collect-call-graph-from-term uri (module-set-value term) source-symbols caller env)]
    [(toplevel-set? term)
      (collect-call-graph-from-term uri (toplevel-set-value term) source-symbols caller env)]
    [(toplevel-define? term)
      (let* ((name (datum->name-string (toplevel-define-name term)))
             (value (toplevel-define-value term))
             (callee-caller (if (proc? value) (proc-name value name) caller)))
        (append
          (if (proc? value)
            (proc-node callee-caller value)
            '())
          (collect-call-graph-from-term uri value source-symbols callee-caller env)))]
    [(if? term)
      (append
        (collect-call-graph-from-term uri (if-test term) source-symbols caller env)
        (collect-call-graph-from-term uri (if-then term) source-symbols caller env)
        (collect-call-graph-from-term uri (if-else term) source-symbols caller env))]
    [(let? term)
      (let ((body-env (extend-lexical-env (let-lhs term)
                       (let-ids term)
                       (let-detail (let-style term))
                       source-symbols
                       (term-src term)
                       env)))
        (append
          (symbol-nodes
            (collect-valued-binding-locals (let-lhs term)
              (let-ids term)
              (let-rhs term)
              (let-detail (let-style term))
              source-symbols
              (term-src term)))
          (collect-list (let-rhs term) caller env)
          (collect-call-graph-from-term uri (let-body term) source-symbols caller body-env)))]
    [(receive? term)
      (let ((consumer-env (extend-lexical-env (receive-vars term)
                           (receive-ids term)
                           "receive binding"
                           source-symbols
                           (term-src term)
                           env)))
        (append
          (collect-call-graph-from-term uri (receive-producer term) source-symbols caller env)
          (collect-call-graph-from-term uri
            (receive-consumer term)
            source-symbols
            caller
            consumer-env)))]
    [(fix? term)
      (let ((fix-env (extend-lexical-env (fix-lhs term)
                      (fix-ids term)
                      "fix binding"
                      source-symbols
                      (term-src term)
                      env)))
        (append
          (symbol-nodes
            (collect-fix-locals (fix-lhs term)
              (fix-ids term)
              (fix-rhs term)
              source-symbols
              (term-src term)))
          (collect-list (fix-rhs term) caller fix-env)
          (collect-call-graph-from-term uri (fix-body term) source-symbols caller fix-env)))]
    [(proc? term)
      (let ((name (proc-name term #f)))
        (append
          (proc-node name term)
          (collect-call-graph-from-term
            uri
            (proc-body term)
            source-symbols
            (or name caller)
            (extend-lexical-env (proc-args term)
              (proc-ids term)
              "lambda parameter"
              source-symbols
              (term-src term)
              env))))]
    [(application? term)
      (call-with-values
        (lambda () (call-target-name (application-operator term) env))
        (lambda (callee callee-kind)
          (append
            (if (and caller callee)
              (list (cons 'edge
                     (make-call-edge caller
                       callee
                       callee-kind
                       (sourcev->range (term-src term)))))
              '())
            (collect-call-graph-from-term uri
              (application-operator term)
              source-symbols
              caller
              env)
            (collect-list (application-operands term) caller env))))]
    [(primcall? term)
      (append
        (if caller
          (list (cons 'edge
                 (make-call-edge caller
                   (datum->name-string (primcall-prim term))
                   "primitive"
                   (sourcev->range (term-src term)))))
          '())
        (collect-list (primcall-args term) caller env))]
    [(values? term)
      (collect-list (values-values term) caller env)]
    [(sequence? term)
      (append
        (collect-call-graph-from-term uri (sequence-head term) source-symbols caller env)
        (collect-call-graph-from-term uri (sequence-tail term) source-symbols caller env))]
    [(wcm? term)
      (append
        (collect-call-graph-from-term uri (wcm-mark term) source-symbols caller env)
        (collect-call-graph-from-term uri (wcm-result term) source-symbols caller env))]
    [else '()]))

(define (call-graph-entry-key entry)
  (let ((value (cdr entry)))
    (if (eq? (car entry) 'node)
      (list 'node (cdr (assq 'name value)) (cdr (assq 'kind value)))
      (list 'edge
        (cdr (assq 'caller value))
        (cdr (assq 'callee value))
        (cdr (assq 'calleeKind value))))))

(define (dedupe-call-graph-entries entries)
  (let loop ((items entries) (seen '()) (out '()))
    (cond
      [(null? items) (reverse out)]
      [else
        (let ((key (call-graph-entry-key (car items))))
          (if (member key seen)
            (loop (cdr items) seen out)
            (loop (cdr items) (cons key seen) (cons (car items) out))))])))

(define (collect-call-graph uri terms source-symbols)
  (let ((entries
          (dedupe-call-graph-entries
            (let loop ((terms terms) (out '()))
              (if (null? terms)
                (reverse out)
                (loop (cdr terms)
                  (append
                    (reverse
                      (collect-call-graph-from-term uri
                        (car terms)
                        source-symbols
                        "<top-level>"
                        '()))
                    out)))))))
    `((nodes . ,(list->vector
                 (let loop ((items entries) (out '()))
                   (cond
                     [(null? items) (reverse out)]
                     [(eq? (caar items) 'node)
                       (loop (cdr items) (cons (cdar items) out))]
                     [else (loop (cdr items) out)]))))
      (edges . ,(list->vector
                 (let loop ((items entries) (out '()))
                   (cond
                     [(null? items) (reverse out)]
                     [(eq? (caar items) 'edge)
                       (loop (cdr items) (cons (cdar items) out))]
                     [else (loop (cdr items) out)])))))))

(define (collect-tree-il-locals-from-term term source-symbols)
  (define (collect-list terms)
    (let loop ((terms terms) (out '()))
      (if (null? terms)
        (reverse out)
        (loop (cdr terms)
          (append (reverse (collect-tree-il-locals-from-term (car terms) source-symbols))
            out)))))
  (cond
    [(or (constant? term)
        (void? term)
        (lref? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term))
      '()]
    [(lset? term)
      (collect-tree-il-locals-from-term (lset-value term) source-symbols)]
    [(module-set? term)
      (collect-tree-il-locals-from-term (module-set-value term) source-symbols)]
    [(toplevel-set? term)
      (collect-tree-il-locals-from-term (toplevel-set-value term) source-symbols)]
    [(toplevel-define? term)
      (collect-tree-il-locals-from-term (toplevel-define-value term) source-symbols)]
    [(if? term)
      (append
        (collect-tree-il-locals-from-term (if-test term) source-symbols)
        (collect-tree-il-locals-from-term (if-then term) source-symbols)
        (collect-tree-il-locals-from-term (if-else term) source-symbols))]
    [(let? term)
      (append
        (collect-valued-binding-locals (let-lhs term)
          (let-ids term)
          (let-rhs term)
          (let-detail (let-style term))
          source-symbols
          (term-src term))
        (collect-list (let-rhs term))
        (collect-tree-il-locals-from-term (let-body term) source-symbols))]
    [(receive? term)
      (append
        (collect-formal-locals (receive-vars term)
          (receive-ids term)
          "receive binding"
          source-symbols
          (term-src term))
        (collect-tree-il-locals-from-term (receive-producer term) source-symbols)
        (collect-tree-il-locals-from-term (receive-consumer term) source-symbols))]
    [(fix? term)
      (append
        (collect-fix-locals (fix-lhs term)
          (fix-ids term)
          (fix-rhs term)
          source-symbols
          (term-src term))
        (collect-list (fix-rhs term))
        (collect-tree-il-locals-from-term (fix-body term) source-symbols))]
    [(proc? term)
      (append
        (collect-formal-locals (proc-args term)
          (proc-ids term)
          "lambda parameter"
          source-symbols
          (term-src term))
        (collect-tree-il-locals-from-term (proc-body term) source-symbols))]
    [(application? term)
      (append
        (collect-tree-il-locals-from-term (application-operator term) source-symbols)
        (collect-list (application-operands term)))]
    [(primcall? term)
      (collect-list (primcall-args term))]
    [(values? term)
      (collect-list (values-values term))]
    [(sequence? term)
      (append
        (collect-tree-il-locals-from-term (sequence-head term) source-symbols)
        (collect-tree-il-locals-from-term (sequence-tail term) source-symbols))]
    [(wcm? term)
      (append
        (collect-tree-il-locals-from-term (wcm-mark term) source-symbols)
        (collect-tree-il-locals-from-term (wcm-result term) source-symbols))]
    [else '()]))

(define (collect-tree-il-locals terms source-symbols)
  (let loop ((terms terms) (out '()))
    (if (null? terms)
      (reverse out)
      (loop (cdr terms)
        (append (reverse (collect-tree-il-locals-from-term (car terms) source-symbols))
          out)))))

(define (tree-il-define-kind value)
  (cond
    [(proc? value) "function"]
    [else "variable"]))

(define (tree-il-proc-documentation value)
  (and (proc? value)
    (cond
      [(assq 'documentation (proc-meta value)) => cdr]
      [else #f])))

(define (readable-formal-name identity readable-name)
  (symbol->string (readable-name->symbol identity readable-name)))

(define (tree-il-formals->strings identities readable-names)
  (cond
    [(null? identities) '()]
    [(pair? identities)
      (cons (readable-formal-name (car identities) (car readable-names))
        (tree-il-formals->strings (cdr identities) (cdr readable-names)))]
    [else
      (list "." (readable-formal-name identities readable-names))]))

(define (tree-il-proc-signature name value)
  (and (proc? value)
    (string-append "("
      name
      (let ((params (tree-il-formals->strings (proc-args value) (proc-ids value))))
        (if (null? params)
          ""
          (string-append " " (string-join params " "))))
      ")")))

(define (tree-il-define-detail value)
  (cond
    [(proc? value)
      (let ((name (proc-name value #f)))
        (if name
          (or (tree-il-proc-signature name value)
            (string-append "define " name))
          "define"))]
    [else "define"]))

(define (collect-tree-il-definitions-from-term term)
  (define (collect-list terms)
    (let loop ((terms terms) (out '()))
      (if (null? terms)
        (reverse out)
        (loop (cdr terms)
          (append (reverse (collect-tree-il-definitions-from-term (car terms))) out)))))
  (cond
    [(or (constant? term)
        (void? term)
        (lref? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term))
      '()]
    [(lset? term)
      (collect-tree-il-definitions-from-term (lset-value term))]
    [(module-set? term)
      (let ((value (module-set-value term)))
        (cons
          (let ((name (module-set-name term)))
            (make-symbol-at/doc
              (datum->name-string name)
              (tree-il-define-kind value)
              (if (proc? value)
                (or (tree-il-proc-signature (datum->name-string name) value) "module binding")
                "module binding")
              (sourcev->binding-range (term-src term) name)
              (tree-il-proc-documentation value)))
          (collect-tree-il-definitions-from-term value)))]
    [(toplevel-set? term)
      (let ((value (toplevel-set-value term)))
        (cons
          (let ((name (toplevel-set-name term)))
            (make-symbol-at/doc
              (datum->name-string name)
              (tree-il-define-kind value)
              (if (proc? value)
                (or (tree-il-proc-signature (datum->name-string name) value) "set!")
                "set!")
              (sourcev->binding-range (term-src term) name)
              (tree-il-proc-documentation value)))
          (collect-tree-il-definitions-from-term value)))]
    [(toplevel-define? term)
      (let ((value (toplevel-define-value term)))
        (cons
          (let ((name (toplevel-define-name term)))
            (make-symbol-at/doc
              (datum->name-string name)
              (tree-il-define-kind value)
              (tree-il-define-detail value)
              (sourcev->binding-range (term-src term) name)
              (tree-il-proc-documentation value)))
          (collect-tree-il-definitions-from-term value)))]
    [(if? term)
      (append
        (collect-tree-il-definitions-from-term (if-test term))
        (collect-tree-il-definitions-from-term (if-then term))
        (collect-tree-il-definitions-from-term (if-else term)))]
    [(let? term)
      (append
        (collect-list (let-rhs term))
        (collect-tree-il-definitions-from-term (let-body term)))]
    [(receive? term)
      (append
        (collect-tree-il-definitions-from-term (receive-producer term))
        (collect-tree-il-definitions-from-term (receive-consumer term)))]
    [(fix? term)
      (append
        (collect-list (fix-rhs term))
        (collect-tree-il-definitions-from-term (fix-body term)))]
    [(proc? term)
      (collect-tree-il-definitions-from-term (proc-body term))]
    [(application? term)
      (append
        (collect-tree-il-definitions-from-term (application-operator term))
        (collect-list (application-operands term)))]
    [(primcall? term)
      (collect-list (primcall-args term))]
    [(values? term)
      (collect-list (values-values term))]
    [(sequence? term)
      (append
        (collect-tree-il-definitions-from-term (sequence-head term))
        (collect-tree-il-definitions-from-term (sequence-tail term)))]
    [(wcm? term)
      (append
        (collect-tree-il-definitions-from-term (wcm-mark term))
        (collect-tree-il-definitions-from-term (wcm-result term)))]
    [else '()]))

(define (collect-tree-il-definitions terms)
  (let loop ((terms terms) (out '()))
    (if (null? terms)
      (reverse out)
      (loop (cdr terms)
        (append (reverse (collect-tree-il-definitions-from-term (car terms))) out)))))

(define (collect-tree-il-symbols terms source-symbols)
  (append
    (collect-tree-il-definitions terms)
    (collect-tree-il-locals terms source-symbols)))

(define (symbol->completion symbol)
  (make-completion (cdr (assq 'name symbol))
    (cdr (assq 'kind symbol))
    (cdr (assq 'detail symbol))
    (let ((entry (assq 'documentation symbol)))
      (and entry (string? (cdr entry)) (cdr entry)))))

(define (zero-range? range)
  (and (= (or (range-start range 'line) -1) 0)
    (= (or (range-start range 'character) -1) 0)))

(define (duplicate-definition-diagnostics symbols)
  '())

(define (variable->completion-kind var)
  (guard (exn [else "variable"])
    (let ((value (and (variable-bound? var)
                  (variable-ref var))))
      (cond
        [(procedure? value) "function"]
        [else "variable"]))))

(define (variable->documentation var)
  (guard (exn [else #f])
    (let ((value (and (variable-bound? var)
                  (variable-ref var))))
      (and (procedure? value)
        (let ((documentation (procedure-documentation value)))
          (and (string? documentation)
            documentation))))))

(define (string-prefix? prefix text)
  (let ((prefix-len (string-length prefix))
        (text-len (string-length text)))
    (and (<= prefix-len text-len)
      (string=? prefix (substring text 0 prefix-len)))))

(define (rename-source-name renames name)
  (let loop ((renames renames))
    (cond
      [(null? renames) name]
      [(and (pair? (car renames))
          (symbol? (caar renames))
          (symbol? (cadar renames))
          (string=? name (symbol->string (cadar renames))))
        (symbol->string (caar renames))]
      [else (loop (cdr renames))])))

(define (import-completion-source-name import-spec name)
  (cond
    [(and (pair? import-spec)
        (eq? (car import-spec) 'for)
        (pair? (cdr import-spec)))
      (import-completion-source-name (cadr import-spec) name)]
    [(and (pair? import-spec)
        (eq? (car import-spec) 'library)
        (pair? (cdr import-spec)))
      (import-completion-source-name (cadr import-spec) name)]
    [(and (pair? import-spec)
        (eq? (car import-spec) 'only)
        (pair? (cdr import-spec)))
      (import-completion-source-name (cadr import-spec) name)]
    [(and (pair? import-spec)
        (eq? (car import-spec) 'except)
        (pair? (cdr import-spec)))
      (import-completion-source-name (cadr import-spec) name)]
    [(and (pair? import-spec)
        (eq? (car import-spec) 'prefix)
        (pair? (cdr import-spec))
        (pair? (cddr import-spec))
        (symbol? (caddr import-spec)))
      (let ((prefix (symbol->string (caddr import-spec))))
        (if (string-prefix? prefix name)
          (import-completion-source-name
            (cadr import-spec)
            (substring name (string-length prefix) (string-length name)))
          name))]
    [(and (pair? import-spec)
        (eq? (car import-spec) 'rename)
        (pair? (cdr import-spec)))
      (import-completion-source-name
        (cadr import-spec)
        (rename-source-name (cddr import-spec) name))]
    [else name]))

(define (resolved-import-interface import-spec)
  (resolve-r6rs-interface import-spec))

(define (resolved-interface-module-name iface)
  (let ((name (module-name iface)))
    (and (module-name? name)
      (normalize-module-name name))))

(define (find-variable-origin-module-name module var seen)
  (cond
    [(or (not module) (memq module seen)) #f]
    [(let loop ((entries (module-map (lambda (entry) entry) module)))
        (and (pair? entries)
          (let ((entry (car entries)))
            (or (and (eq? (cdr entry) var)
                 (resolved-interface-module-name module))
              (loop (cdr entries))))))]
    [else
      (let loop ((uses (module-uses module)))
        (and (pair? uses)
          (or (find-variable-origin-module-name (car uses) var (cons module seen))
            (loop (cdr uses)))))]))

(define (variable-origin-module-name iface var)
  (let ((iface-name (resolved-interface-module-name iface)))
    (or (and iface-name
         (guard (exn [else #f])
           (find-variable-origin-module-name
             (resolve-module iface-name #f #f)
             var
             '())))
      iface-name)))

(define (import-completion-detail imported-label origin-label documentation)
  (let* ((origin
           (cond
             [(and origin-label
                 imported-label
                 (not (string=? origin-label imported-label)))
               (string-append "defined in " origin-label
                 "\nre-exported from "
                 imported-label)]
             [origin-label (string-append "defined in " origin-label)]
             [imported-label (string-append "imported from " imported-label)]
             [else "imported binding"]))
         (detail (if documentation
                  (string-append origin "\n" documentation)
                  origin)))
    detail))

(define (import-interface-completions import-spec)
  (guard (exn [else '()])
    (let* ((iface (resolved-import-interface import-spec))
           (module-name (resolved-interface-module-name iface))
           (module-label (and module-name (module-name-string module-name))))
      (module-map
        (lambda (entry)
          (let ((name (car entry))
                (var (cdr entry)))
            (let* ((label (symbol->string name))
                   (origin-name (variable-origin-module-name iface var))
                   (origin-label (and origin-name (module-name-string origin-name)))
                   (file (and origin-name (safe-module-filename origin-name)))
                   (documentation (variable->documentation var))
                   (detail (import-completion-detail
                            module-label
                            origin-label
                            documentation)))
              (make-import-completion label
                (variable->completion-kind var)
                detail
                origin-label
                file
                (import-completion-source-name import-spec label)
                documentation))))
        iface))))

(define (capy-module-ref? term name)
  (and (module-ref? term)
    (equal? (module-ref-module term) '(capy))
    (eq? (module-ref-name term) name)))

(define (resolve-r6rs-interface-call-spec term)
  (and (application? term)
    (capy-module-ref? (application-operator term) 'resolve-r6rs-interface)
    (let ((operands (application-operands term)))
      (and (pair? operands)
        (null? (cdr operands))
        (constant? (car operands))
        (constant-value (car operands))))))

(define (tree-il-contains-lref? term identity)
  (define (contains-list? terms)
    (let loop ((terms terms))
      (and (pair? terms)
        (or (tree-il-contains-lref? (car terms) identity)
          (loop (cdr terms))))))
  (cond
    [(lref? term) (eq? (lref-sym term) identity)]
    [(or (constant? term)
        (void? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term))
      #f]
    [(lset? term)
      (tree-il-contains-lref? (lset-value term) identity)]
    [(module-set? term)
      (tree-il-contains-lref? (module-set-value term) identity)]
    [(toplevel-set? term)
      (tree-il-contains-lref? (toplevel-set-value term) identity)]
    [(toplevel-define? term)
      (tree-il-contains-lref? (toplevel-define-value term) identity)]
    [(if? term)
      (or (tree-il-contains-lref? (if-test term) identity)
        (tree-il-contains-lref? (if-then term) identity)
        (tree-il-contains-lref? (if-else term) identity))]
    [(let? term)
      (or (contains-list? (let-rhs term))
        (tree-il-contains-lref? (let-body term) identity))]
    [(receive? term)
      (or (tree-il-contains-lref? (receive-producer term) identity)
        (tree-il-contains-lref? (receive-consumer term) identity))]
    [(fix? term)
      (or (contains-list? (fix-rhs term))
        (tree-il-contains-lref? (fix-body term) identity))]
    [(proc? term)
      (tree-il-contains-lref? (proc-body term) identity)]
    [(application? term)
      (or (tree-il-contains-lref? (application-operator term) identity)
        (contains-list? (application-operands term)))]
    [(primcall? term)
      (contains-list? (primcall-args term))]
    [(values? term)
      (contains-list? (values-values term))]
    [(sequence? term)
      (or (tree-il-contains-lref? (sequence-head term) identity)
        (tree-il-contains-lref? (sequence-tail term) identity))]
    [(wcm? term)
      (or (tree-il-contains-lref? (wcm-mark term) identity)
        (tree-il-contains-lref? (wcm-result term) identity))]
    [else #f]))

(define (module-use-interfaces-with-id? term identity)
  (define (contains-list? terms)
    (let loop ((terms terms))
      (and (pair? terms)
        (or (module-use-interfaces-with-id? (car terms) identity)
          (loop (cdr terms))))))
  (cond
    [(application? term)
      (let ((operands (application-operands term)))
        (or (and (capy-module-ref? (application-operator term) 'module-use-interfaces!)
             (let loop ((operands operands))
               (and (pair? operands)
                 (or (tree-il-contains-lref? (car operands) identity)
                   (loop (cdr operands))))))
          (module-use-interfaces-with-id? (application-operator term) identity)
          (contains-list? operands)))]
    [(or (constant? term)
        (void? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term)
        (lref? term))
      #f]
    [(lset? term)
      (module-use-interfaces-with-id? (lset-value term) identity)]
    [(module-set? term)
      (module-use-interfaces-with-id? (module-set-value term) identity)]
    [(toplevel-set? term)
      (module-use-interfaces-with-id? (toplevel-set-value term) identity)]
    [(toplevel-define? term)
      (module-use-interfaces-with-id? (toplevel-define-value term) identity)]
    [(if? term)
      (or (module-use-interfaces-with-id? (if-test term) identity)
        (module-use-interfaces-with-id? (if-then term) identity)
        (module-use-interfaces-with-id? (if-else term) identity))]
    [(let? term)
      (or (contains-list? (let-rhs term))
        (module-use-interfaces-with-id? (let-body term) identity))]
    [(receive? term)
      (or (module-use-interfaces-with-id? (receive-producer term) identity)
        (module-use-interfaces-with-id? (receive-consumer term) identity))]
    [(fix? term)
      (or (contains-list? (fix-rhs term))
        (module-use-interfaces-with-id? (fix-body term) identity))]
    [(proc? term)
      (module-use-interfaces-with-id? (proc-body term) identity)]
    [(primcall? term)
      (contains-list? (primcall-args term))]
    [(values? term)
      (contains-list? (values-values term))]
    [(sequence? term)
      (or (module-use-interfaces-with-id? (sequence-head term) identity)
        (module-use-interfaces-with-id? (sequence-tail term) identity))]
    [(wcm? term)
      (or (module-use-interfaces-with-id? (wcm-mark term) identity)
        (module-use-interfaces-with-id? (wcm-result term) identity))]
    [else #f]))

(define (collect-tree-il-import-specs-from-let term)
  (let loop ((ids (let-lhs term)) (rhs (let-rhs term)) (out '()))
    (cond
      [(or (null? ids) (null? rhs)) (reverse out)]
      [else
        (let ((spec (resolve-r6rs-interface-call-spec (car rhs))))
          (loop (cdr ids)
            (cdr rhs)
            (if (and spec
                 (module-use-interfaces-with-id? (let-body term) (car ids)))
              (cons (cons spec (sourcev->range (term-src (car rhs)))) out)
              out)))])))

(define (collect-tree-il-import-specs-from-term term)
  (define (collect-list terms)
    (let loop ((terms terms) (out '()))
      (if (null? terms)
        (reverse out)
        (loop (cdr terms)
          (append (reverse (collect-tree-il-import-specs-from-term (car terms)))
            out)))))
  (cond
    [(or (constant? term)
        (void? term)
        (module-ref? term)
        (toplevel-ref? term)
        (primref? term)
        (lref? term))
      '()]
    [(lset? term)
      (collect-tree-il-import-specs-from-term (lset-value term))]
    [(module-set? term)
      (collect-tree-il-import-specs-from-term (module-set-value term))]
    [(toplevel-set? term)
      (collect-tree-il-import-specs-from-term (toplevel-set-value term))]
    [(toplevel-define? term)
      (collect-tree-il-import-specs-from-term (toplevel-define-value term))]
    [(if? term)
      (append
        (collect-tree-il-import-specs-from-term (if-test term))
        (collect-tree-il-import-specs-from-term (if-then term))
        (collect-tree-il-import-specs-from-term (if-else term)))]
    [(let? term)
      (append
        (collect-tree-il-import-specs-from-let term)
        (collect-list (let-rhs term))
        (collect-tree-il-import-specs-from-term (let-body term)))]
    [(receive? term)
      (append
        (collect-tree-il-import-specs-from-term (receive-producer term))
        (collect-tree-il-import-specs-from-term (receive-consumer term)))]
    [(fix? term)
      (append
        (collect-list (fix-rhs term))
        (collect-tree-il-import-specs-from-term (fix-body term)))]
    [(proc? term)
      (collect-tree-il-import-specs-from-term (proc-body term))]
    [(application? term)
      (append
        (collect-tree-il-import-specs-from-term (application-operator term))
        (collect-list (application-operands term)))]
    [(primcall? term)
      (collect-list (primcall-args term))]
    [(values? term)
      (collect-list (values-values term))]
    [(sequence? term)
      (append
        (collect-tree-il-import-specs-from-term (sequence-head term))
        (collect-tree-il-import-specs-from-term (sequence-tail term)))]
    [(wcm? term)
      (append
        (collect-tree-il-import-specs-from-term (wcm-mark term))
        (collect-tree-il-import-specs-from-term (wcm-result term)))]
    [else '()]))

(define (collect-tree-il-import-specs terms)
  (let loop ((terms terms) (out '()))
    (if (null? terms)
      (reverse out)
      (loop (cdr terms)
        (append (reverse (collect-tree-il-import-specs-from-term (car terms))) out)))))

(define (collect-tree-il-import-completions import-specs)
  (let loop ((import-specs import-specs) (out '()))
    (if (null? import-specs)
      (reverse out)
      (loop (cdr import-specs)
        (append (reverse (import-interface-completions (caar import-specs))) out)))))

(define (completion-import-spec spec)
  (cond
    [(and (pair? spec)
        (eq? (car spec) 'for)
        (pair? (cdr spec)))
      (cadr spec)]
    [else spec]))

(define (completion-key completion)
  (cons (cdr (assq 'label completion))
    (cdr (assq 'kind completion))))

(define (dedupe-completions completions)
  (let loop ((items completions) (seen '()) (out '()))
    (cond
      [(null? items) (reverse out)]
      [else
        (let ((key (completion-key (car items))))
          (if (member key seen)
            (loop (cdr items) seen out)
            (loop (cdr items)
              (cons key seen)
              (cons (car items) out))))])))

(define (module-name? value)
  (and (list? value)
    (pair? value)
    (or (module-name-components? value)
      (versioned-module-name? value))))

(define (import-wrapper? name)
  (memq name '(only except prefix rename for library)))

(define (import-spec-module-name spec)
  (cond
    [(and (pair? spec)
        (symbol? (car spec))
        (import-wrapper? (car spec))
        (pair? (cdr spec)))
      (import-spec-module-name (cadr spec))]
    [(module-name? spec)
      (normalize-module-name spec)]
    [else #f]))

(define (make-import name range resolved error)
  (let ((file (safe-module-filename name)))
    `((name . ,(module-name-string name))
      (range . ,range)
      (resolved . ,resolved)
      (error . ,(json-nullable-string error))
      (file . ,(json-nullable-string file)))))

(define (import-spec-resolution spec)
  (let ((fallback-name (import-spec-module-name spec)))
    (guard (exn
            [else
              (values fallback-name #f (condition->message exn))])
      (let* ((iface (resolved-import-interface spec))
             (resolved-name (resolved-interface-module-name iface)))
        (values (or resolved-name fallback-name) #t #f)))))

(define (tree-il-imports import-specs)
  (let loop ((import-specs import-specs) (out '()))
    (cond
      [(null? import-specs) (reverse out)]
      [else
        (let ((spec (caar import-specs))
              (range (cdar import-specs)))
          (call-with-values
            (lambda () (import-spec-resolution spec))
            (lambda (name resolved error)
              (loop (cdr import-specs)
                (if name
                  (cons (make-import name range resolved error)
                    out)
                  out)))))])))

(define (pretty-objects objects)
  (let ((port (open-output-string)))
    (let loop ((objects objects) (first? #t))
      (cond
        [(null? objects) (get-output-string port)]
        [else
          (unless first?
            (newline port))
          (pretty-print (car objects) port)
          (loop (cdr objects) #f)]))))

(define (action-title action)
  (cond
    [(assoc action action-definitions) => cdr]
    [else action]))

(define (run-document-action uri text version path action range)
  (set! current-analysis-path path)
  (set! current-analysis-text text)
  (let ((action-text (or (range->text text range) text)))
    (call-with-values
      (lambda () (read-forms action-text))
    (lambda (forms read-diagnostics)
      (if (pair? read-diagnostics)
        `((title . ,(action-title action))
          (language . "scheme")
          (content . ,(string-append "read failed: "
                       (let ((diagnostic (car read-diagnostics)))
                         (cond
                           [(and (list? diagnostic)
                               (assq 'message diagnostic))
                             =>
                             cdr]
                           [else "invalid syntax"])))))
          (if (null? forms)
            `((title . ,(action-title action))
              (language . "scheme")
              (content . "no form found for action range\n"))
            (call-with-values
              (lambda () (expand-forms forms))
              (lambda (expanded-forms expand-diagnostics)
                (if (pair? expand-diagnostics)
                  `((title . ,(action-title action))
                    (language . "scheme")
                    (content . ,(string-append "expand failed: "
                                 (let ((diagnostic (car expand-diagnostics)))
                                   (cond
                                     [(and (list? diagnostic)
                                         (assq 'message diagnostic))
                                       =>
                                       cdr]
                                     [else "macroexpand failed"])))))
                  (let ((content
                          (pretty-objects
                            (map (lambda (term)
                                  (tree-il->scheme
                                    term
                                    '(denoise-lexicals? use-case?)))
                              expanded-forms))))
                    `((title . ,(action-title action))
                      (language . "scheme")
                      (content . ,content)))))))))))))

(define (analyze-document uri text version . maybe-path)
  (set! current-analysis-path
    (and (pair? maybe-path)
      (string? (car maybe-path))
      (car maybe-path)))
  (set! current-analysis-text text)
  (call-with-values
    (lambda () (read-forms text))
    (lambda (forms read-diagnostics)
      (let ((datums (normalize-forms forms)))
        (call-with-values
          (lambda ()
            (if (null? read-diagnostics)
              (expand-forms (expansion-forms forms datums))
              (values '() '())))
          (lambda (expanded-forms expand-diagnostics)
            (let* (
                   (diagnostics (append (scan-syntax text) read-diagnostics expand-diagnostics))
                   (source-symbols #f)
                   (symbols (collect-tree-il-symbols expanded-forms source-symbols))
                   (references (collect-tree-il-references uri expanded-forms source-symbols))
                   (call-graph (collect-call-graph uri expanded-forms source-symbols))
                   (import-specs (collect-tree-il-import-specs expanded-forms))
                   (semantic-diagnostics (duplicate-definition-diagnostics symbols))
                   (all-diagnostics (append diagnostics semantic-diagnostics))
                   (completions (dedupe-completions
                                 (append (map symbol->completion symbols)
                                   (collect-tree-il-import-completions import-specs))))
                   (imports (tree-il-imports import-specs)))
              `((uri . ,(json-nullable-string uri))
                (version . ,(json-nullable-version version))
                (engine . "macroexpand")
                (treeIl . ((available . ,(null? expand-diagnostics))
                           (formCount . ,(length expanded-forms))
                           (reason . ,(if (null? expand-diagnostics)
                                       "macroexpand completed"
                                       "macroexpand failed"))))
                (diagnostics . ,(list->vector all-diagnostics))
                (symbols . ,(list->vector symbols))
                (references . ,(list->vector references))
                (callGraph . ,call-graph)
                (completions . ,(list->vector completions))
                (imports . ,(list->vector imports))
                (actions . ,(list->vector '()))))))))))
