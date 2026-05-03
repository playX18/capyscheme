#!r6rs

(library (capy lsp analysis)
  (export analyze-document run-document-action invalidate-files)
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
  (define file->modules '())
  (define module->imports '())
  (define module->file '())
  (define module-name-cache '())

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

  (define (range-start range key)
    (let ((start (and (list? range) (assq 'start range))))
      (and start
           (let ((entry (and (list? (cdr start)) (assq key (cdr start)))))
             (and entry (number? (cdr entry)) (cdr entry))))))

  (define (same-range-start? sourcev range)
    (and range
         (let ((line (sourcev->line sourcev))
               (character (sourcev->character sourcev)))
           (let ((range-line (range-start range 'line))
                 (range-character (range-start range 'character)))
             (and line
                  character
                  range-line
                  range-character
                  (= line range-line)
                  (= character range-character)))))))

  (define (make-action action title range)
    `((action . ,action)
      (title . ,title)
      (range . ,range)))

  (define (document-actions forms)
    (let loop ((forms forms) (out '()))
      (cond
        [(null? forms) (reverse out)]
        [else
         (let* ((sourcev (and (syntax? (car forms)) (syntax-sourcev (car forms))))
                (range (sourcev->range sourcev))
                (actions (map (lambda (definition)
                                (make-action (car definition) (cdr definition) range))
                              action-definitions)))
           (loop (cdr forms) (append (reverse actions) out)))])))

  (define (position->index text target-line target-character)
    (let ((len (string-length text)))
      (let loop ((i 0) (line 0) (character 0))
        (cond
          [(>= i len) len]
          [(and (= line target-line) (= character target-character)) i]
          [else
           (let ((ch (string-ref text i)))
             (if (char=? ch #\newline)
                 (loop (+ i 1) (+ line 1) 0)
                 (loop (+ i 1) line (+ character 1))))]))))

  (define (index->position text target-index)
    (let ((len (string-length text)))
      (let loop ((i 0) (line 0) (character 0))
        (cond
          [(or (>= i len) (>= i target-index)) (make-position line character)]
          [else
           (let ((ch (string-ref text i)))
             (if (char=? ch #\newline)
                 (loop (+ i 1) (+ line 1) 0)
                 (loop (+ i 1) line (+ character 1))))]))))

  (define (identifier-char? ch)
    (not (delimiter? ch)))

  (define (identifier-boundary? text i)
    (or (< i 0)
        (>= i (string-length text))
        (not (identifier-char? (string-ref text i)))))

  (define (identifier-at? text i name)
    (let ((name-len (string-length name))
          (text-len (string-length text)))
      (and (<= (+ i name-len) text-len)
           (string=? (substring text i (+ i name-len)) name)
           (identifier-boundary? text (- i 1))
           (identifier-boundary? text (+ i name-len)))))

  (define (range-for-text-indices text start end)
    `((start . ,(index->position text start))
      (end . ,(index->position text end))))

  (define (sourcev->binding-range sourcev name)
    (let ((fallback (sourcev->range sourcev)))
      (if (and (vector? sourcev)
               (>= (vector-length sourcev) 3)
               (number? (vector-ref sourcev 1))
               (number? (vector-ref sourcev 2)))
          (let* ((text current-analysis-text)
                 (name-string (symbol->string name))
                 (start (position->index text
                                         (max 0 (- (vector-ref sourcev 1) 1))
                                         (vector-ref sourcev 2)))
                 (len (string-length text)))
            (let loop ((i start))
              (cond
                [(>= i len) fallback]
                [(identifier-at? text i name-string)
                 (range-for-text-indices text i (+ i (string-length name-string)))]
                [else (loop (+ i 1))])))
          fallback)))

  (define (make-diagnostic severity code message line character)
    `((source . "capy-lsp")
      (severity . ,severity)
      (code . ,code)
      (message . ,message)
      (range . ,(make-range line character))))

  (define (syntax-error message line character)
    (make-diagnostic "error" "syntax" message line character))

  (define (read-error-diagnostic exn)
    (make-diagnostic
      "error"
      "read"
      (string-append "read failed: " (condition->message exn))
      0
      0))

  (define (expand-error-diagnostic exn)
    (make-diagnostic
      "error"
      "expand"
      (string-append "expand failed: " (condition->message exn))
      0
      0))

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
          (current-module (make-fresh-user-module))
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

  (define (assoc-delete-all key alist)
    (let loop ((alist alist) (out '()))
      (cond
        [(null? alist) (reverse out)]
        [(equal? (caar alist) key) (loop (cdr alist) out)]
        [else (loop (cdr alist) (cons (car alist) out))])))

  (define (module-name-key name)
    (module-name-string name))

  (define (remember-module-name! name)
    (let ((key (module-name-key name)))
      (set! module-name-cache
            (cons (cons key name) (assoc-delete-all key module-name-cache)))
      key))

  (define (current-file-modules path)
    (cond
      [(assoc path file->modules) => cdr]
      [else '()]))

  (define (remember-module-file! path name)
    (let ((key (remember-module-name! name)))
      (set! module->file
            (cons (cons key path) (assoc-delete-all key module->file)))))

  (define (remember-file-modules! path modules)
    (when (string? path)
      (set! file->modules
            (cons (cons path modules) (assoc-delete-all path file->modules)))
      (for-each
        (lambda (name)
          (remember-module-file! path name))
        modules)))

  (define (remember-file-module! path name)
    (when (string? path)
      (let ((modules (current-file-modules path)))
        (remember-file-modules!
          path
          (if (member name modules) modules (cons name modules))))))

  (define (remember-import-files! imports)
    (for-each
      (lambda (name)
        (let ((path (safe-module-filename name)))
          (when path
            (remember-file-module! path name))))
      imports))

  (define (remember-module-imports! modules imports)
    (let ((import-keys (map remember-module-name! imports)))
      (for-each
        (lambda (name)
          (let ((key (remember-module-name! name)))
            (set! module->imports
                  (cons (cons key import-keys)
                        (assoc-delete-all key module->imports)))))
        modules)))

  (define (forget-module! key)
    (set! module->imports (assoc-delete-all key module->imports))
    (set! module->file (assoc-delete-all key module->file))
    (set! module-name-cache (assoc-delete-all key module-name-cache)))

  (define (forget-file! path)
    (set! file->modules (assoc-delete-all path file->modules)))

  (define (dedupe-equal values)
    (let loop ((values values) (out '()))
      (cond
        [(null? values) (reverse out)]
        [(member (car values) out) (loop (cdr values) out)]
        [else (loop (cdr values) (cons (car values) out))])))

  (define (declared-module-name form)
    (cond
      [(and (pair? form)
            (or (eq? (car form) 'library)
                (eq? (car form) 'define-library))
            (pair? (cdr form))
            (module-name? (cadr form)))
       (normalize-module-name (cadr form))]
      [else #f]))

  (define (collect-declared-modules forms)
    (dedupe-equal
      (let loop ((forms forms) (out '()))
        (cond
          [(null? forms) out]
          [else
           (let ((name (declared-module-name (car forms))))
             (loop (cdr forms) (if name (cons name out) out)))]))))

  (define (collect-import-module-names forms)
    (dedupe-equal
      (letrec ((define-library-imports
                 (lambda (clauses)
                   (let loop ((clauses clauses) (out '()))
                     (cond
                       [(null? clauses) out]
                       [(and (pair? (car clauses)) (eq? (caar clauses) 'import))
                        (loop (cdr clauses)
                              (append (form-imports (car clauses)) out))]
                       [(and (pair? (car clauses)) (eq? (caar clauses) 'begin))
                        (loop (cdr clauses)
                              (append (collect (cdar clauses)) out))]
                       [else (loop (cdr clauses) out)]))))
               (form-imports
                 (lambda (form)
                   (cond
                     [(and (pair? form) (eq? (car form) 'import))
                      (let loop ((specs (cdr form)) (out '()))
                        (cond
                          [(null? specs) out]
                          [else
                           (let ((name (import-spec-module-name (car specs))))
                             (loop (cdr specs)
                                   (if name (cons name out) out)))]))]
                     [(and (pair? form) (eq? (car form) 'library))
                      (collect (cddr form))]
                     [(and (pair? form) (eq? (car form) 'define-library))
                      (define-library-imports (cddr form))]
                     [(and (pair? form) (eq? (car form) 'begin))
                      (collect (cdr form))]
                     [else '()])))
               (collect
                 (lambda (forms)
                   (let loop ((forms forms) (out '()))
                     (if (null? forms)
                         out
                         (loop (cdr forms)
                               (append (form-imports (car forms)) out)))))))
        (collect forms))))

  (define (record-analysis! path datums)
    (when (string? path)
      (let ((modules (collect-declared-modules datums))
            (imports (collect-import-module-names datums)))
        (remember-file-modules! path modules)
        (remember-import-files! imports)
        (remember-module-imports! modules imports))))

  (define (read-file-string path)
    (guard (exn [else #f])
      (call-with-input-file path
        (lambda (port)
          (get-string-all port)))))

  (define (override-text path overrides)
    (cond
      [(assoc path overrides) => cdr]
      [else #f]))

  (define (changed-file-modules path overrides)
    (let ((text (or (override-text path overrides)
                    (read-file-string path))))
      (if (string? text)
          (call-with-values
            (lambda () (read-forms text))
            (lambda (forms read-diagnostics)
              (if (null? read-diagnostics)
                  (let* ((datums (normalize-forms forms))
                         (modules (collect-declared-modules datums))
                         (imports (collect-import-module-names datums)))
                    (values modules imports #t))
                  (values '() '() #f))))
          (values '() '() #f))))

  (define (known-file-modules path)
    (current-file-modules path))

  (define (module-key->name key)
    (cond
      [(assoc key module-name-cache) => cdr]
      [else #f]))

  (define (module-imports-key? module-key imported-key)
    (cond
      [(assoc module-key module->imports)
       => (lambda (entry) (member imported-key (cdr entry)))]
      [else #f]))

  (define (direct-dependents keys)
    (let loop ((entries module->imports) (out '()))
      (cond
        [(null? entries) (dedupe-equal out)]
        [(let scan ((keys keys))
           (and (pair? keys)
                (or (module-imports-key? (caar entries) (car keys))
                    (scan (cdr keys)))))
         (loop (cdr entries) (cons (caar entries) out))]
        [else (loop (cdr entries) out)])))

  (define (dependent-closure initial-keys)
    (let loop ((pending initial-keys) (seen initial-keys))
      (if (null? pending)
          seen
          (let ((next (direct-dependents (list (car pending)))))
            (let add ((items next) (pending (cdr pending)) (seen seen))
              (cond
                [(null? items) (loop pending seen)]
                [(member (car items) seen) (add (cdr items) pending seen)]
                [else (add (cdr items)
                           (cons (car items) pending)
                           (cons (car items) seen))]))))))

  (define (invalidate-module-key! key)
    (let ((name (module-key->name key)))
      (values name (and name (invalidate-module! name)))))

  (define (invalidate-files paths overrides)
    (let loop ((paths paths) (direct-names '()) (new-records '()))
      (if (null? paths)
          (let* ((direct-keys (dedupe-equal (map remember-module-name! direct-names)))
                 (all-keys (dependent-closure direct-keys))
                 (invalidation
                   (let invalidate ((keys all-keys) (out '()) (restart? #f))
                     (cond
                       [(null? keys) (cons out restart?)]
                       [else
                        (call-with-values
                          (lambda () (invalidate-module-key! (car keys)))
                          (lambda (name loaded?)
                            (forget-module! (car keys))
                            (invalidate (cdr keys)
                                        (if name (cons name out) out)
                                        (or restart? loaded?))))])))
                 (invalidated-names (car invalidation))
                 (restart-required? (cdr invalidation)))
            (for-each
              (lambda (record)
                (let ((path (car record))
                      (modules (cadr record))
                      (imports (caddr record)))
                  (forget-file! path)
                  (remember-file-modules! path modules)
                  (remember-import-files! imports)
                  (remember-module-imports! modules imports)))
              new-records)
            `((invalidatedModules
                . ,(list->vector (map module-name-string
                                      (dedupe-equal invalidated-names))))
              (restartRequired . ,restart-required?)))
          (let* ((path (car paths))
                 (old-modules (known-file-modules path)))
            (call-with-values
              (lambda () (changed-file-modules path overrides))
              (lambda (new-modules new-imports parsed?)
                (let ((all-direct (append old-modules new-modules direct-names))
                      (records (if parsed?
                                   (cons (list path new-modules new-imports) new-records)
                                   new-records)))
                  (loop (cdr paths) all-direct records))))))))

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
              => (lambda (dir) (string-append dir "/" file))]
             [(and (string? current-analysis-path)
                   (lsp-dirname current-analysis-path))
              => (lambda (dir) (string-append dir "/" file))]
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
      (string-append "srfi-"
                     (cond
                       [(number? n) (number->string n)]
                       [(symbol? n)
                        (let ((colon-number (colon-srfi-number n)))
                          (if colon-number
                              (number->string colon-number)
                              (symbol->string n)))]
                       [else (datum->name-string n)]))))

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
    `((name . ,name)
      (kind . ,kind)
      (detail . ,detail)
      (range . ,range)
      (selectionRange . ,range)))

  (define (make-completion name kind detail)
    `((label . ,name)
      (kind . ,kind)
      (detail . ,detail)))

  (define (make-location uri range)
    `((uri . ,(json-nullable-string uri))
      (range . ,range)))

  (define (make-reference uri name range definition-range)
    `((name . ,name)
      (uri . ,(json-nullable-string uri))
      (range . ,range)
      (definition . ,(make-location uri definition-range))))

  (define (make-import-completion name kind detail module-name file source-name)
    `((label . ,name)
      (kind . ,kind)
      (detail . ,detail)
      (sourceModule . ,(json-nullable-string module-name))
      (sourceFile . ,(json-nullable-string file))
      (sourceName . ,(json-nullable-string source-name))))

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

  (define (source-symbols datum)
    (cond
      [(symbol? datum) (list datum)]
      [(pair? datum)
       (let loop ((xs datum) (out '()))
         (cond
           [(null? xs) out]
           [(pair? xs)
            (loop (cdr xs) (append (source-symbols (car xs)) out))]
           [else (append (source-symbols xs) out)]))]
      [(vector? datum)
       (let loop ((i 0) (out '()))
         (if (= i (vector-length datum))
             out
             (loop (+ i 1) (append (source-symbols (vector-ref datum i)) out))))]
      [else '()]))

  (define (collect-source-symbols forms)
    (let loop ((forms forms) (out '()))
      (if (null? forms)
          out
          (loop (cdr forms) (append (source-symbols (car forms)) out)))))

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

  (define (collect-one-local identity readable-name detail source-symbols sourcev)
    (let ((name (readable-name->symbol identity readable-name)))
      (if (memq name source-symbols)
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
         (if (memq name source-symbols)
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
              (if (memq name source-symbols)
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
         (collect-binding-locals (let-lhs term)
                                 (let-ids term)
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
         (collect-binding-locals (fix-lhs term)
                                 (fix-ids term)
                                 "fix binding"
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

  (define (symbol->completion symbol)
    (make-completion (cdr (assq 'name symbol))
                     (cdr (assq 'kind symbol))
                     (cdr (assq 'detail symbol))))

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

  (define (import-interface-completions import-spec)
    (guard (exn [else '()])
      (let* ((module-name (import-spec-module-name import-spec))
             (module-label (and module-name (module-name-string module-name)))
             (file (and module-name (safe-module-filename module-name)))
             (iface (resolve-r6rs-interface import-spec)))
        (module-map
          (lambda (entry)
            (let ((name (car entry))
                  (var (cdr entry)))
              (let* ((label (symbol->string name))
                     (origin (if module-label
                                 (string-append "imported from " module-label)
                                 "imported binding"))
                     (documentation (variable->documentation var))
                     (detail (if documentation
                                 (string-append origin "\n" documentation)
                                 origin)))
                (make-import-completion label
                                        (variable->completion-kind var)
                                        detail
                                        module-label
                                        file
                                        (import-completion-source-name import-spec label)))))
          iface))))

  (define (completion-import-spec spec)
    (cond
      [(and (pair? spec)
            (eq? (car spec) 'for)
            (pair? (cdr spec)))
       (cadr spec)]
      [else spec]))

  (define (form-import-completions form)
    (cond
      [(and (pair? form) (eq? (car form) 'import))
       (let loop ((specs (cdr form)) (out '()))
         (cond
           [(null? specs) (reverse out)]
           [else
            (loop (cdr specs)
                  (append (reverse (import-interface-completions
                                     (completion-import-spec (car specs))))
                          out))]))]
      [(and (pair? form) (eq? (car form) 'library))
       (collect-import-completions (cddr form))]
      [(and (pair? form) (eq? (car form) 'define-library))
       (collect-define-library-import-completions (cddr form))]
      [(and (pair? form) (eq? (car form) 'begin))
       (collect-import-completions (cdr form))]
      [else '()]))

  (define (collect-define-library-import-completions clauses)
    (let loop ((clauses clauses) (out '()))
      (cond
        [(null? clauses) (reverse out)]
        [(and (pair? (car clauses)) (eq? (caar clauses) 'import))
         (loop (cdr clauses)
               (append (reverse (form-import-completions (car clauses))) out))]
        [(and (pair? (car clauses)) (eq? (caar clauses) 'begin))
         (loop (cdr clauses)
               (append (reverse (collect-import-completions (cdar clauses))) out))]
        [else (loop (cdr clauses) out)])))

  (define (collect-import-completions forms)
    (let loop ((forms forms) (out '()))
      (if (null? forms)
          (reverse out)
          (loop (cdr forms)
                (append (reverse (form-import-completions (car forms))) out)))))

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

  (define (formal-names formals)
    (cond
      [(null? formals) '()]
      [(pair? formals)
       (cons (datum->name-string (car formals))
             (formal-names (cdr formals)))]
      [else (list "." (datum->name-string formals))]))

  (define (define-head-signature head)
    (and (pair? head)
         (symbol? (car head))
         (let ((parts (cons (symbol->string (car head))
                            (formal-names (cdr head)))))
           (string-append "(" (string-join parts " ") ")"))))

  (define (definition-symbol form)
    (cond
      [(and (pair? form) (eq? (car form) 'define) (pair? (cdr form)))
       (let ((head (cadr form)))
         (cond
           [(symbol? head)
            (list (make-symbol (symbol->string head) "variable" "define"))]
           [(and (pair? head) (symbol? (car head)))
            (list (make-symbol (symbol->string (car head))
                               "function"
                               (or (define-head-signature head) "define")))]
           [else '()]))]
      [(and (pair? form) (eq? (car form) 'define-syntax) (pair? (cdr form)) (symbol? (cadr form)))
       (list (make-symbol (symbol->string (cadr form)) "macro" "define-syntax"))]
      [(and (pair? form) (eq? (car form) 'define-record-type) (pair? (cdr form)))
       (let ((name (cadr form)))
         (cond
           [(symbol? name)
            (list (make-symbol (symbol->string name) "variable" "define-record-type"))]
           [(and (pair? name) (symbol? (car name)))
            (list (make-symbol (symbol->string (car name)) "variable" "define-record-type"))]
           [else '()]))]
      [(and (pair? form) (eq? (car form) 'define-values) (pair? (cdr form)) (list? (cadr form)))
       (map (lambda (name)
              (make-symbol (datum->name-string name) "variable" "define-values"))
            (cadr form))]
      [(and (pair? form) (eq? (car form) 'library) (pair? (cdr form)))
       (cons (make-symbol (module-name-string (cadr form)) "module" "library")
             (collect-symbols (cddr form)))]
      [(and (pair? form) (eq? (car form) 'define-library) (pair? (cdr form)))
       (cons (make-symbol (module-name-string (cadr form)) "module" "define-library")
             (collect-define-library-symbols (cddr form)))]
      [(and (pair? form) (eq? (car form) 'begin))
       (collect-symbols (cdr form))]
      [else '()]))

  (define (collect-define-library-symbols clauses)
    (let loop ((clauses clauses) (out '()))
      (cond
        [(null? clauses) (reverse out)]
        [(and (pair? (car clauses))
              (eq? (caar clauses) 'begin))
         (loop (cdr clauses)
               (append (reverse (collect-symbols (cdar clauses))) out))]
        [else (loop (cdr clauses) out)])))

  (define (collect-symbols forms)
    (let loop ((forms forms) (out '()))
      (if (null? forms)
          (reverse out)
          (loop (cdr forms) (append (reverse (definition-symbol (car forms))) out)))))

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

  (define (make-import name)
    (let ((file (safe-module-filename name)))
      `((name . ,(module-name-string name))
        (file . ,(json-nullable-string file)))))

  (define (form-imports form)
    (cond
      [(and (pair? form) (eq? (car form) 'import))
       (let loop ((specs (cdr form)) (out '()))
         (cond
           [(null? specs) (reverse out)]
           [else
            (let ((name (import-spec-module-name (car specs))))
              (loop (cdr specs)
                    (if name (cons (make-import name) out) out)))]))]
      [(and (pair? form) (eq? (car form) 'library))
       (collect-imports (cddr form))]
      [(and (pair? form) (eq? (car form) 'define-library))
       (collect-define-library-imports (cddr form))]
      [(and (pair? form) (eq? (car form) 'begin))
       (collect-imports (cdr form))]
      [else '()]))

  (define (collect-define-library-imports clauses)
    (let loop ((clauses clauses) (out '()))
      (cond
        [(null? clauses) (reverse out)]
        [(and (pair? (car clauses)) (eq? (caar clauses) 'import))
         (loop (cdr clauses)
               (append (reverse (form-imports (car clauses))) out))]
        [(and (pair? (car clauses)) (eq? (caar clauses) 'begin))
         (loop (cdr clauses)
               (append (reverse (collect-imports (cdar clauses))) out))]
        [else (loop (cdr clauses) out)])))

  (define (collect-imports forms)
    (let loop ((forms forms) (out '()))
      (if (null? forms)
          (reverse out)
          (loop (cdr forms) (append (reverse (form-imports (car forms))) out)))))

  (define (selected-action-forms forms range)
    (if (not range)
        forms
        (let loop ((forms forms) (out '()))
          (cond
            [(null? forms) (reverse out)]
            [(and (syntax? (car forms))
                  (same-range-start? (syntax-sourcev (car forms)) range))
             (loop (cdr forms) (cons (car forms) out))]
            [else (loop (cdr forms) out)]))))

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
    (call-with-values
      (lambda () (read-forms text))
      (lambda (forms read-diagnostics)
        (if (pair? read-diagnostics)
            `((title . ,(action-title action))
              (language . "scheme")
              (content . ,(string-append "read failed: "
                                         (let ((diagnostic (car read-diagnostics)))
                                           (cond
                                             [(and (list? diagnostic)
                                                   (assq 'message diagnostic))
                                              => cdr]
                                             [else "invalid syntax"])))))
            (let ((selected (selected-action-forms forms range)))
              (if (null? selected)
                  `((title . ,(action-title action))
                    (language . "scheme")
                    (content . "no form found for action range\n"))
                  (call-with-values
                    (lambda () (expand-forms selected))
                    (lambda (expanded-forms expand-diagnostics)
                      (if (pair? expand-diagnostics)
                          `((title . ,(action-title action))
                            (language . "scheme")
                            (content . ,(string-append "expand failed: "
                                                       (let ((diagnostic (car expand-diagnostics)))
                                                         (cond
                                                           [(and (list? diagnostic)
                                                                 (assq 'message diagnostic))
                                                            => cdr]
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
                              (content . ,content))))))))))))

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
          (record-analysis! current-analysis-path datums)
          (call-with-values
            (lambda ()
              (if (null? read-diagnostics)
                  (expand-forms (expansion-forms forms datums))
                  (values '() '())))
            (lambda (expanded-forms expand-diagnostics)
              (let* (
                   (diagnostics (append (scan-syntax text) read-diagnostics expand-diagnostics))
                   (source-symbols (collect-source-symbols datums))
                   (symbols (append (collect-symbols datums)
                                    (collect-tree-il-locals expanded-forms source-symbols)))
                   (references (collect-tree-il-references uri expanded-forms source-symbols))
                   (completions (dedupe-completions
                                  (append (map symbol->completion symbols)
                                          (collect-import-completions datums))))
                   (imports (collect-imports datums)))
              `((uri . ,(json-nullable-string uri))
                (version . ,(json-nullable-version version))
                (engine . "macroexpand")
                (treeIl . ((available . ,(null? expand-diagnostics))
                           (formCount . ,(length expanded-forms))
                           (reason . ,(if (null? expand-diagnostics)
                                          "macroexpand completed"
                                          "macroexpand failed"))))
                (diagnostics . ,(list->vector diagnostics))
                (symbols . ,(list->vector symbols))
                (references . ,(list->vector references))
                (completions . ,(list->vector completions))
                (imports . ,(list->vector imports))
                (actions . ,(list->vector (document-actions forms)))))))))))
