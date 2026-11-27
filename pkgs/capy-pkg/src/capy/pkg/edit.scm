(library (capy pkg edit)
  (export dependency-text
    add-dependency-text
    remove-dependency-text
    set-dependency-version-text
    manifest-dependency-names
    inherited-dependency-names
    add-member-text)
  (import (rnrs)
    (capy pkg ui))

  ;;; Scanning

  (define (scan-error)
    (fail "package.scm: cannot find the (package ...) form to edit"))

  (define (workspace-scan-error)
    (fail "package.scm: cannot find the (workspace ...) form to edit"))

  (define (delimiter? c)
    (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\" #\;))))

  ;; The index of the first thing after INDEX that is not a blank or a comment.
  (define (skip-blank text i)
    (let ([n (string-length text)])
      (let loop ([i i])
        (cond
          [(>= i n) i]
          [(char-whitespace? (string-ref text i)) (loop (+ i 1))]
          [(char=? (string-ref text i) #\;)
            (let line ([i i])
              (cond [(>= i n) i]
                [(char=? (string-ref text i) #\newline) (loop (+ i 1))]
                [else (line (+ i 1))]))]
          [(and (< (+ i 1) n) (char=? (string-ref text i) #\#) (char=? (string-ref text (+ i 1)) #\|))
            (let block ([i (+ i 2)] [depth 1])
              (cond
                [(>= (+ i 1) n) n]
                [(and (char=? (string-ref text i) #\|) (char=? (string-ref text (+ i 1)) #\#))
                  (if (= depth 1) (loop (+ i 2)) (block (+ i 2) (- depth 1)))]
                [(and (char=? (string-ref text i) #\#) (char=? (string-ref text (+ i 1)) #\|))
                  (block (+ i 2) (+ depth 1))]
                [else (block (+ i 1) depth)]))]
          [else i]))))

  ;; The index after the datum that starts at I.
  (define (datum-end text i)
    (let ([n (string-length text)] [c (string-ref text i)])
      (cond
        [(memv c '(#\( #\[))
          (let-values ([(children close) (list-children text i)])
            (+ close 1))]
        [(char=? c #\")
          (let loop ([j (+ i 1)])
            (cond [(>= j n) (scan-error)]
              [(char=? (string-ref text j) #\\) (loop (+ j 2))]
              [(char=? (string-ref text j) #\") (+ j 1)]
              [else (loop (+ j 1))]))]
        [(and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\\))
          ;; #\x, #\space: the character after the backslash, then the letters that follow
          (let loop ([j (min n (+ i 3))])
            (if (and (< j n) (not (delimiter? (string-ref text j)))) (loop (+ j 1)) j))]
        [(and (char=? c #\#) (< (+ i 1) n) (memv (string-ref text (+ i 1)) '(#\( #\[)))
          (datum-end text (+ i 1))]
        [(memv c '(#\' #\` #\,))
          (datum-end text (skip-blank text (+ i 1)))]
        [else
          (let loop ([j (+ i 1)])
            (if (and (< j n) (not (delimiter? (string-ref text j)))) (loop (+ j 1)) j))])))

  ;; The data inside the list that starts with a parenthesis at I, as (start . end) pairs, and
  ;; the index of the closing parenthesis.
  (define (list-children text i)
    (let ([n (string-length text)])
      (let loop ([j (skip-blank text (+ i 1))] [out '()])
        (cond
          [(>= j n) (scan-error)]
          [(memv (string-ref text j) '(#\) #\])) (values (reverse out) j)]
          [else
            (let ([end (datum-end text j)])
              (loop (skip-blank text end) (cons (cons j end) out)))]))))

  (define (child-text text child) (substring text (car child) (cdr child)))

  ;; The (start . end) of the (package ...) form.
  (define (package-form text)
    (let ([i (skip-blank text 0)])
      (unless (and (< i (string-length text)) (char=? (string-ref text i) #\()) (scan-error))
      (cons i (datum-end text i))))

  (define (list-form? text child)
    (char=? (string-ref text (car child)) #\())

  ;; The first element of the list CHILD as text, or #f.
  (define (head-text text child)
    (and (list-form? text child)
      (let-values ([(children close) (list-children text (car child))])
        (and (pair? children) (child-text text (car children))))))

  ;; The (start . end) of the form whose fields an edit of SCOPE changes: the package (the top
  ;; form), or the workspace.
  (define (container text scope)
    (let ([top (package-form text)])
      (cond
        [(eq? scope 'package) top]
        [(equal? (head-text text top) "workspace") top]
        [(find-field-in text top "workspace") => values]
        [else (workspace-scan-error)])))

  (define (find-field-in text form key)
    (let-values ([(fields close) (list-children text (car form))])
      (let loop ([fields (if (pair? fields) (cdr fields) '())])
        (cond
          [(null? fields) #f]
          [(equal? (head-text text (car fields)) key) (car fields)]
          [else (loop (cdr fields))]))))

  ;; The (start . end) of the form `(KEY ...)` among the fields of the package (or of the
  ;; workspace, for SCOPE `workspace`), or #f.
  (define find-field
    (case-lambda
      [(text key) (find-field text key 'package)]
      [(text key scope) (find-field-in text (container text scope) key)]))

  ;; The dependencies as (name . (start . end)).
  (define dependency-forms
    (case-lambda
      [(text) (dependency-forms text 'package)]
      [(text scope) (scoped-dependency-forms text scope)]))

  (define (scoped-dependency-forms text scope)
    (let ([field (find-field text "dependencies" scope)])
      (if field
        (let-values ([(children close) (list-children text (car field))])
          (filter values
            (map (lambda (child)
                  (let ([name (head-text text child)])
                    (and name (cons name child))))
              (cdr children))))
        '())))

  (define manifest-dependency-names
    (case-lambda
      [(text) (map car (dependency-forms text))]
      [(text scope) (map car (dependency-forms text scope))]))

  ;; The names of the dependencies of the package that are `(NAME (workspace #t) ...)`.
  (define (inherited-dependency-names text)
    (map car
      (filter (lambda (entry)
                (let-values ([(clauses close) (list-children text (car (cdr entry)))])
                  (exists (lambda (c)
                            (and (list-form? text c)
                              (equal? (head-text text c) "workspace")))
                    (cdr clauses))))
        (dependency-forms text))))

  ;;; Edits

  (define (replace text start end new)
    (string-append (substring text 0 start) new (substring text end (string-length text))))

  ;; The number of columns of blanks before the datum at I on its line, or #f when other text
  ;; comes first.
  (define (indentation text i)
    (let loop ([j (- i 1)])
      (cond [(< j 0) i]
        [(char=? (string-ref text j) #\newline) (- i j 1)]
        [(memv (string-ref text j) '(#\space #\tab)) (loop (- j 1))]
        [else #f])))

  (define (write-text datum)
    (call-with-string-output-port (lambda (port) (write datum port))))

  ;; `(NAME (git "URL") (version "1.2.0"))`, or with the other CLAUSES: strings written as they
  ;; are, ("git" "URL") ...
  (define (dependency-text name clauses)
    (string-append "(" name
      (apply string-append
        (map (lambda (clause) (string-append " (" (car clause) " " (write-text (cadr clause)) ")")) clauses))
      ")"))

  ;; The text of the manifest TEXT with the dependency DEPENDENCY (text of a form, see
  ;; `dependency-text`) named NAME added.
  (define add-dependency-text
    (case-lambda
      [(text name dependency) (add-dependency-text text name dependency 'package)]
      [(text name dependency scope) (scoped-add-dependency-text text name dependency scope)]))

  (define (scoped-add-dependency-text text name dependency scope)
    (when (assoc name (dependency-forms text scope))
      (fail (string-append "the " (symbol->string scope) " already has a dependency " name
              "; remove it first (capy remove " name ")")))
    (let ([field (find-field text "dependencies" scope)])
      (cond
        [field
          (let-values ([(children close) (list-children text (car field))])
            (let* ([last-child (car (reverse children))]
                   [indent (if (pair? (cdr children))
                            (or (indentation text (car last-child)) 4)
                            4)])
              (replace text (cdr last-child) (cdr last-child)
                (string-append "\n" (make-string indent #\space) dependency))))]
        [else
          (let*-values ([(form) (container text scope)]
                        [(fields close) (list-children text (car form))])
            (let* ([last-field (car (reverse fields))]
                   [indent (or (and (pair? (cdr fields)) (indentation text (car last-field)))
                             (+ 2 (or (indentation text (car form)) 0)))])
              (replace text (cdr last-field) (cdr last-field)
                (string-append "\n" (make-string indent #\space) "(dependencies\n"
                  (make-string (+ indent 2) #\space) dependency ")"))))])))

  ;; The manifest TEXT without the dependency NAME.
  (define remove-dependency-text
    (case-lambda
      [(text name) (remove-dependency-text text name 'package)]
      [(text name scope) (scoped-remove-dependency-text text name scope)]))

  (define (scoped-remove-dependency-text text name scope)
    (let ([found (assoc name (dependency-forms text scope))])
      (unless found (fail (string-append "the " (symbol->string scope) " has no dependency " name)))
      (let* ([child (cdr found)]
             [start (let back ([j (car child)])
                      (cond
                        [(and (> j 0) (memv (string-ref text (- j 1)) '(#\space #\tab))) (back (- j 1))]
                        [(and (> j 0) (char=? (string-ref text (- j 1)) #\newline)) (- j 1)]
                        [else j]))])
        (replace text start (cdr child) ""))))

  ;; The manifest TEXT with the version of the dependency NAME replaced by VERSION.
  (define set-dependency-version-text
    (case-lambda
      [(text name version) (set-dependency-version-text text name version 'package)]
      [(text name version scope) (scoped-set-dependency-version-text text name version scope)]))

  (define (scoped-set-dependency-version-text text name version scope)
    (let ([found (assoc name (dependency-forms text scope))])
      (unless found (fail (string-append "the package has no dependency " name)))
      (let-values ([(clauses close) (list-children text (car (cdr found)))])
        (let ([clause (find (lambda (c) (equal? (head-text text c) "version")) clauses)])
          (cond
            [clause (replace text (car clause) (cdr clause) (string-append "(version " (write-text version) ")"))]
            [else (fail (string-append "the dependency " name " has no version to change"))])))))

  ;; The manifest TEXT with the directory MEMBER (a string) added to the `members` of its
  ;; workspace.
  (define (add-member-text text member)
    (let ([field (find-field text "members" 'workspace)]
          [entry (write-text member)])
      (cond
        [field
          (let-values ([(children close) (list-children text (car field))])
            (let ([last-child (car (reverse children))])
              (replace text (cdr last-child) (cdr last-child) (string-append " " entry))))]
        [else
          (let*-values ([(form) (container text 'workspace)]
                        [(fields close) (list-children text (car form))])
            (let ([last-field (car (reverse fields))])
              (replace text (cdr last-field) (cdr last-field)
                (string-append " (members " entry ")"))))]))))
