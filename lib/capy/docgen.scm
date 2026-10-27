(library (capy docgen)
  (export document-modules docgen-main)
  (import (core) (core sorting) (capy) (srfi 13) (srfi 180))

  ;; Extract structured documentation for libraries.
  ;;
  ;; Walks explicitly requested modules at runtime (so native procedures,
  ;; whose documentation is attached as closure metadata by capy-derive,
  ;; are covered) and merges in `;;` comment blocks read from the
  ;; libraries' source files (module headers and per-definition comments,
  ;; which never reach the runtime: the expander drops the library doc
  ;; string and comments are not values).

  (define (join-strings parts sep)
    (if (null? parts)
      ""
      (let loop ((out (car parts)) (rest (cdr parts)))
        (if (null? rest)
          out
          (loop (string-append out sep (car rest)) (cdr rest))))))

  (define (split-lines str)
    (let ((len (string-length str)))
      (let loop ((i 0) (start 0) (out '()))
        (if (>= i len)
          (reverse (cons (substring str start len) out))
          (if (char=? (string-ref str i) #\newline)
            (loop (+ i 1) (+ i 1) (cons (substring str start i) out))
            (loop (+ i 1) start out))))))

  (define (trailing-space? c)
    (or (char=? c #\space) (char=? c #\newline)
        (char=? c #\tab) (char=? c #\return)))

  (define (rtrim str)
    (let loop ((end (string-length str)))
      (if (zero? end)
        ""
        (if (trailing-space? (string-ref str (- end 1)))
          (loop (- end 1))
          (substring str 0 end)))))

  (define (trim str)
    (let ((len (string-length str)))
      (let left ((i 0))
        (if (or (>= i len) (not (trailing-space? (string-ref str i))))
          (rtrim (substring str i len))
          (left (+ i 1))))))


  ;; capy-derive appends the same GC-contract footer to every native
  ;; procedure's documentation. It is identical on all of them, so it
  ;; carries no per-procedure information and is dropped here. Returns
  ;; the user-written part, or the whole string when no footer is found.
  (define native-footer-markers
    (list "# GC contract (`#[scheme]`)" "# Safety (`#[scheme(unsafe)]`)"))

  (define (strip-native-footer doc)
    (let loop ((markers native-footer-markers))
      (if (null? markers)
        doc
        (let ((pos (string-contains doc (car markers))))
          (if pos
            (rtrim (substring doc 0 pos))
            (loop (cdr markers)))))))

  ;; Rust `///` lines keep one leading space in the doc attribute.
  (define (strip-one-leading-space line)
    (if (and (> (string-length line) 0)
             (char=? (string-ref line 0) #\space))
      (substring line 1 (string-length line))
      line))

  (define (clean-native-doc doc)
    (let ((cleaned (join-strings (map strip-one-leading-space
                                      (split-lines (strip-native-footer doc)))
                                 "\n")))
      (if (equal? (trim cleaned) "") 'null cleaned)))

  (define (segment? x)
    (or (symbol? x) (and (integer? x) (exact? x))))

  (define (segment->string x)
    (if (symbol? x) (symbol->string x) (number->string x)))

  ;; "(core lists)" -> (core lists); anything else -> #f.
  (define (parse-module-path str)
    (guard (c (#t #f))
      (let ((datum (read (open-input-string str))))
        (let loop ((rest datum))
          (cond ((not (list? datum)) #f)
                ((null? datum) #f)
                ((null? rest) datum)
                ((segment? (car rest)) (loop (cdr rest)))
                (else #f))))))

  (define (module-relpath mod-name)
    (string-append (join-strings (map segment->string mod-name) "/") ".scm"))

  ;; First load-path entry holding the module's file, or #f for
  ;; programmatically defined modules (e.g. (capy) itself).
  (define (module-file-path mod-name)
    (let ((rel (module-relpath mod-name)))
      (let loop ((dirs (if (list? %load-path) %load-path '())))
        (if (null? dirs)
          #f
          (let ((cand (string-append (car dirs) "/" rel)))
            (if (file-exists? cand)
              cand
              (loop (cdr dirs))))))))

  ;; A full-line `;;` comment's text, or #f when the line is not one.
  (define (comment-text line)
    (let ((len (string-length line)))
      (let skip ((i 0))
        (if (>= i len)
          #f
          (let ((c (string-ref line i)))
            (if (or (char=? c #\space) (char=? c #\tab))
              (skip (+ i 1))
              (if (and (char=? c #\;)
                       (< (+ i 1) len)
                       (char=? (string-ref line (+ i 1)) #\;))
                (let strip ((j (+ i 2)))
                  (cond ((and (< j len) (char=? (string-ref line j) #\;))
                         (strip (+ j 1)))
                        ((and (< j len) (char=? (string-ref line j) #\space))
                         (substring line (+ j 1) len))
                        (else (substring line j len))))
                #f)))))))

  ;; A single-line string literal's content, e.g. a library docstring.
  (define (string-literal-line line)
    (let ((t (trim line)))
      (let ((n (string-length t)))
        (if (and (>= n 2)
                 (char=? (string-ref t 0) #\")
                 (char=? (string-ref t (- n 1)) #\"))
          (substring t 1 (- n 1))
          #f))))

  (define (blank-line? line)
    (let ((len (string-length line)))
      (let loop ((i 0))
        (if (>= i len)
          #t
          (let ((c (string-ref line i)))
            (if (or (char=? c #\space) (char=? c #\tab) (char=? c #\return))
              (loop (+ i 1))
              #f))))))

  ;; `  (define (name ...`, `  (define name ...`, or
  ;; `  (define-syntax name ...` -> name, else #f.
  ;; Only `define` and `define-syntax` are covered: other defining forms
  ;; (record types, define-values) create bindings the runtime already
  ;; reports, and guessing their generated names from text would
  ;; misattribute comments.
  (define (define-target-name line)
    (let ((len (string-length line)))
      ;; Read a token starting at/after i; returns (token . end) or #f.
      (define (token-at i)
        (let blanks ((k i))
          (if (and (< k len)
                   (let ((c (string-ref line k)))
                     (or (char=? c #\space) (char=? c #\tab))))
            (blanks (+ k 1))
            (let chars ((k k) (buf '()))
              (if (or (>= k len)
                      (let ((ch (string-ref line k)))
                        (or (char=? ch #\space) (char=? ch #\tab)
                            (char=? ch #\() (char=? ch #\)))))
                (and (pair? buf) (cons (list->string (reverse buf)) k))
                (chars (+ k 1) (cons (string-ref line k) buf)))))))
      (let skip ((i 0))
        (if (>= i len)
          #f
          (let ((c (string-ref line i)))
            (cond ((or (char=? c #\space) (char=? c #\tab)) (skip (+ i 1)))
                  ((not (char=? c #\()) #f)
                  (else
                   (let ((kw (token-at (+ i 1))))
                     (and kw
                          (or (equal? (car kw) "define")
                              (equal? (car kw) "define-syntax"))
                          (let ((nm (if (and (< (cdr kw) len)
                                             (char=? (string-ref line (cdr kw))
                                                     #\())
                                        ;; `(define (name ...`
                                        (token-at (+ (cdr kw) 1))
                                        ;; `(define name ...`
                                        (token-at (cdr kw)))))
                            (and nm (string->symbol (car nm)))))))))))))

  ;; Returns ((module-doc . string-or-#f) (defs . ((name . doc) ...))).
  ;; The first comment block (or single-line docstring) before the first
  ;; form documents the module; a comment block directly above a
  ;; `(define ...)` line documents that binding. Blank lines end a block.
  (define (scan-file path)
    (define first-block #f)
    (define docstring #f)
    (define defs '())
    (define pending '())
    (define seen-form? #f)
    ;; Comments preceding the first form document the module (the
    ;; usual header style, blank lines included). Later blocks belong to
    ;; the definition beneath them.
    (define (note-block!)
      (when (and (not seen-form?) (pair? pending))
        (set! first-block (join-strings (reverse pending) "\n"))))
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (let ((line (read-line in)))
            (if (eof-object? line)
              (begin (note-block!) (set! pending '()))
              (let ((text (comment-text line)))
                (cond (text
                       (set! pending (cons text pending))
                       (loop))
                      ;; Blank lines never break a block: a comment
                      ;; paragraph, a blank line, then the definition is
                      ;; the usual style. A non-comment line still ends
                      ;; the block (attaching it or dropping it).
                      ((blank-line? line)
                       (note-block!)
                       (loop))
                      (else
                       (let ((lit (and (not seen-form?)
                                       (string-literal-line line)))
                             (name (define-target-name line)))
                         (note-block!)
                         (when (and lit (not docstring))
                           (set! docstring lit))
                         (when (and name
                                    (pair? pending)
                                    (not (assq name defs)))
                           (set! defs
                                 (cons (cons name
                                             (join-strings
                                              (reverse pending) "\n"))
                                       defs)))
                         (set! seen-form? #t)
                         (set! pending '())
                         (loop))))))))))
    (list (cons 'module-doc (or docstring first-block)) (cons 'defs defs)))

  (define (visited? mod-name visited)
    (let loop ((rest visited))
      (cond ((null? rest) #f)
            ((equal? (car rest) mod-name) #t)
            (else (loop (cdr rest))))))

  (define (binding-value var)
    (guard (c (#t #f))
      (variable-ref var)))

  (define (procedure-source-alist proc)
    (let ((src (and (procedure? proc) (procedure-sourcev proc))))
      (if (and (vector? src) (>= (vector-length src) 3)
               (string? (vector-ref src 0)))
        (list (cons 'file (vector-ref src 0))
              (cons 'line (vector-ref src 1))
              (cons 'column (vector-ref src 2)))
        'null)))

  (define (procedure-kind proc src-alist)
    (cond ((macro? proc) "syntax")
          ((not (procedure? proc)) "value")
          (else
           (let ((file (and (pair? src-alist) (cdr (assq 'file src-alist)))))
             (if (and (string? file)
                      (let ((n (string-length file)))
                        (and (>= n 3) (equal? (substring file (- n 3) n) ".rs"))))
               "native-procedure"
               "scheme-procedure")))))

  (define (export-doc kind value static-defs name)
    (let ((rt-doc (and (procedure? value) (procedure-documentation value))))
      (cond ((and (string? rt-doc) (equal? kind "native-procedure"))
             (clean-native-doc rt-doc))
            ((string? rt-doc) rt-doc)
            (else
             (let ((found (assq name static-defs)))
               (if found (cdr found) 'null))))))

  (define (document-bindings mod static-defs)
    (let ((out '()))
      (module-for-each
       (lambda (name var)
         (when (symbol? name)
           (let ((value (binding-value var)))
             (when value
               (let* ((src (procedure-source-alist value))
                      (kind (procedure-kind value src)))
                 (set! out (cons (list (cons 'name (symbol->string name))
                                       (cons 'kind kind)
                                       (cons 'doc (export-doc kind value
                                                              static-defs name))
                                       (cons 'source src))
                                 out)))))))
       mod)
      (list-sort (lambda (a b)
                   (string<? (cdr (assq 'name a)) (cdr (assq 'name b))))
                 out)))

  (define (module-display-name doc)
    (join-strings (vector->list (cdr (assq 'name doc))) " "))

  ;; Scan results keyed by file path, so several modules sharing one
  ;; file (or repeated walks) scan it only once.
  (define (cached-scan path static-cache-box)
    (let ((hit (assoc path (car static-cache-box))))
      (if hit
        (cdr hit)
        (let ((s (guard (c (#t #f)) (scan-file path))))
          (when s
            (set-car! static-cache-box
                      (cons (cons path s) (car static-cache-box))))
          s))))

  (define (document-submodules mod-name mod visited static-cache-box)
    (let ((subs '()))
      (for-each
       (lambda (kv)
         (when (symbol? (car kv))
           (let ((child (document-module (append mod-name (list (car kv)))
                                         visited
                                         static-cache-box)))
             (when child
               (set! subs (cons child subs))))))
       (core-hash->list (module-submodules mod)))
      (list-sort (lambda (a b)
                   (string<? (module-display-name a)
                             (module-display-name b)))
                 subs)))

  (define (document-module mod-name visited static-cache-box)
    ;; Autoload brings in requested libraries that are not loaded yet,
    ;; so every resolvable module documents on demand.
    (let ((mod (guard (c (#t #f)) (resolve-module mod-name #t #f))))
      (if (or (not mod) (not (module? mod)) (visited? mod-name visited))
        #f
        (let* ((seen (cons mod-name visited))
               (path (module-file-path mod-name))
               (scan (and path (cached-scan path static-cache-box)))
               (mod-doc (let ((d (and scan (cdr (assq 'module-doc scan)))))
                          (if (string? d) d 'null)))
               (static-defs (if scan (cdr (assq 'defs scan)) '()))
               (exports (document-bindings mod static-defs))
               (subs (document-submodules mod-name mod seen static-cache-box)))
          (list (cons 'name (list->vector (map segment->string mod-name)))
                (cons 'doc mod-doc)
                (cons 'exports (list->vector exports))
                (cons 'submodules (list->vector subs)))))))

  (define (document-modules mod-names)
    (let ((static-cache-box (list '()))
          (modules '()))
      (for-each
       (lambda (mod-name)
         (let ((doc (document-module mod-name '() static-cache-box)))
           (when doc (set! modules (cons doc modules)))))
       mod-names)
      (list (cons 'generator "capy-docgen")
            (cons 'version (implementation-version))
            (cons 'modules (list->vector (reverse modules))))))

  (define (module-path-string mod-name)
    (string-append "(" (join-strings (map segment->string mod-name) " ") ")"))

  ;; Requested modules that produced no documentation entry, e.g.
  ;; because the name does not resolve. Containers that resolved are
  ;; never reported even when empty.
  (define (missing-modules mod-names doc)
    (let ((found (map (lambda (m)
                        (join-strings (vector->list (cdr (assq 'name m))) " "))
                      (vector->list (cdr (assq 'modules doc))))))
      (let loop ((rest mod-names) (out '()))
        (if (null? rest)
          (reverse out)
          (let ((want (join-strings (map segment->string (car rest)) " ")))
            (loop (cdr rest)
                  (if (member want found) out (cons (car rest) out))))))))

  ;; module-paths: list of strings like "(core lists)".
  ;; output-path: file to write, or #f for stdout.
  (define (docgen-main module-paths output-path)
    (let ((mods (map parse-module-path module-paths)))
      (if (and (pair? mods) (not (memq #f mods)))
        (let ((doc (document-modules mods)))
          (let ((missing (missing-modules mods doc)))
            (when (pair? missing)
              (format (current-error-port)
                      "capy-docgen: could not resolve module~a: ~a~%"
                      (if (null? (cdr missing)) "" "s")
                      (join-strings (map module-path-string missing) " "))
              (exit 1)))
          (if (and (string? output-path) (not (equal? output-path "")))
            (call-with-output-file output-path
              (lambda (out) (json-write doc out) (newline out)))
            (begin (json-write doc) (newline)))
          (exit 0))
        (begin
          (format (current-error-port)
                  "capy-docgen: each module must look like '(core lists)', got: ~a~%"
                  module-paths)
          (exit 1))))))
