;;; `capy doc`: the API documentation of a package, as HTML or Markdown, in `target/doc/`.

(library (capy pkg doc)
  (export generate-docs!)
  (import (rnrs)
    (rnrs arithmetic bitwise)
    (rnrs hashtables)
    (only (capy) file-exists? file-directory? create-directory* delete-directory* canonicalize-path-string)
    (capy libmap)
    (capy path)
    (capy pkg graph)
    (capy pkg manifest)
    (capy pkg ui))

  ;;; Text

  (define (string-prefix? s prefix)
    (let ([n (string-length s)] [m (string-length prefix)])
      (and (>= n m) (string=? (substring s 0 m) prefix))))

  (define (join strings separator)
    (let loop ([strings strings] [out ""])
      (cond [(null? strings) out]
        [(string=? out "") (loop (cdr strings) (car strings))]
        [else (loop (cdr strings) (string-append out separator (car strings)))])))

  (define (written datum)
    (call-with-string-output-port (lambda (port) (write datum port))))

  (define (split-lines text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text)) (reverse (cons (substring text start i) out))]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (trim-left s)
    (let loop ([i 0])
      (if (and (< i (string-length s)) (char-whitespace? (string-ref s i)))
        (loop (+ i 1))
        (substring s i (string-length s)))))

  (define (trim s)
    (let loop ([end (string-length s)])
      (if (and (> end 0) (char-whitespace? (string-ref s (- end 1))))
        (loop (- end 1))
        (trim-left (substring s 0 end)))))

  (define (read-text path)
    (guard (e [#t ""])
      (call-with-input-file path get-string-all)))

  ;;; The source of a library

  ;; What the source says about a name: the text above it, its signatures (a list of strings) and
  ;; its docstring, each a string or #f.
  (define-record-type entry (fields comment signatures docstring))

  (define definers
    '("define" "define-syntax" "define-record-type" "define-condition-type" "define-values"
      "define-integrable" "define-inline" "define-constant" "define-generic" "define-class"
      "define-method"))

  ;; "define-record-type" of the line "(define-record-type point ...)", or #f.
  (define (definer-of line)
    (let ([line (trim-left line)])
      (and (> (string-length line) 1) (char=? (string-ref line 0) #\()
        (let loop ([i 1])
          (if (and (< i (string-length line))
                (let ([c (string-ref line i)]) (or (char-alphabetic? c) (char=? c #\-))))
            (loop (+ i 1))
            (let ([word (substring line 1 i)])
              (and (member word definers) word)))))))

  ;; The text of a comment line without its semicolons and the space after them.
  (define (comment-text line)
    (let loop ([i 0])
      (cond
        [(and (< i (string-length line)) (char=? (string-ref line i) #\;)) (loop (+ i 1))]
        [(and (< i (string-length line)) (char=? (string-ref line i) #\space))
          (substring line (+ i 1) (string-length line))]
        [else (substring line i (string-length line))])))

  (define (comment-line? line)
    (let ([line (trim-left line)])
      (and (> (string-length line) 0) (char=? (string-ref line 0) #\;))))

  ;; The comment lines that end just above the line I of the vector LINES, as one text, or #f. A
  ;; comment that is followed by a blank line is not about what comes after it.
  (define (comment-above lines i)
    (let loop ([j (- i 1)] [out '()])
      (if (and (>= j 0) (comment-line? (vector-ref lines j)))
        (loop (- j 1) (cons (comment-text (trim-left (vector-ref lines j))) out))
        (and (pair? out) (trim (join out "\n"))))))

  ;; The comment block at the top of the file, if the first library form follows it.
  (define (head-comment lines)
    (let loop ([j 0] [out '()])
      (cond
        [(and (< j (vector-length lines)) (comment-line? (vector-ref lines j)))
          (loop (+ j 1) (cons (comment-text (trim-left (vector-ref lines j))) out))]
        [(and (< j (vector-length lines)) (string=? (trim (vector-ref lines j)) "") (null? out))
          (loop (+ j 1) out)]
        [else (and (pair? out) (trim (join (reverse out) "\n")))])))

  ;; The name a definition form defines: `(define (f . x) ...)`, `(define f ...)`,
  ;; `(define-record-type (point ...) ...)`.
  (define (defined-name form)
    (and (pair? form) (pair? (cdr form))
      (let ([target (cadr form)])
        (cond
          [(symbol? target) target]
          [(and (pair? target) (symbol? (car target))) (car target)]
          [(and (pair? target) (pair? (car target)) (symbol? (car (car target)))) (car (car target))]
          [else #f]))))

  (define (lambda-formals value)
    (and (pair? value) (memq (car value) '(lambda case-lambda)) (eq? (car value) 'lambda)
      (pair? (cdr value)) (cadr value)))

  (define (docstring-of body)
    (and (pair? body) (string? (car body)) (pair? (cdr body)) (car body)))

  (define (syntax-patterns name transformer)
    (and (pair? transformer) (memq (car transformer) '(syntax-rules))
      (pair? (cdr transformer))
      (let* ([rest (cdr transformer)]
             [clauses (if (symbol? (car rest)) (if (pair? (cdr rest)) (cddr rest) '()) (cdr rest))])
        (filter values
          (map (lambda (clause)
                (and (pair? clause) (pair? (car clause))
                  (written (cons name (cdr (car clause))))))
            clauses)))))

  ;; The `entry` for a definition form, with the comment text above it.
  (define (form-entry form kind comment)
    (let ([name (defined-name form)])
      (cond
        [(not name) #f]
        [(string=? kind "define")
          (cond
            [(pair? (cadr form))
              (make-entry comment (list (written (cadr form)))
                (and (pair? (cddr form)) (docstring-of (cddr form))))]
            [(and (pair? (cddr form)) (lambda-formals (caddr form)))
              => (lambda (formals)
                  (make-entry comment (list (written (cons name formals)))
                    (and (pair? (cdddr form)) (docstring-of (cddr (caddr form))))))]
            [else (make-entry comment '() #f)])]
        [(string=? kind "define-syntax")
          (make-entry comment
            (or (and (pair? (cddr form)) (syntax-patterns name (caddr form))) '())
            #f)]
        [else (make-entry comment '() #f)])))

  ;; name -> entry for the definitions in TEXT. The first definition of a name wins, unless it has
  ;; no comment and a later one has.
  (define (scan-definitions text)
    (let* ([lines (list->vector (split-lines text))]
           [table (make-eq-hashtable)]
           ;; the offset of each line
           [offsets (let loop ([i 0] [offset 0] [out '()])
                      (if (= i (vector-length lines))
                        (list->vector (reverse out))
                        (loop (+ i 1) (+ offset (string-length (vector-ref lines i)) 1) (cons offset out))))])
      (let loop ([i 0])
        (when (< i (vector-length lines))
          (let ([kind (definer-of (vector-ref lines i))])
            (when kind
              (let* ([line (vector-ref lines i)]
                     [indent (- (string-length line) (string-length (trim-left line)))]
                     [form (guard (e [#t #f])
                            (read (open-string-input-port
                                    (substring text (+ (vector-ref offsets i) indent) (string-length text)))))]
                     [entry (and form (pair? form) (form-entry form kind (comment-above lines i)))]
                     [name (and entry (defined-name form))])
                (when entry
                  (let ([old (hashtable-ref table name #f)])
                    (when (or (not old) (and (not (entry-comment old)) (entry-comment entry)))
                      (hashtable-set! table name entry)))))))
          (loop (+ i 1))))
      table))

  ;; library name -> comment above its `(library ...)` form, for the libraries in TEXT.
  (define (scan-libraries text)
    (let* ([lines (list->vector (split-lines text))]
           [out '()])
      (let loop ([i 0])
        (when (< i (vector-length lines))
          (let ([line (vector-ref lines i)])
            (when (or (string-prefix? line "(library ") (string-prefix? line "(define-library "))
              (let ([form (guard (e [#t #f])
                           (read (open-string-input-port
                                   (let offset ([j 0] [pos 0])
                                     (if (= j i)
                                       (substring text pos (string-length text))
                                       (offset (+ j 1) (+ pos (string-length (vector-ref lines j)) 1)))))))])
                (when (and form (pair? form) (pair? (cdr form)) (pair? (cadr form)))
                  (let ([name (strip-version (cadr form))]
                        [comment (or (comment-above lines i)
                                   (and (null? out) (head-comment lines)))])
                    (set! out (cons (cons name comment) out)))))))
          (loop (+ i 1))))
      out))

  (define (strip-version name)
    (if (and (pair? (cdr name)) (list? (car (last-pair* name))))
      (reverse (cdr (reverse name)))
      name))

  (define (last-pair* x) (if (pair? (cdr x)) (last-pair* (cdr x)) x))

  ;;; The libraries of the package

  ;; A library of the package: its name, its exports `(name kind origin entry)` and the text above
  ;; the library form.
  (define-record-type doc-library (fields name key exports comment))

  (define (key-name key)
    (if (and (pair? key) (eq? (car key) '|#%pkg|)) (cddr key) key))

  (define (key-package key)
    (and (pair? key) (eq? (car key) '|#%pkg|) (cadr key)))

  (define (name-text name) (written name))

  ;; The knowns of a decl are only there for what the compiler could prove, so a definition with
  ;; the head of a procedure `(define (f x) ...)` is a procedure too.
  (define (kind-of export knowns entry)
    (let ([known (assq (car export) knowns)])
      (cond
        [(eq? (caddr export) 'macro) "syntax"]
        [(and known (eq? (cadr known) 'procedure)) "procedure"]
        [(and entry (pair? (entry-signatures entry))) "procedure"]
        [(and known (eq? (cadr known) 'constant)) "constant"]
        [else "variable"])))

  ;; "1 argument", "2 to 3 arguments", "1 or more arguments" from an arity mask
  (define (arity-text mask)
    (define (arguments n) (string-append (number->string n) (if (= n 1) " argument" " arguments")))
    (let ([first (let loop ([i 0]) (if (bitwise-bit-set? mask i) i (loop (+ i 1))))])
      (if (< mask 0)
        (string-append (number->string first) " or more arguments")
        (let ([last (let loop ([i first]) (if (bitwise-bit-set? mask (+ i 1)) (loop (+ i 1)) i))])
          (cond [(= first last) (arguments first)]
            [(= (bitwise-arithmetic-shift-right mask (+ last 1)) 0)
              (string-append (number->string first) " to " (number->string last) " arguments")]
            [else (string-append "between " (number->string first) " and "
                    (number->string (let top ([i last]) (if (= (bitwise-arithmetic-shift-right mask (+ i 1)) 0) i (top (+ i 1)))))
                    " arguments")])))))

  (define (origin-text origin)
    (let ([key (car origin)])
      (string-append "re-exported from " (name-text (key-name key))
        (if (key-package key) (string-append " (package " (symbol->string (key-package key)) ")") ""))))

  ;; The libraries of PKG that the build compiled, with what the decls and the sources say.
  ;; The libraries of the package of MANIFEST among ENTRIES, the `(key source fasl ...)` entries of
  ;; a libmap with absolute paths. The libraries of a package build have keys `(#%pkg NAME . lib)`;
  ;; the ones of the toolchain have plain keys, so a package of the toolchain (`capy doc` in its
  ;; own workspace) is told apart by its sources alone.
  (define (package-libraries manifest entries)
    (let* ([name (manifest-name manifest)]
           [source-root (string-append (canonicalize-path-string (manifest-source-directory manifest)) "/")]
           [texts (make-hashtable equal-hash equal?)]
           [mine (filter
                  (lambda (e)
                    (and (memq (key-package (car e)) (list name #f))
                      (cadr e) (caddr e)
                      (string-prefix? (cadr e) source-root)))
                  entries)])
      (list-sort
        (lambda (a b) (string<? (name-text (doc-library-name a)) (name-text (doc-library-name b))))
        (filter values
          (map
            (lambda (e)
              (let* ([lib (find (lambda (lib) (equal? (car lib) (car e))) (fasl-libraries (caddr e)))]
                     [decl (and lib (caddr lib))])
                (and decl
                  (let* ([text (or (hashtable-ref texts (cadr e) #f)
                                 (let ([t (read-text (cadr e))]) (hashtable-set! texts (cadr e) t) t))]
                         [definitions (scan-definitions text)]
                         [knowns (decl-knowns decl)]
                         [name (key-name (car e))])
                    (make-doc-library name (car e)
                      (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                        (map
                          (lambda (export)
                            (list (car export)
                              (kind-of export knowns (hashtable-ref definitions (cadr export) #f))
                              (cadddr export)
                              (hashtable-ref definitions (cadr export) #f)
                              (let ([known (assq (car export) knowns)])
                                (and known (eq? (cadr known) 'procedure) (caddr known)))))
                          (decl-exports decl)))
                      (let ([found (assoc name (scan-libraries text))])
                        (and found (cdr found))))))))
            mine)))))

  ;;; Output

  (define (html-escape s)
    (apply string-append
      (map (lambda (c)
            (case c
              [(#\&) "&amp;"] [(#\<) "&lt;"] [(#\>) "&gt;"] [(#\") "&quot;"]
              [else (string c)]))
        (string->list s))))

  ;; "capy.path": a name as a file name
  (define (file-stem name)
    (join (map (lambda (part)
                (list->string
                  (map (lambda (c) (if (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_))) c #\_))
                    (string->list (if (symbol? part) (symbol->string part) (number->string part)))))) name)
      "."))

  (define (first-line text)
    (let ([lines (split-lines text)])
      (if (null? lines) "" (car lines))))

  (define style
    (string-append
      ":root{--bg:#fff;--fg:#1c1c1c;--dim:#666;--rule:#ddd;--code:#f4f4f2;--link:#0b5cad}\n"
      "@media(prefers-color-scheme:dark){:root{--bg:#161616;--fg:#e6e6e6;--dim:#999;--rule:#333;--code:#222;--link:#7ab4f5}}\n"
      "body{background:var(--bg);color:var(--fg);font:16px/1.5 system-ui,sans-serif;max-width:50rem;margin:0 auto;padding:1.5rem 1rem}\n"
      "a{color:var(--link)}code,pre{font-family:ui-monospace,Menlo,monospace;font-size:.92em}\n"
      "pre{background:var(--code);padding:.5rem .75rem;border-radius:4px;white-space:pre-wrap;overflow-wrap:anywhere}\n"
      "h1{font-size:1.6rem}h2{border-bottom:1px solid var(--rule);padding-bottom:.2rem;margin-top:2rem}\n"
      "dt{margin-top:1.1rem}dd{margin-left:1rem}.kind{color:var(--dim);font-size:.85em;margin-left:.5rem}\n"
      ".note{color:var(--dim);font-size:.9em}\n"))

  (define (html-page title body)
    (string-append "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">\n"
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
      "<title>" (html-escape title) "</title>\n<style>\n" style "</style></head>\n<body>\n"
      body "</body></html>\n"))

  (define (entry-lines export)
    ;; the lines that name the export: its signatures, or its name
    (let ([entry (cadddr export)])
      (if (and entry (pair? (entry-signatures entry)))
        (entry-signatures entry)
        (list (symbol->string (car export))))))

  (define (export-notes export)
    (let ([arity (list-ref export 4)] [entry (cadddr export)])
      (append
        (if (list-ref export 2) (list (origin-text (list-ref export 2))) '())
        (if (and arity (not (and entry (pair? (entry-signatures entry)))))
          (list (string-append "takes " (arity-text arity)))
          '()))))

  (define (export-text export)
    (let ([entry (cadddr export)])
      (append
        (if (and entry (entry-comment entry)) (list (entry-comment entry)) '())
        (if (and entry (entry-docstring entry)) (list (entry-docstring entry)) '()))))

  (define (html-library lib pkg-text)
    (string-append
      "<p class=\"note\"><a href=\"index.html\">" (html-escape pkg-text) "</a></p>\n"
      "<h1><code>" (html-escape (name-text (doc-library-name lib))) "</code></h1>\n"
      (if (doc-library-comment lib)
        (string-append "<pre>" (html-escape (doc-library-comment lib)) "</pre>\n")
        "")
      "<h2>Exports</h2>\n"
      (if (null? (doc-library-exports lib))
        "<p class=\"note\">The library exports nothing.</p>\n"
        (string-append "<dl>\n"
          (apply string-append
            (map
              (lambda (export)
                (string-append
                  "<dt id=\"" (html-escape (symbol->string (car export))) "\">"
                  (apply string-append
                    (map (lambda (line) (string-append "<code>" (html-escape line) "</code> ")) (entry-lines export)))
                  "<span class=\"kind\">" (cadr export) "</span></dt>\n<dd>"
                  (apply string-append
                    (map (lambda (text) (string-append "<pre>" (html-escape text) "</pre>")) (export-text export)))
                  (apply string-append
                    (map (lambda (note) (string-append "<p class=\"note\">" (html-escape note) "</p>")) (export-notes export)))
                  "</dd>\n"))
              (doc-library-exports lib)))
          "</dl>\n"))))

  (define (html-index pkg-name pkg-version libs)
    (string-append
      "<h1>" (html-escape pkg-name) " <span class=\"note\">" (html-escape pkg-version) "</span></h1>\n"
      "<h2>Libraries</h2>\n<ul>\n"
      (apply string-append
        (map
          (lambda (lib)
            (string-append "<li><a href=\"" (file-stem (doc-library-name lib)) ".html\"><code>"
              (html-escape (name-text (doc-library-name lib))) "</code></a>"
              (if (doc-library-comment lib)
                (string-append " &mdash; " (html-escape (first-line (doc-library-comment lib))))
                "")
              "</li>\n"))
          libs))
      "</ul>\n"))

  (define (markdown-library lib pkg-text)
    (string-append
      "[" pkg-text "](index.md)\n\n"
      "# `" (name-text (doc-library-name lib)) "`\n\n"
      (if (doc-library-comment lib) (string-append (doc-library-comment lib) "\n\n") "")
      "## Exports\n\n"
      (if (null? (doc-library-exports lib))
        "The library exports nothing.\n"
        (apply string-append
          (map
            (lambda (export)
              (string-append
                "### `" (symbol->string (car export)) "` (" (cadr export) ")\n\n"
                (apply string-append
                  (map (lambda (line) (string-append "    " line "\n")) (entry-lines export)))
                "\n"
                (apply string-append (map (lambda (text) (string-append text "\n\n")) (export-text export)))
                (apply string-append (map (lambda (note) (string-append "*" note "*\n\n")) (export-notes export)))))
            (doc-library-exports lib))))))

  (define (markdown-index pkg-name pkg-version libs)
    (string-append
      "# " pkg-name " " pkg-version "\n\n## Libraries\n\n"
      (apply string-append
        (map
          (lambda (lib)
            (string-append "- [`" (name-text (doc-library-name lib)) "`](" (file-stem (doc-library-name lib)) ".md)"
              (if (doc-library-comment lib)
                (string-append " - " (first-line (doc-library-comment lib)))
                "")
              "\n"))
          libs))))

  (define (write-text! path text)
    (call-with-output-file path (lambda (port) (put-string port text))))

  ;; Writes the documentation of PKG (a `package` of `(capy pkg graph)`, built, with the libmap
  ;; LIBMAP-PATH) into DIRECTORY, in FORMAT (`html` or `markdown`). Returns the number of
  ;; libraries.
  ;; Writes the documentation of the package of MANIFEST, whose libraries are among ENTRIES (see
  ;; `package-libraries`), into DIRECTORY. Returns how many libraries it documented.
  (define (generate-docs! manifest entries directory format)
    (let* ([pkg-name (symbol->string (manifest-name manifest))]
           [pkg-version (string-append "v" (manifest-version manifest))]
           [pkg-text (string-append pkg-name " " pkg-version)]
           [libs (package-libraries manifest entries)]
           [html? (string=? format "html")]
           [extension (if html? ".html" ".md")])
      (when (null? libs)
        (fail "the package has no libraries to document: there are none in" (manifest-source manifest)))
      (when (file-exists? directory) (delete-directory* directory))
      (create-directory* directory)
      (write-text! (path-join directory (string-append "index" extension))
        (if html?
          (html-page pkg-text (html-index pkg-name pkg-version libs))
          (markdown-index pkg-name pkg-version libs)))
      (for-each
        (lambda (lib)
          (write-text! (path-join directory (string-append (file-stem (doc-library-name lib)) extension))
            (if html?
              (html-page (name-text (doc-library-name lib)) (html-library lib pkg-text))
              (markdown-library lib pkg-text))))
        libs)
      (length libs))))
