;;; The package graph and the library graph.

(library (capy pkg graph)
  (export resolve-packages
    resolve-package-tree
    package?
    package-name
    package-manifest
    package-directory
    package-source-directory
    package-dependencies
    package-view
    package-view-set!
    package-units
    package-out-directory
    package-out-directory-set!
    make-views!
    scan-packages!
    scan-generated!
    unit?
    unit-package
    unit-path
    unit-relative-path
    unit-names
    unit-keys
    unit-dependencies
    unit-kind
    unit-boot-name
    unit-built?
    unit-requires
    library-graph
    program-dependencies
    library-name-text
    source-extensions
    source-file-name?)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) canonicalize-path-string file-directory? directory-list file-exists?)
    (capy libmap)
    (capy path)
    (capy pkg manifest)
    (capy pkg resolve)
    (capy pkg toolchain)
    (capy pkg ui))

  (define-record-type package
    (fields name manifest directory dependencies (mutable view) (mutable units)
      ;; the output directory of the build, or #f
      (mutable out-directory)))

  ;; KIND is `library`, `boot` (a headerless boot file psyntax compiles, `(|#%require| (boot X) ...)`)
  ;; or `expander` (a boot file the Rust expander compiles). A boot file has the name X of its
  ;; file, BOOT-NAME, which the boot files that require it use, and REQUIRES, the names of the
  ;; boot files it requires. The libraries a boot file defines after its header are its keys.
  (define-record-type unit
    (fields package path relative-path names keys imports (mutable dependencies)
      kind boot-name requires))

  (define (library-name-text name)
    (call-with-string-output-port (lambda (port) (write name port))))

  (define (package-source-directory pkg) (manifest-source-directory (package-manifest pkg)))

  ;;; The packages

  ;; The package in the directory DIR and the packages it depends on, as a list with the
  ;; dependencies first (the package itself is last). 
  (define (resolve-packages dir resolution)
    (let ([nodes '()] [order '()])
      (define (visit dir visiting id)
        (let* ([dir (canonicalize-path-string dir)]
               [seen (assoc dir nodes)])
          (cond
            [seen (cdr seen)]
            [(member dir visiting)
              (fail "the dependencies form a cycle through" dir)]
            [else
              (let* ([manifest (read-manifest (path-join dir "package.scm"))]
                     [deps (map
                            (lambda (dependency)
                              (cond
                                [(dependency-path dependency)
                                  => (lambda (relative)
                                      (let ([target (path-join dir relative)])
                                        (unless (file-exists? (path-join target "package.scm"))
                                          (fail (string-append "the dependency "
                                                 (symbol->string (dependency-name dependency))
                                                 " of " (symbol->string (manifest-name manifest))
                                                 " has no package.scm:")
                                            target))
                                        (cons dependency (visit target (cons dir visiting) #f))))]
                                [(resolution-lookup resolution dependency)
                                  => (lambda (node)
                                      (cons dependency
                                        (visit (node-directory node) (cons dir visiting) (node-id node))))]
                                [else
                                  (fail (string-append "the dependency "
                                         (symbol->string (dependency-name dependency))
                                         " of " (symbol->string (manifest-name manifest))
                                         " was not resolved"))]))
                            (manifest-dependencies manifest))]
                     [pkg (make-package (or id (manifest-name manifest)) manifest dir deps #f '() #f)])
                (set! nodes (cons (cons dir pkg) nodes))
                (set! order (cons pkg order))
                pkg)])))
      (for-each (lambda (dir) (visit dir '() #f)) (if (string? dir) (list dir) dir))
      (let ([packages (reverse order)])
        (let loop ([ps packages])
          (when (pair? ps)
            (for-each
              (lambda (other)
                (when (eq? (package-name (car ps)) (package-name other))
                  (fail (string-append "two packages are named " (symbol->string (package-name other)) ":")
                    (package-directory (car ps)) (package-directory other))))
              (cdr ps))
            (loop (cdr ps))))
        packages)))

  ;; The packages of the tree DIR (`DIR/*/package.scm`), the dependencies first, for the build of
  ;; the toolchain itself. The directories of the packages are the ones the manifests are in, as
  ;; they are given.
  (define (resolve-package-tree dir)
    (let ([manifests (read-manifest-tree dir)] [made '()])
      (for-each
        (lambda (manifest)
          (let ([deps (map (lambda (pair) (cons (car pair) (cdr (assq (cdr pair) made))))
                       (manifest-path-dependencies manifest manifests))])
            (set! made
              (cons (cons manifest
                      (make-package (manifest-name manifest) manifest (manifest-directory manifest)
                        deps #f '() #f))
                made))))
        manifests)
      (map cdr (reverse made))))

  ;;; Views


  (define (core-provides toolchain)
    (let ([view (libmap-view (toolchain-libmap toolchain) 'core)])
      (if view
        (let loop ([entries (view-entries view)] [out '()])
          (cond
            [(null? entries)
              (append (reverse out)
                (filter (lambda (lib) (not (member lib out))) native-libraries))]
            [(eq? (car (cdr (car entries))) 'core) (loop (cdr entries) (cons (car (car entries)) out))]
            [else (loop (cdr entries) out)]))
        '())))

  ;; Gives every package its view: what it provides, the packages it depends on under the names
  ;; their `rename` clauses give, the packages of the toolchain `core` and, unless it says
  ;; `no-std`, `std`.
  (define (make-views! packages toolchain)
    (define bootstrap? (toolchain-bootstrap? toolchain))
    (define (key-id pkg) (manifest-stage-key (package-manifest pkg)))
    (define core-package (find (lambda (pkg) (manifest-boot-package? (package-manifest pkg))) packages))
    (for-each
      (lambda (pkg)
        (when (memq (package-name pkg) (toolchain-package-ids toolchain))
          (fail (string-append "the package name " (symbol->string (package-name pkg))
                 " is the name of a package of the toolchain:")
            (package-directory pkg)))
        (let* ([manifest (package-manifest pkg)]
               [deps (map (lambda (pair)
                           (list (if bootstrap? (key-id (cdr pair)) (package-name (cdr pair)))
                             (manifest-provides (package-manifest (cdr pair)))
                             (dependency-renames (car pair))))
                      (package-dependencies pkg))]
               [core-entry
                 (cond
                   [(not bootstrap?) (list (list 'core (core-provides toolchain) '()))]
                   [(or (eq? pkg core-package) (assq 'core deps) (not core-package)) '()]
                   [else (list (list 'core (manifest-provides (package-manifest core-package)) '()))])])
          (package-view-set! pkg
            (if bootstrap?
              (make-package-view (package-name pkg) (manifest-provides manifest)
                (append deps core-entry) (manifest-no-std? manifest) (key-id pkg))
              (make-package-view (package-name pkg) (manifest-provides manifest)
                (append deps core-entry) (manifest-no-std? manifest))))))
      packages))

  ;;; The sources

;; The extensions of the files that hold libraries and programs: the package manager's own notion,
  ;; which the language server follows.
  (define source-extensions '(".scm" ".sld" ".sls"))

  (define (source-file-name? name)
    (let ([n (string-length name)])
      (exists (lambda (ext)
                (let ([m (string-length ext)])
                  (and (> n m) (string=? (substring name (- n m) n) ext))))
        source-extensions)))

  (define (source-file? name) (source-file-name? name))

  (define (files-under dir)
    (let walk ([dir dir])
      (apply append
        (map (lambda (name)
              (let ([path (path-join dir name)])
                (cond
                  [(file-directory? path) (walk path)]
                  [(source-file? name) (list path)]
                  [else '()])))
          (list-sort string<? (directory-list dir))))))

  (define (name-without-version name)
    (if (and (pair? name) (pair? (cdr name))
         (let last? ([l name]) (if (null? (cdr l)) (list? (car l)) (last? (cdr l)))))
      (let drop-last ([l name])
        (if (null? (cdr l)) '() (cons (car l) (drop-last (cdr l)))))
      name))

  ;; The library an import set names.
  (define (spec-library spec)
    (cond
      [(and (pair? spec) (memq (car spec) '(only except prefix rename for library)) (pair? (cdr spec)))
        (spec-library (cadr spec))]
      [(and (pair? spec) (list? spec)) (name-without-version spec)]
      [else #f]))

  ;; The library names the declarations CLAUSES import: `import` clauses, and the ones under
  ;; `cond-expand` whatever the feature requirement.
  (define (clause-imports clauses)
    (apply append
      (map (lambda (clause)
            (cond
              [(and (pair? clause) (eq? (car clause) 'import) (list? clause))
                (filter (lambda (name) name) (map spec-library (cdr clause)))]
              [(and (pair? clause) (eq? (car clause) 'cond-expand) (list? clause))
                (apply append
                  (map (lambda (branch) (if (pair? branch) (clause-imports (cdr branch)) '()))
                    (cdr clause)))]
              [else '()]))
        clauses)))

  ;; Whether the build compiles the unit: the libraries a manifest lists as `source-only` are not.
  (define (unit-built? unit) (not (eq? (unit-kind unit) 'source)))

  (define (library-form? form)
    (and (pair? form) (memq (car form) '(library define-library))
      (pair? (cdr form)) (pair? (cadr form)) (list? form)))

  (define (library-header form)
    (cons (name-without-version (cadr form)) (clause-imports (cddr form))))

  ;; The names X of the `(boot X)` libraries the specs of a `|#%require|` form name.
  (define (boot-requires specs)
    (filter values
      (map (lambda (spec)
            (let ([name (spec-library spec)])
              (and name (= (length name) 2) (eq? (car name) 'boot) (cadr name))))
        specs)))

  (define (read-error-guard path thunk)
    (guard (e [(message-condition? e)
                (fail (string-append path ": " (condition-message e)))])
      (thunk)))

  ;; What the top of the file PATH is: `(boot REQUIRES)` for a boot file, which starts with a
  ;; `(|#%require| (boot X) ...)` form, `(library HEADERS)` for the `(name . imports)` of the
  ;; library forms at its top (the first form that is not one ends the list: the file is a
  ;; program), or `(other)`.
  (define (file-top path)
    (read-error-guard path
      (lambda ()
        (call-with-input-file path
          (lambda (port)
            (let ([first (read port)])
              (if (and (pair? first) (eq? (car first) '|#%require|) (list? first))
                (list 'boot (boot-requires (cdr first)))
                (let loop ([form first] [out '()])
                  (if (library-form? form)
                    (loop (read port) (cons (library-header form) out))
                    (if (null? out) (list 'other) (list 'library (reverse out))))))))))))

  ;; The headers of the libraries a boot file defines after its `#%require` form, and the
  ;; libraries its other forms import (`(import ...)` after the header).
  (define (boot-rest path)
    (read-error-guard path
      (lambda ()
        (call-with-input-file path
          (lambda (port)
            (read port)
            (let loop ([headers '()] [imports '()])
              (let ([form (read port)])
                (cond
                  [(eof-object? form) (values (reverse headers) imports)]
                  [(library-form? form) (loop (cons (library-header form) headers) imports)]
                  [(and (pair? form) (eq? (car form) 'import) (list? form))
                    (loop headers (append imports (clause-imports (list form))))]
                  [else (loop headers imports)]))))))))

  (define (without-extension path)
    (let ([n (string-length path)])
      (if (and (> n 4) (member (substring path (- n 4) n) source-extensions))
        (substring path 0 (- n 4))
        path)))

  ;; The units of the files below ROOT that define libraries, for the package PKG, whose paths are
  ;; not in SEEN. The paths of the files in the units are relative to ROOT. The boot files of the
  ;; package (see `manifest-boot-expander`) are units too.
  (define (directory-units pkg root seen)
    (let* ([expanders (map (lambda (file) (path-join root file))
                       (manifest-boot-expander (package-manifest pkg)))]
           [tops (map (lambda (file) (cons file (file-top file)))
                  (filter (lambda (file) (not (member file seen)))
                    (prefer-edition-sources (files-under root)
                      (manifest-edition (package-manifest pkg)))))]
           [boot-file (lambda (name) (path-join root (string-append "boot/" (symbol->string name) ".scm")))]
           [name-of (lambda (file)
                      (string->symbol (without-extension (path-file-name file))))]
           [boots (filter (lambda (entry) (eq? (cadr entry) 'boot)) tops)]
           ;; the boot files the Rust expander compiles: the listed ones and what they require
           [expander-set
             (let loop ([todo expanders] [done '()])
               (cond
                 [(null? todo) done]
                 [(member (car todo) done) (loop (cdr todo) done)]
                 [else
                   (let ([entry (assoc (car todo) boots)])
                     (loop (append (cdr todo)
                             (if entry
                               (map boot-file (caddr entry))
                               '()))
                       (cons (car todo) done)))]))])
      (let loop ([entries tops] [out '()])
        (cond
          [(null? entries) (reverse out)]
          [else
            (let* ([file (car (car entries))]
                   [top (cdr (car entries))]
                   [relative (without-extension (path-relative file root))]
                   [expander? (member file expander-set)])
              (define (unit headers imports kind boot-name requires)
                (make-unit pkg file relative
                  (if (null? headers) (list (list 'boot (name-of file))) (map car headers))
                  (map (lambda (header)
                         (or (view-library-key (package-view pkg) (car header))
                           (fail (string-append file ": the library "
                                  (library-name-text (car header))
                                  " is not provided by the package "
                                  (symbol->string (package-name pkg))
                                  "; its provides are")
                             (manifest-provides (package-manifest pkg)))))
                    headers)
                  (append (apply append (map cdr headers)) imports)
                  '() kind boot-name requires))
              (loop (cdr entries)
                (cond
                  [(eq? (car top) 'boot)
                    (if expander?
                      (cons (unit '() '() 'expander (name-of file) (cadr top)) out)
                      (let-values ([(headers imports) (boot-rest file)])
                        (cons (unit headers imports 'boot (name-of file) (cadr top)) out)))]
                  [(eq? (car top) 'library)
                    (cons (unit (cadr top) '() 
                            (if (for-all (lambda (header)
                                           (member (car header) (manifest-source-only (package-manifest pkg))))
                                  (cadr top))
                              'source
                              'library)
                            #f '())
                      out)]
                  [expander? (cons (unit '() '() 'expander #f '()) out)]
                  [else out])))]))))

  ;; The units of the files of the output directory of PKG, if it has one.
  (define (generated-units pkg seen)
    (let ([out (package-out-directory pkg)])
      (if (and out (file-exists? out) (file-directory? out))
        (directory-units pkg out seen)
        '())))

  ;; Reads the headers of the source files of every package, and of the libraries its output
  ;; directory holds already, and makes their units: one for each file that defines libraries.
  (define (scan-packages! packages)
    (for-each
      (lambda (pkg)
        (package-units-set! pkg
          (append (directory-units pkg (package-source-directory pkg) '())
            (generated-units pkg '()))))
      packages))

  ;; Makes the units of the libraries that appeared in the output directories of PACKAGES since they
  ;; were scanned, adds them to their packages and returns them.
  (define (scan-generated! packages)
    (apply append
      (map
        (lambda (pkg)
          (let ([fresh (generated-units pkg (map unit-path (package-units pkg)))])
            (package-units-set! pkg (append (package-units pkg) fresh))
            fresh))
        packages)))

  ;;; The library graph

  (define (unknown-library! pkg path name)
    (let ([deps (map (lambda (pair) (symbol->string (package-name (cdr pair))))
                 (package-dependencies pkg))])
      (fail (string-append path ": no library " (library-name-text name)
             " in the view of package " (symbol->string (package-name pkg))
             " (it provides "
             (call-with-string-output-port (lambda (p) (write (manifest-provides (package-manifest pkg)) p)))
             (if (null? deps) "" (string-append ", and depends on the packages: "
                                  (let loop ([d deps] [out ""])
                                    (if (null? d) out
                                      (loop (cdr d) (string-append out (if (string=? out "") "" ", ") (car d)))))))
             (if (manifest-no-std? (package-manifest pkg)) "; it has no std" "")
             ")"))))

  ;; Finds the unit that defines every library the units import, checks that nothing is
  ;; defined twice and that the imports have no cycle. Returns the units, every unit after the
  ;; ones it imports from.
  (define (library-graph packages toolchain)
    (let ([table (make-hashtable equal-hash equal?)]
          [boot-table #f]
          [units (apply append (map package-units packages))])
      (for-each
        (lambda (unit)
          (for-each
            (lambda (key)
              (let ([other (hashtable-ref table key #f)])
                (when other
                  (fail "a library is defined in two files:" (unit-path other) (unit-path unit)))
                (hashtable-set! table key unit)))
            (unit-keys unit)))
        units)
      (let ([boots (make-eq-hashtable)])
        (for-each
          (lambda (unit)
            (when (unit-boot-name unit) (hashtable-set! boots (unit-boot-name unit) unit)))
          units)
        (for-each
          (lambda (unit)
            (for-each
              (lambda (name)
                (unless (hashtable-ref boots name #f)
                  (fail (string-append (unit-path unit) ": no boot file " (symbol->string name)))))
              (unit-requires unit)))
          units)
        (set! boot-table boots))
      (for-each
        (lambda (unit)
          (let ([pkg (unit-package unit)])
            (unit-dependencies-set! unit
              (let loop ([names (unit-imports unit)]
                         [out (reverse (map (lambda (name) (hashtable-ref boot-table name #f))
                                        (unit-requires unit)))])
                (if (null? names)
                  (reverse out)
                  (let* ([key (view-library-key (package-view pkg) (car names))]
                         [dep (and key (hashtable-ref table key #f))])
                    (cond
                      [(not key) (unknown-library! pkg (unit-path unit) (car names))]
                      [dep (loop (cdr names)
                             (if (or (eq? dep unit) (memq dep out) (not (unit-built? dep))) out (cons dep out)))]
                      [(toolchain-has-library? toolchain key) (loop (cdr names) out)]
                      [else (unknown-library! pkg (unit-path unit) (car names))])))))))
        units)
      ;; depth first: a unit comes after its dependencies
      (let ([state (make-eq-hashtable)] [order '()])
        (define (visit unit trail)
          (case (hashtable-ref state unit 'new)
            [(done) #f]
            [(visiting)
              (fail "the libraries import each other in a cycle:"
                (let loop ([trail (cons unit trail)] [out '()])
                  (if (eq? (car trail) unit)
                    (if (null? out)
                      (loop (cdr trail) (cons (unit-path unit) out))
                      (string-append (unit-path unit) " -> " (let join ([o out]) (if (null? (cdr o)) (car o) (string-append (car o) " -> " (join (cdr o)))))))
                    (loop (cdr trail) (cons (unit-path (car trail)) out)))))]
            [else
              (hashtable-set! state unit 'visiting)
              (for-each (lambda (dep) (visit dep (cons unit trail))) (unit-dependencies unit))
              (hashtable-set! state unit 'done)
              (set! order (cons unit order))]))
        (for-each (lambda (unit) (visit unit '())) units)
        (reverse order))))

  ;; The imports of the program in the file PATH: the `import` forms before the first form that
  ;; is not one.
  (define (program-imports path)
    (guard (e [(message-condition? e) (fail (string-append path ": " (condition-message e)))])
      (call-with-input-file path
        (lambda (port)
          (let loop ([out '()])
            (let ([form (read port)])
              (if (and (pair? form) (eq? (car form) 'import) (list? form))
                (loop (append out (clause-imports (list form))))
                out)))))))

  (define (program-dependencies pkg path units toolchain)
    (let ([table (make-hashtable equal-hash equal?)]
          [seen '()]
          [direct '()])
      (for-each
        (lambda (unit) (for-each (lambda (key) (hashtable-set! table key unit)) (unit-keys unit)))
        units)
      (define (visit unit)
        (unless (memq unit seen)
          (for-each visit (unit-dependencies unit))
          (set! seen (cons unit seen))))
      (for-each
        (lambda (name)
          (let* ([key (view-library-key (package-view pkg) name)]
                 [unit (and key (hashtable-ref table key #f))])
            (cond
              [unit (set! direct (cons unit direct)) (visit unit)]
              [(and key (toolchain-has-library? toolchain key)) #f]
              [else (unknown-library! pkg path name)])))
        (program-imports path))
      (values (reverse seen) (reverse direct)))))
