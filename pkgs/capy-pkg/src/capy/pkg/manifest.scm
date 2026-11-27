;;; Package manifests.

(library (capy pkg manifest)
  (export read-manifest
    read-manifest-tree
    manifest?
    manifest-name
    manifest-version
    manifest-edition
    manifest-provides
    manifest-source
    manifest-dependencies
    manifest-programs
    program-name
    program-file
    program-static?
    program-static-native?
    manifest-no-std?
    manifest-boot-expander
    manifest-boot-package?
    manifest-stage-key
    manifest-source-only
    manifest-native
    extension-name
    extension-kind
    extension-files
    extension-cflags
    extension-link
    extension-archive
    manifest-directory
    manifest-source-directory
    manifest-metadata-ref
    manifest-path-dependencies
    dependency-name
    dependency-clauses
    dependency-path
    dependency-git
    dependency-version
    dependency-rev
    dependency-renames
    pattern-matches?
    provides-match?
    read-workspace-file
    workspace?
    workspace-directory
    workspace-path
    workspace-members
    workspace-exclude
    workspace-version
    workspace-edition
    workspace-dependencies
    workspace-root-package?
    workspace-member-directories
    workspace-covers?
    find-enclosing-workspace)
  (import (rnrs)
    (only (capy) directory-list file-directory? canonicalize-path-string))

  (define-record-type manifest
    (fields name version edition provides source dependencies programs no-std? directory boot-expander source-only
      native metadata))

  ;; True for the package of the runtime itself, the only one that declares `boot-expander`.
  ;; A toolchain build gives that package the library key `core`; see `manifest-stage-key`.
  (define (manifest-boot-package? manifest)
    (not (null? (manifest-boot-expander manifest))))

  ;; The library key of a package while a toolchain is built: `core` for the boot package, `std`
  ;; for every package that ships with it.
  (define (manifest-stage-key manifest)
    (if (manifest-boot-package? manifest) 'core 'std))

  ;; The metadata field KEY (description, license, homepage: a string; authors: a list of
  ;; strings; dist: its clauses), or DEFAULT when the manifest does not have it.
  (define (manifest-metadata-ref manifest key default)
    (let ([entry (assq key (manifest-metadata manifest))])
      (if entry (cdr entry) default)))

  ;; KIND is `cargo`, `c` or `prebuilt`; FILES the directory of the crate, the C sources or the
  ;; library (and the archive after it, for a `prebuilt` that has one); CFLAGS and LINK are lists of
  ;; strings (for `c`).
  (define-record-type extension
    (fields name kind files cflags link))

  ;; The archive a `prebuilt` extension is linked from in a fully static executable, or #f.
  (define (extension-archive ext)
    (and (eq? (extension-kind ext) 'prebuilt) (pair? (cdr (extension-files ext))) (cadr (extension-files ext))))

  (define-record-type dependency
    (fields name clauses))

  ;; DIRECTORY is the root of the workspace, PATH its package.scm. MEMBERS and EXCLUDE are the
  ;; strings of the fields; VERSION and EDITION are #f when the workspace does not give them.
  ;; ROOT-PACKAGE? says that the package.scm is also a package, a member of its own workspace.
  (define-record-type workspace
    (fields directory path members exclude version edition dependencies root-package?))

  ;; A program is (NAME "FILE" OPTION ...), where an option is (static #t), (static #f) or
  ;; (static native): an executable that has its native extensions linked into it.
  (define (program-name program) (car program))
  (define (program-file program) (cadr program))
  (define (program-static? program)
    (let ([option (assq 'static (cddr program))])
      (and option (cadr option) #t)))
  (define (program-static-native? program)
    (let ([option (assq 'static (cddr program))])
      (and option (eq? (cadr option) 'native))))

  (define (manifest-error path message . irritants)
    (apply error 'manifest (string-append path ": " message) irritants))

  ;;; Names and patterns

  (define (name-part? x) (or (symbol? x) (and (exact-integer? x) (>= x 0))))

  ;; A library name is a non-empty list of symbols and non-negative integers; a pattern may
  ;; end in `*`, which stands for every longer name with the same beginning.
  (define (library-name? x)
    (and (pair? x) (list? x) (for-all name-part? x)))

  (define (pattern? x)
    (and (pair? x) (list? x)
      (let loop ([x x])
        (cond
          [(null? (cdr x)) (or (name-part? (car x)))]
          [else (and (name-part? (car x)) (not (eq? (car x) '*)) (loop (cdr x)))]))))

  (define (prefix-pattern? x) (and (pair? x) (eq? (car (last-pair x)) '*)))

  (define (pattern-base x)
    (if (null? (cdr x)) '() (cons (car x) (pattern-base (cdr x)))))

  (define (list-prefix? prefix list)
    (cond
      [(null? prefix) #t]
      [(and (pair? list) (equal? (car prefix) (car list))) (list-prefix? (cdr prefix) (cdr list))]
      [else #f]))

  ;; Whether the library NAME is matched by PATTERN: `(srfi *)` matches `(srfi 1)`
  ;; and `(srfi 1 lists)` but not `(srfi)`.
  (define (pattern-matches? pattern name)
    (if (prefix-pattern? pattern)
      (let ([base (pattern-base pattern)])
        (and (list-prefix? base name) (> (length name) (length base))))
      (equal? pattern name)))

  (define (provides-match? provides name)
    (exists (lambda (pattern) (pattern-matches? pattern name)) provides))

  ;;; Fields

  (define known-fields
    '(name version edition provides source dependencies programs no-std boot-expander source-only native
      description license authors homepage dist workspace))

  (define workspace-fields '(members exclude version edition dependencies))

  (define inherit-marker '((workspace #t)))

  (define (field-ref path fields key default)
    (let ([entry (assq key fields)])
      (cond
        [entry
          (unless (list? entry) (manifest-error path "malformed field" entry))
          (cdr entry)]
        [(eq? default 'required) (manifest-error path "missing required field" key)]
        [else default])))

  (define (single path fields key valid? what default)
    (let ([values (field-ref path fields key default)])
      (cond
        [(eq? values default) default]
        [(and (pair? values) (null? (cdr values)) (valid? (car values))) (car values)]
        [else (manifest-error path (string-append "the field " (symbol->string key) " takes one " what) values)])))

  (define (parse-dependency path spec)
    (unless (and (pair? spec) (symbol? (car spec)) (list? spec))
      (manifest-error path "a dependency is (NAME CLAUSE ...)" spec))
    (for-each
      (lambda (clause)
        (unless (and (pair? clause) (memq (car clause) '(path git version rev rename)) (list? clause))
          (manifest-error path "unknown dependency clause" (car spec) clause))
        (case (car clause)
          [(path git version rev)
            (unless (and (pair? (cdr clause)) (null? (cddr clause)) (string? (cadr clause)))
              (manifest-error path "the clause takes one string" (car spec) clause))]
          [(rename)
            (unless (and (pair? (cdr clause)) (null? (cddr clause)) (list? (cadr clause))
                     (for-all (lambda (pair)
                               (and (list? pair) (= (length pair) 2)
                                 (library-name? (car pair)) (library-name? (cadr pair))))
                       (normalize-renames (cadr clause))))
              (manifest-error path "rename takes a list of ((FROM ...) (TO ...)) pairs" (car spec) clause))]))
      (cdr spec))
    (let ([kinds (map car (cdr spec))])
      (unless (or (memq 'path kinds) (memq 'git kinds))
        (manifest-error path "a dependency needs a path or a git clause" (car spec)))
      (when (and (memq 'path kinds) (memq 'git kinds))
        (manifest-error path "a dependency is a path or a git dependency, not both" (car spec)))
      (for-each
        (lambda (kind)
          (when (> (length (filter (lambda (k) (eq? k kind)) kinds)) 1)
            (manifest-error path "a clause is given twice" (car spec) kind)))
        '(path git version rev rename))
      (when (and (memq 'path kinds) (or (memq 'version kinds) (memq 'rev kinds)))
        (manifest-error path "a path dependency has no version or rev" (car spec)))
      (when (and (memq 'git kinds) (not (or (memq 'version kinds) (memq 'rev kinds))))
        (manifest-error path "a git dependency needs a version or a rev" (car spec)))
      (when (and (memq 'version kinds) (memq 'rev kinds))
        (manifest-error path "a dependency has a version or a rev, not both" (car spec)))
      (let ([clause (assq 'version (cdr spec))])
        (when (and clause (not (version-text? (cadr clause))))
          (manifest-error path "a version is numbers separated by dots: \"1\", \"1.2\" or \"1.2.3\""
            (car spec) clause))))
    (make-dependency (car spec) (cdr spec)))

  (define (version-text? text)
    (let loop ([i 0] [start 0] [parts 0])
      (cond
        [(= i (string-length text))
          (and (> i start) (< parts 3))]
        [(char=? (string-ref text i) #\.)
          (and (> i start) (< parts 2) (loop (+ i 1) (+ i 1) (+ parts 1)))]
        [(char<=? #\0 (string-ref text i) #\9) (loop (+ i 1) start parts)]
        [else #f])))

  (define (clause-string dependency key)
    (let ([clause (assq key (dependency-clauses dependency))])
      (and clause (cadr clause))))

  (define (dependency-git dependency) (clause-string dependency 'git))
  (define (dependency-version dependency) (clause-string dependency 'version))
  (define (dependency-rev dependency) (clause-string dependency 'rev))

  ;; The renames of a dependency as a list of (FROM . TO) pairs of library names.
  (define (dependency-renames dependency)
    (let ([clause (assq 'rename (dependency-clauses dependency))])
      (if clause
        (map (lambda (pair) (cons (car pair) (cadr pair))) (normalize-renames (cadr clause)))
        '())))

  ;; A list of ((FROM ...) (TO ...)) pairs, or one pair.
  (define (normalize-renames renames)
    (if (and (pair? renames) (pair? (car renames)) (not (pair? (car (car renames)))))
      (list renames)
      renames))

  (define (dependency-path dependency)
    (let ([clause (assq 'path (dependency-clauses dependency))])
      (and clause (cadr clause))))

  ;;; Files

  (define (join-path directory file)
    (if (and (> (string-length file) 0) (char=? (string-ref file 0) #\/))
      file
      (string-append directory "/" file)))

  (define (directory-of path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) "."]
        [(char=? (string-ref path i) #\/) (if (= i 0) "/" (substring path 0 i))]
        [else (loop (- i 1))])))

  (define (read-datums path)
    (call-with-input-file path
      (lambda (port)
        (let loop ([out '()])
          (let ([x (read port)])
            (if (eof-object? x) (reverse out) (loop (cons x out))))))))

  ;; The datum of the root file PATH: `(package ...)` or `(workspace ...)`.
  (define (root-datum path)
    (unless (file-exists? path) (manifest-error path "no such file"))
    (let ([datums (read-datums path)])
      (unless (and (pair? datums) (null? (cdr datums))
               (pair? (car datums)) (memq (car (car datums)) '(package workspace))
               (list? (car datums)))
        (manifest-error path "expected one (package ...) or (workspace ...) form"))
      (car datums)))

  (define (check-unique-fields! path fields)
    (let loop ([seen '()] [rest fields])
      (when (pair? rest)
        (when (memq (car (car rest)) seen)
          (manifest-error path "field given twice" (car (car rest))))
        (loop (cons (car (car rest)) seen) (cdr rest)))))

  (define (parse-workspace path fields root-package?)
    (for-each
      (lambda (field)
        (unless (and (pair? field) (list? field) (memq (car field) workspace-fields))
          (manifest-error path "unknown field of the workspace" field)))
      fields)
    (check-unique-fields! path fields)
    (let ([strings (lambda (key)
                     (let ([v (field-ref path fields key '())])
                       (unless (for-all string? v)
                         (manifest-error path (string-append "the field " (symbol->string key) " of the workspace lists directories") v))
                       v))])
      (let ([members (strings 'members)]
            [exclude (strings 'exclude)]
            [version (single path fields 'version string? "string" #f)]
            [edition (single path fields 'edition (lambda (x) (memq x '(r6rs r7rs))) "of r6rs and r7rs" #f)]
            [dependencies (map (lambda (spec) (parse-dependency path spec))
                            (field-ref path fields 'dependencies '()))])
        (when (and (null? members) (not root-package?))
          (manifest-error path "a workspace needs members"))
        (make-workspace (canonicalize-path-string (directory-of path)) path members exclude version
          edition dependencies root-package?))))

  ;; The workspace the file PATH declares, or #f when it declares none. The package in it, if any,
  ;; is not checked.
  (define (read-workspace-file path)
    (and (file-exists? path)
      (let ([datum (root-datum path)])
        (if (eq? (car datum) 'workspace)
          (parse-workspace path (cdr datum) #f)
          (let ([field (assq 'workspace (cdr datum))])
            (and field (list? field) (parse-workspace path (cdr field) #t)))))))

  (define (sub-directories dir)
    (filter (lambda (name)
              (and (not (zero? (string-length name))) (not (char=? (string-ref name 0) #\.))
                (file-directory? (join-path dir name))))
      (list-sort string<? (directory-list dir))))

  ;; The directories PATTERN names below the root DIR: `D/*` is every D/X that has a package.scm,
  ;; anything else the directory itself (which must have one when MUST-EXIST?).
  (define (expand-member ws pattern must-exist?)
    (let* ([dir (workspace-directory ws)]
           [n (string-length pattern)])
      (cond
        [(or (string=? pattern "*")
             (and (>= n 2) (string=? (substring pattern (- n 2) n) "/*")))
          (let ([base (if (string=? pattern "*") dir (join-path dir (substring pattern 0 (- n 2))))])
            (if (and (file-exists? base) (file-directory? base))
              (filter (lambda (d) (file-exists? (join-path d "package.scm")))
                (map (lambda (name) (join-path base name)) (sub-directories base)))
              '()))]
        [else
          (let ([member (join-path dir pattern)])
            (when (and must-exist? (not (file-exists? (join-path member "package.scm"))))
              (manifest-error (workspace-path ws) "the member has no package.scm" pattern))
            (list member))])))

  (define (canonical dir) (if (file-exists? dir) (canonicalize-path-string dir) dir))

  ;; The directories of the members of WS, in the order of `members` (the root package last), as
  ;; joined to the root: not canonical, so that a tree given by a relative path stays relative.
  (define (workspace-member-directories ws)
    (let* ([excluded (map canonical
                       (apply append (map (lambda (p) (expand-member ws p #f)) (workspace-exclude ws))))]
           [listed (apply append (map (lambda (p) (expand-member ws p #t)) (workspace-members ws)))]
           [members
             (let loop ([dirs listed] [seen '()] [out '()])
               (cond
                 [(null? dirs) (reverse out)]
                 [else
                   (let ([c (canonical (car dirs))])
                     (if (or (member c seen) (member c excluded)
                             (string=? c (workspace-directory ws)))
                       (loop (cdr dirs) (cons c seen) out)
                       (loop (cdr dirs) (cons c seen) (cons (car dirs) out))))]))])
      (for-each
        (lambda (dir)
          (when (read-workspace-file (join-path dir "package.scm"))
            (manifest-error (join-path dir "package.scm")
              "a member of a workspace cannot be a workspace itself; the workspace is"
              (workspace-path ws))))
        members)
      (if (workspace-root-package? ws)
        (append members (list (workspace-directory ws)))
        members)))

  ;; Whether DIR is a member of WS.
  (define (workspace-covers? ws dir)
    (let ([dir (canonical dir)])
      (exists (lambda (m) (string=? (canonical m) dir)) (workspace-member-directories ws))))

  ;; The workspace whose members include the package in DIR: the nearest one above it. #f when
  ;; there is none.
  (define (find-enclosing-workspace dir)
    (let ([dir (canonical dir)])
      (let loop ([up (directory-of dir)])
        (let ([ws (read-workspace-file (join-path up "package.scm"))])
          (cond
            [(and ws (workspace-covers? ws dir)) ws]
            [(or (string=? up "/") (string=? up ".") (string=? (directory-of up) up)) #f]
            [else (loop (directory-of up))])))))

  ;; The manifest in the file PATH, checked on its own. What it inherits is taken from its
  ;; workspace.
  (define (read-manifest path)
    (let ([datum (root-datum path)])
      (when (eq? (car datum) 'workspace)
        (manifest-error path "this is a workspace, not a package; name a member with -p"))
      (let ([fields (cdr datum)] [directory (directory-of path)])
        (for-each
          (lambda (field)
            (unless (and (pair? field) (memq (car field) known-fields))
              (manifest-error path "unknown field" field)))
          fields)
        (check-unique-fields! path fields)
        (let* ([ws-cache #f]
               [ws (lambda (what)
                     (or ws-cache
                       (let ([found (or (read-workspace-file path) (find-enclosing-workspace directory))])
                         (unless found
                           (manifest-error path
                             (string-append what " is inherited with (workspace #t), but the package is not a member of a workspace")))
                         (set! ws-cache found)
                         found)))]
               [inherit (lambda (fields key accessor)
                          (let ([entry (assq key fields)])
                            (if (and entry (equal? (cdr entry) inherit-marker))
                              (let ([value (accessor (ws (symbol->string key)))])
                                (unless value
                                  (manifest-error path (string-append "the workspace " (workspace-path (ws "")) " has no "
                                                         (symbol->string key) " to inherit")))
                                (map (lambda (f) (if (eq? f entry) (list key value) f)) fields))
                              fields)))]
               [fields (inherit fields 'version workspace-version)]
               [fields (inherit fields 'edition workspace-edition)]
               [dependency-specs
                 (map (lambda (spec)
                        (if (and (pair? spec) (list? spec) (member '(workspace #t) (cdr spec)))
                          (inherit-dependency path spec (ws (string-append "the dependency " (format-name spec))))
                          spec))
                   (field-ref path fields 'dependencies '()))])
          (parse-package path fields directory dependency-specs)))))

  (define (format-name spec)
    (if (symbol? (car spec)) (symbol->string (car spec)) "?"))

  ;; The spec SPEC `(NAME (workspace #t) [(rename ...)])` with the clauses of the dependency NAME of
  ;; the workspace WS. A path is made absolute, since it is relative to the root of the workspace.
  (define (inherit-dependency path spec ws)
    (let ([others (filter (lambda (c) (not (equal? c '(workspace #t)))) (cdr spec))])
      (unless (for-all (lambda (c) (and (pair? c) (eq? (car c) 'rename))) others)
        (manifest-error path "a dependency inherited with (workspace #t) may only add a rename clause" spec))
      (let ([found (find (lambda (d) (eq? (dependency-name d) (car spec))) (workspace-dependencies ws))])
        (unless found
          (manifest-error path (string-append "the workspace " (workspace-path ws) " has no dependency "
                                 (format-name spec) " to inherit")))
        (cons (car spec)
          (append
            (map (lambda (clause)
                   (if (eq? (car clause) 'path)
                     (list 'path (join-path (workspace-directory ws) (cadr clause)))
                     clause))
              (filter (lambda (c) (not (and (pair? others) (eq? (car c) 'rename))))
                (dependency-clauses found)))
            others)))))

  (define (parse-package path fields directory dependency-specs)
        (let* ([name (single path fields 'name symbol? "symbol" 'required)]
               [version (single path fields 'version string? "string" 'required)]
               [edition (single path fields 'edition (lambda (x) (memq x '(r6rs r7rs))) "of r6rs and r7rs" 'required)]
               [provides (field-ref path fields 'provides 'required)]
               [source (single path fields 'source string? "string" 'required)]
               [dependencies (map (lambda (spec) (parse-dependency path spec)) dependency-specs)]
               [programs (field-ref path fields 'programs '())]
               [no-std (single path fields 'no-std boolean? "boolean" #f)]
               [boot-expander (field-ref path fields 'boot-expander '())]
               [source-only (field-ref path fields 'source-only '())]
               [native (parse-native path (field-ref path fields 'native '()))]
               [metadata (parse-metadata path fields)])
          (for-each
            (lambda (name)
              (unless (library-name? name) (manifest-error path "source-only lists library names" name)))
            source-only)
          (for-each
            (lambda (file)
              (unless (and (string? file) (file-exists? (join-path (join-path directory source) file)))
                (manifest-error path "boot-expander lists files of the source directory" file)))
            boot-expander)
          (when (null? provides) (manifest-error path "provides needs at least one pattern"))
          (for-each
            (lambda (pattern)
              (unless (pattern? pattern) (manifest-error path "bad pattern in provides" pattern)))
            provides)
          (for-each
            (lambda (program)
              (unless (and (list? program) (>= (length program) 2) (symbol? (car program)) (string? (cadr program))
                       (for-all (lambda (option)
                                 (and (pair? option) (eq? (car option) 'static) (list? option)
                                   (= (length option) 2) (or (boolean? (cadr option)) (eq? (cadr option) 'native))))
                         (cddr program)))
                (manifest-error path "a program is (NAME \"FILE\" [(static BOOLEAN)]) or (NAME \"FILE\" (static native))" program))
              (unless (file-exists? (join-path directory (cadr program)))
                (manifest-error path "no such program file" (cadr program))))
            programs)
          (let loop ([deps dependencies] [seen '()])
            (when (pair? deps)
              (when (memq (dependency-name (car deps)) seen)
                (manifest-error path "dependency given twice" (dependency-name (car deps))))
              (loop (cdr deps) (cons (dependency-name (car deps)) seen))))
          (unless (let ([dir (join-path directory source)]) (and (file-exists? dir) (file-directory? dir)))
            (manifest-error path "the source directory does not exist" source))
          (make-manifest name version edition provides source dependencies programs
            (and no-std #t) directory boot-expander source-only native metadata)))

  ;; The metadata fields of FIELDS, as (KEY . VALUE): see `manifest-metadata-ref`.
  (define (parse-metadata path fields)
    (define (strings? x) (and (list? x) (for-all string? x)))
    (append
      (filter values
        (map (lambda (key)
               (let ([value (single path fields key string? "string" #f)])
                 (and value (cons key value))))
          '(description license homepage)))
      (let ([authors (field-ref path fields 'authors #f)])
        (cond
          [(not authors) '()]
          [(and (pair? authors) (strings? authors)) (list (cons 'authors authors))]
          [else (manifest-error path "authors lists strings" authors)]))
      (let ([dist (field-ref path fields 'dist #f)])
        (cond
          [(not dist) '()]
          [else
            (for-each
              (lambda (clause)
                (unless (and (pair? clause) (memq (car clause) '(deb rpm)) (list? clause)
                         (for-all (lambda (entry)
                                   (and (pair? entry) (memq (car entry) '(depends requires)) (strings? (cdr entry))))
                           (cdr clause)))
                  (manifest-error path "dist is (deb (depends \"PKG\" ...)) (rpm (requires \"PKG\" ...))" clause)))
              dist)
            (list (cons 'dist dist))]))))

  ;; The extensions of the `native` field: a list of (NAME SPEC). The files are not looked for
  ;; here (the installed copy of a package, the toolchain's, has no sources to build from).
  (define (parse-native path entries)
    (define (strings? x) (and (list? x) (for-all string? x)))
    (define (option spec key)
      (let loop ([rest spec] [found #f])
        (cond
          [(null? rest) (or found '())]
          [(and (pair? (car rest)) (eq? (car (car rest)) key))
            (when found (manifest-error path "an option is given twice" spec key))
            (unless (strings? (cdr (car rest))) (manifest-error path "the option takes strings" (car rest)))
            (loop (cdr rest) (cdr (car rest)))]
          [else (loop (cdr rest) found)])))
    (let loop ([entries entries] [seen '()] [out '()])
      (cond
        [(null? entries) (reverse out)]
        [else
          (let ([entry (car entries)])
            (unless (and (list? entry) (= (length entry) 2) (symbol? (car entry))
                     (pair? (cadr entry)) (list? (cadr entry)) (symbol? (car (cadr entry))))
              (manifest-error path "an extension is (NAME (cargo \"DIR\")), (NAME (c \"FILE\" ...)) or (NAME (prebuilt \"FILE\"))" entry))
            (when (memq (car entry) seen) (manifest-error path "extension given twice" (car entry)))
            (let* ([name (car entry)] [spec (cadr entry)] [kind (car spec)] [args (cdr spec)])
              (loop (cdr entries) (cons name seen)
                (cons
                  (case kind
                    [(cargo)
                      (unless (and (pair? args) (null? (cdr args)) (string? (car args)))
                        (manifest-error path (string-append "the extension " (symbol->string name)
                                               " takes one string: cargo")
                          spec))
                      (make-extension name kind args '() '())]
                    [(prebuilt)
                      (unless (and (pair? args) (for-all string? args) (<= (length args) 2))
                        (manifest-error path (string-append "the extension " (symbol->string name)
                                               " takes a library and, after it, an archive: (prebuilt \"FILE\" [\"ARCHIVE\"])")
                          spec))
                      (make-extension name kind args '() '())]
                    [(c)
                      (let ([files (filter string? args)]
                            [options (filter pair? args)])
                        (when (null? files) (manifest-error path "a c extension needs source files" spec))
                        (unless (= (+ (length files) (length options)) (length args))
                          (manifest-error path "a c extension is files and (cflags ...) or (link ...)" spec))
                        (for-each
                          (lambda (o)
                            (unless (memq (car o) '(cflags link))
                              (manifest-error path "unknown option of a c extension" o)))
                          options)
                        (make-extension name 'c files (option options 'cflags) (option options 'link)))]
                    [else (manifest-error path "an extension is cargo, c or prebuilt" entry)])
                  out))))])))

  (define (manifest-source-directory manifest)
    (join-path (manifest-directory manifest) (manifest-source manifest)))

  ;;; Trees of packages

  ;; The path dependencies of MANIFEST, resolved against the manifests MANIFESTS of the tree: a
  ;; list of (dependency . manifest) pairs.
  (define (manifest-path-dependencies manifest manifests)
    (let ([who (string-append (manifest-directory manifest) "/package.scm")])
      (let loop ([deps (manifest-dependencies manifest)] [out '()])
        (cond
          [(null? deps) (reverse out)]
          [(dependency-path (car deps))
            => (lambda (relative)
                (let* ([target (canonicalize-path-string (join-path (manifest-directory manifest) relative))]
                       [found (find (lambda (m)
                                     (string=? (canonicalize-path-string (manifest-directory m)) target))
                               manifests)])
                  (unless found
                    (manifest-error who "the path dependency is not a package of the tree"
                      (dependency-name (car deps)) relative))
                  (loop (cdr deps) (cons (cons (car deps) found) out))))]
          [else (loop (cdr deps) out)]))))

  ;; The members of the workspace DIR/package.scm, or every package DIR/*/package.scm when DIR
  ;; has no workspace, dependencies first (in the order of the members otherwise). Checks the
  ;; dependencies.
  (define (read-manifest-tree dir)
    (let* ([ws (read-workspace-file (join-path dir "package.scm"))]
           [dirs (if ws
                   ;; joined to DIR as it is given, not to the canonical root
                   (map (lambda (member)
                          (let ([root (workspace-directory ws)])
                            (if (and (> (string-length member) (string-length root))
                                     (string=? (substring member 0 (string-length root)) root))
                              (string-append dir (substring member (string-length root) (string-length member)))
                              member)))
                     (workspace-member-directories ws))
                   ;; a workspace below DIR is a tree of its own
                   (filter (lambda (d) (and (file-exists? (join-path d "package.scm"))
                                         (not (read-workspace-file (join-path d "package.scm")))))
                     (map (lambda (name) (string-append dir "/" name)) (list-sort string<? (directory-list dir)))))]
           [manifests (map (lambda (d) (read-manifest (join-path d "package.scm"))) dirs)])
      (let loop ([ms manifests] [seen '()])
        (when (pair? ms)
          (when (memq (manifest-name (car ms)) seen)
            (manifest-error dir "two packages have the same name" (manifest-name (car ms))))
          (loop (cdr ms) (cons (manifest-name (car ms)) seen))))
      ;; Depth-first, so that a package comes after its dependencies; a package met again
      ;; while it is being visited is a cycle.
      (let ([done '()] [visiting '()] [order '()])
        (define (visit m)
          (cond
            [(memq m done) #f]
            [(memq m visiting)
              (manifest-error (string-append (manifest-directory m) "/package.scm")
                "the dependencies form a cycle" (manifest-name m))]
            [else
              (set! visiting (cons m visiting))
              (for-each (lambda (pair) (visit (cdr pair))) (manifest-path-dependencies m manifests))
              (set! visiting (remq m visiting))
              (set! done (cons m done))
              (set! order (cons m order))]))
        (for-each visit manifests)
        (reverse order))))

  (define (remq x list)
    (cond [(null? list) '()] [(eq? x (car list)) (remq x (cdr list))] [else (cons (car list) (remq x (cdr list)))]))

  (define (last-pair x) (if (pair? (cdr x)) (last-pair (cdr x)) x)))
