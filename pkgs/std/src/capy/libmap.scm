;;; Builds libmaps.
;;;
;;; A libmap says where the compiled form of every library is, and what names the code of each
;;; package can import (see "The libmap" and "Library keys of packages, and views" in
;;; boot/linker.scm). This library writes one for the compiled files of a directory tree. It
;;; reads the directory headers of the files, `%fasl-directory`, and never runs any library
;;; code, so it is cheap and cannot be affected by the libraries it describes.
;;;
;;;   (write-libmap! "stage-1/capy.libmap" '("stage-1/core/fasl") '(("pkgs/core/src" . core)) '())
;;;
;;; The toolchain's own libmap is made like that by `write-toolchain-libmap!` in (capy pkg dist).
;;; A package manager passes the views of its packages too, built with `make-package-view`.
;;;
;;; A package manager that compiles library by library (the `capy` driver, which writes the
;;; libmap of a build after each file it compiles) makes the libmap from entries instead:
;;; `read-libmap` reads one, `fasl-libraries` lists what a compiled file holds, `make-libmap`
;;; and `write-libmap-file!` put the entries and the views together.

(library (capy libmap)
  (export build-libmap
    write-libmap!
    make-package-view
    libmap-library-keys
    read-libmap
    make-libmap
    write-libmap-file!
    libmap-entries
    libmap-views
    libmap-native
    native-entries
    libmap-compiler-id
    libmap-view
    view-library-key
    view-package
    view-entries
    relative-path
    fasl-libraries
    decl-key
    decl-exports
    decl-knowns
    decl-source-hash
    decl-interface-hash
    decl-deps
    decl-compiler-id
    decl-compile-deps
    decl-compile-deps-current?
    file-hash
    source-file-family
    prefer-edition-sources)
  (import (rnrs)
    (rnrs hashtables)
    (rnrs sorting)
    (capy))

  ;;; Files

  (define (string-suffix? s suffix)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (string-prefix? s prefix)
    (let ([n (string-length s)] [m (string-length prefix)])
      (and (>= n m) (string=? (substring s 0 m) prefix))))

  ;; The files under DIR whose name ends in one of the SUFFIXES, sorted, as paths that start
  ;; with DIR.
  (define (files-under dir suffixes)
    (let walk ([dir dir])
      (apply append
        (map (lambda (name)
              (let ([path (string-append dir "/" name)])
                (cond
                  [(file-directory? path) (walk path)]
                  [(exists (lambda (suffix) (string-suffix? name suffix)) suffixes) (list path)]
                  [else '()])))
          (list-sort string<? (directory-list dir))))))

  ;; The language of a source file from its extension: r7rs for `.sld`, r6rs for `.sls` and
  ;; `.sps`, #f for the others (`.scm` is written in the edition of the package).
  (define (source-file-family path)
    (cond
      [(string-suffix? path ".sld") 'r7rs]
      [(or (string-suffix? path ".sls") (string-suffix? path ".sps")) 'r6rs]
      [else #f]))

  ;; FILES without the ones that have a twin of the language EDITION: `foo.sls` is dropped when
  ;; `foo.sld` is there and EDITION is r7rs, with a warning that names both. A package holding
  ;; both files is portable to the two standards; its edition says which one Capy builds. With
  ;; EDITION #f nothing is dropped. Variants (`foo.capy.sld`) have a stem of their own and are
  ;; never twins.
  (define (prefer-edition-sources files edition)
    (if (not edition)
      files
      (let ([stems (make-hashtable string-hash string=?)]
            [stem (lambda (path) (substring path 0 (- (string-length path) 4)))])
        (for-each
          (lambda (file)
            (when (eq? (source-file-family file) edition)
              (hashtable-set! stems (stem file) file)))
          files)
        (filter
          (lambda (file)
            (let ([family (source-file-family file)])
              (cond
                [(or (not family) (eq? family edition)) #t]
                [(hashtable-ref stems (stem file) #f)
                  => (lambda (twin)
                      (let ([port (current-error-port)])
                        (put-string port "warning: ")
                        (put-string port twin)
                        (put-string port " and ")
                        (put-string port file)
                        (put-string port " define the same library; using the first (edition ")
                        (put-string port (symbol->string edition))
                        (put-string port ")\n"))
                      #f)]
                [else #t])))
          files))))

  ;; The components of the absolute path PATH.
  (define (path-components path)
    (let loop ([chars (string->list path)] [current '()] [out '()])
      (define (flush) (if (null? current) out (cons (list->string (reverse current)) out)))
      (cond
        [(null? chars) (reverse (flush))]
        [(char=? (car chars) #\/) (loop (cdr chars) '() (flush))]
        [else (loop (cdr chars) (cons (car chars) current) out)])))

  ;; PATH relative to DIRECTORY, with `..` where it has to leave the directory, so that a
  ;; libmap and the files it names can be moved together.
  (define (relative-path path directory)
    (let loop ([path (path-components (canonicalize-path-string path))]
               [base (path-components (canonicalize-path-string directory))])
      (cond
        [(and (pair? path) (pair? base) (string=? (car path) (car base)))
          (loop (cdr path) (cdr base))]
        [else
          (let ([parts (append (map (lambda (_) "..") base) path)])
            (if (null? parts)
              "."
              (let join ([parts parts])
                (if (null? (cdr parts))
                  (car parts)
                  (string-append (car parts) "/" (join (cdr parts)))))))])))

  ;;; The sources
  ;;;
  ;;; The source of a library is found by reading the headers of the files of a package: the
  ;;; `(library name ...)` forms at the top of a file, and nothing else is read, so no library
  ;;; code is run and none is expanded.

  ;; The name of a library form without its version, `(a b (1 2))` -> `(a b)`.
  (define (library-name-without-version name)
    (if (and (pair? name) (pair? (cdr name)) (let last? ([l name]) (if (null? (cdr l)) (list? (car l)) (last? (cdr l)))))
      (let drop-last ([l name])
        (if (null? (cdr l)) '() (cons (car l) (drop-last (cdr l)))))
      name))

  ;; The names of the libraries a source file defines: the `library` and `define-library` forms at
  ;; its top.
  (define (source-library-names path)
    (call-with-input-file path
      (lambda (port)
        (let loop ([out '()])
          (let ([form (read port)])
            (if (and (pair? form) (memq (car form) '(library define-library)) (pair? (cdr form)) (pair? (cadr form)))
              (loop (cons (library-name-without-version (cadr form)) out))
              (reverse out)))))))

  ;; The `(key . source-path)` of the libraries defined by the files under SRC-DIR, which hold
  ;; the libraries of the package whose libraries have the keys made by KEY-ID. EDITION (r6rs,
  ;; r7rs or #f) chooses between an `.sld` and an `.sls` of one library, see
  ;; `prefer-edition-sources`. A key in two other files is an error.
  (define (source-entries src-dir key-id edition)
    (let loop ([files (prefer-edition-sources (files-under src-dir '(".scm" ".sld" ".sls")) edition)]
               [out '()])
      (if (null? files)
        (reverse out)
        (loop (cdr files)
          (let inner ([names (source-library-names (car files))] [out out])
            (if (null? names)
              out
              (let ([key (linker-package-key key-id (car names))])
                (cond
                  [(assoc key out)
                    => (lambda (other)
                        (error 'build-libmap "library is defined in two source files"
                          key (cdr other) (car files)))]
                  [else (inner (cdr names) (cons (cons key (car files)) out))]))))))))

  ;;; The compiled files

  ;; The compiled libraries in the file FASL as a list of `(key index decl wpo)`. DECL is the
  ;; decl of the library, #f for a boot file, which has none. WPO is the `.wpo` file when it was
  ;; made by the same compilation as the file, else #f.
  (define (file-libraries fasl)
    (let ([dir (%fasl-directory fasl)]
          [decls (fasl-library-decls fasl)])
      (if (not dir)
        '()
        (let ([wpo (wpo-path fasl)])
          (let loop ([entries dir] [index 0] [out '()])
            (if (null? entries)
              (reverse out)
              (let* ([entry (car entries)]
                     [meta (cadddr entry)]
                     [decl? (and (vector? meta) (> (vector-length meta) 0)
                              (eq? (vector-ref meta 0) '|#%library|))]
                     ;; the meta of a boot linklet is `(#%boot key)`
                     [boot? (and (pair? meta) (eq? (car meta) '|#%boot|))])
                (loop (cdr entries) (+ index 1)
                  (if (or decl? boot?)
                    (cons
                      (list (if decl? (library-decl-key meta) (car entry)) index
                        (and decl? meta)
                        (and decl? decls (file-exists? wpo) (wpo-current? wpo decls) wpo))
                      out)
                    out)))))))))

  ;; Whether the `.wpo` file WPO was left by the compilation that made the decls DECLS.
  (define (wpo-current? wpo decls)
    (let ([wpo-decls (fasl-library-decls wpo)])
      (and wpo-decls (pair? wpo-decls)
        (equal? (library-compile-id wpo-decls) (library-compile-id decls)))))

  ;; The compiled libraries under the directories DIRS as `(key fasl-path index decl wpo)`. A
  ;; library in two files is an error.
  (define (compiled-libraries dirs)
    (let ([seen (make-hashtable equal-hash equal?)])
      (let loop ([files (apply append (map (lambda (dir) (files-under dir '(".fasl"))) dirs))]
                 [out '()])
        (if (null? files)
          (reverse out)
          (loop (cdr files)
            (let inner ([libs (file-libraries (car files))] [out out])
              (cond
                [(null? libs) out]
                [(hashtable-ref seen (car (car libs)) #f)
                  => (lambda (other)
                      (error 'build-libmap "library is in two compiled files"
                        (car (car libs)) other (car files)))]
                [else
                  (hashtable-set! seen (car (car libs)) (car files))
                  (inner (cdr libs) (cons (cons (car (car libs)) (cons (car files) (cdr (car libs)))) out))])))))))

  ;;; Which compiled libraries are current
  ;;;
  ;;; The libmap trusts the compiled file of a library it lists, so it lists only the ones that
  ;;; are current: made by this compiler (`%compiler-id`), from the source as it is now, and
  ;;; against the libraries as they are now. The others are listed without a compiled file and
  ;;; are compiled from their source when something imports them.

  (define (decl-current? decl source deps-hash)
    (and (equal? (library-decl-compiler-id decl) (%compiler-id))
      (or (not source)
        (equal? (library-decl-source-hash decl) (%file-hash source)))
      (compile-dependencies-current? (library-decl-compile-deps decl) source)
      (for-all
        (lambda (dep)
          (let ([hash (hashtable-ref deps-hash (car dep) 'unknown)])
            ;; a library the libmap does not know is not one of the toolchain's
            (or (eq? hash 'unknown) (equal? hash (cdr dep)))))
        (library-decl-deps decl))))

  ;; The compiled libraries of COMPILED, `(key fasl-path index decl wpo)`, that are current,
  ;; given SOURCES, the `(key . source-path)` pairs. Whether a library is current depends on
  ;; whether the ones it imports are, so this is repeated until nothing changes.
  (define (current-libraries compiled sources)
    (let loop ([live compiled])
      (let ([hashes (make-hashtable equal-hash equal?)])
        ;; the interface hash of every library whose compiled file is believed current; a library
        ;; the libmap knows (it has a source) and has no such file has the hash #f, which no
        ;; dependency matches.
        (for-each (lambda (source) (hashtable-set! hashes (car source) #f)) sources)
        (for-each
          (lambda (lib)
            (if (cadddr lib)
              (hashtable-set! hashes (car lib) (library-decl-interface-hash (cadddr lib)))))
          live)
        (let ([kept (filter
                      (lambda (lib)
                        (or (not (cadddr lib))
                          (decl-current? (cadddr lib)
                            (let ([s (assoc (car lib) sources)]) (and s (cdr s)))
                            hashes)))
                      live)])
          (if (= (length kept) (length live)) kept (loop kept))))))

  ;;; The entries

  ;; The source file of the boot library KEY, `(boot name)`, in one of the SOURCE-DIRS, or #f.
  (define (boot-source key source-dirs)
    (and (pair? key) (eq? (car key) 'boot) (pair? (cdr key)) (null? (cddr key))
      (let ([file (string-append "boot/" (symbol->string (cadr key)) ".scm")])
        (let loop ([dirs source-dirs])
          (cond
            [(null? dirs) #f]
            [(file-exists? (string-append (car dirs) "/" file)) (string-append (car dirs) "/" file)]
            [else (loop (cdr dirs))])))))

  ;; The entries `(key source fasl index interface-hash wpo)` of the libraries of the packages
  ;; whose sources are in SOURCE-ROOTS, `(dir . key-id)` or `(dir key-id edition)` lists, and of the compiled files in
  ;; the directories FASL-DIRS. The paths are relative to DIRECTORY.
  (define (library-entries fasl-dirs source-roots directory)
    (let* ([sources (apply append
                      (map (lambda (root)
                             ;; `(dir . key-id)`, or `(dir key-id edition)`
                             (if (pair? (cdr root))
                               (source-entries (car root) (cadr root) (caddr root))
                               (source-entries (car root) (cdr root) #f)))
                        source-roots))]
           [_ (let ([seen (make-hashtable equal-hash equal?)])
                (for-each
                  (lambda (source)
                    (if (hashtable-ref seen (car source) #f)
                      (error 'build-libmap "library is defined in two packages" (car source)))
                    (hashtable-set! seen (car source) #t))
                  sources))]
           [compiled (current-libraries (compiled-libraries fasl-dirs) sources)]
           [rel (lambda (path) (and path (relative-path path directory)))]
           [source-dirs (map car source-roots)]
           [entry (lambda (key source lib)
                    (list key (rel source)
                      (and lib (rel (cadr lib)))
                      (and lib (caddr lib))
                      (and lib (cadddr lib) (library-decl-interface-hash (cadddr lib)))
                      (and lib (rel (car (cddddr lib))))))])
      (append
        (map (lambda (source) (entry (car source) (cdr source) (assoc (car source) compiled)))
          sources)
        ;; the compiled libraries no source was found for: the boot files, whose source is
        ;; `boot/NAME.scm`, and the libraries of a tree that was only compiled
        (map (lambda (lib) (entry (car lib) (boot-source (car lib) source-dirs) lib))
          (filter (lambda (lib) (not (assoc (car lib) sources))) compiled)))))

  (define (libmap-library-keys libmap)
    (map car (vector-ref libmap 3)))

  ;;; Views
  ;;;
  ;;; (make-package-view id provides deps no-std? [key-id])
  ;;;
  ;;;   id        the package id, a symbol such as |github.com/x/json@1|
  ;;;   provides  the library name patterns the package provides: (json) is that library,
  ;;;             (json *) every library below it
  ;;;   deps      a list of (id provides renames): the dependencies of the package, what
  ;;;             they provide, and (from . to) pairs that put the names beginning with the
  ;;;             list FROM under the list TO instead
  ;;;   no-std?   #t leaves the standard libraries out: the view shows only what the package
  ;;;             and the dependencies in DEPS provide, so a package that has no std lists the
  ;;;             package `core` among its dependencies. Otherwise every other name is a name
  ;;;             of `std`.
  ;;;   key-id    the package id the libraries of this package have in their keys, ID by
  ;;;             default. The packages of the toolchain (`core`, `std` and the packages that
  ;;;             ship with them) pass `core` or `std`, which have plain keys.
  ;;;
  ;;; Two dependencies that provide the same name are an error, the message suggests the
  ;;; `rename` that resolves it. What the package provides itself takes precedence. Besides
  ;;; PROVIDES the package provides `(capy c gen *)` to itself: the libraries its code generates
  ;;; into the compile output directory (the `core` package has none).

  (define (pattern-prefix? pattern)
    (and (pair? pattern)
      (if (null? (cdr pattern)) (eq? (car pattern) '*) (pattern-prefix? (cdr pattern)))))

  (define (pattern-base pattern)
    (if (null? (cdr pattern)) '() (cons (car pattern) (pattern-base (cdr pattern)))))

  ;; The rest of LIST after the list PREFIX, or #f.
  (define (list-tail-after prefix list)
    (cond
      [(null? prefix) list]
      [(and (pair? list) (equal? (car prefix) (car list))) (list-tail-after (cdr prefix) (cdr list))]
      [else #f]))

  ;; The pattern PATTERN, renamed with the pairs RENAMES.
  (define (rename-pattern pattern renames)
    (let* ([prefix? (pattern-prefix? pattern)]
           [base (if prefix? (pattern-base pattern) pattern)])
      (let loop ([renames renames])
        (cond
          [(null? renames) pattern]
          [(list-tail-after (car (car renames)) base)
            => (lambda (rest)
                (let ([renamed (append (cdr (car renames)) rest)])
                  (if prefix? (append renamed '(*)) renamed)))]
          [else (loop (cdr renames))]))))

  (define (name->string name)
    (call-with-string-output-port (lambda (port) (write name port))))

  ;; The libraries the code of a package generates while it is compiled (`c-include`) are libraries of
  ;; the package, named `(capy c gen ...)`, and its view sees them as its own. They are in the
  ;; package's own entries only: a package that depends on it does not see them.
  (define generated-libraries-pattern '((capy c gen *)))

  (define (make-package-view id provides deps no-std? . maybe-key-id)
    (let ([table (make-hashtable equal-hash equal?)]
          [key-id (if (pair? maybe-key-id) (car maybe-key-id) id)])
      ;; The visible pattern -> the dependency that provides it.
      (for-each
        (lambda (dep)
          (let ([dep-id (car dep)] [renames (if (pair? (cddr dep)) (caddr dep) '())])
            (for-each
              (lambda (real)
                (let* ([visible (rename-pattern real renames)]
                       [other (hashtable-ref table visible #f)])
                  (if (and other (not (eq? (car other) dep-id)))
                    (error 'make-package-view
                      (string-append "the dependencies " (symbol->string (car other)) " and "
                        (symbol->string dep-id) " of " (symbol->string id) " both provide "
                        (name->string visible) "; rename one of them, for example "
                        "(rename (" (name->string (pattern-base-or-name real)) " -> "
                        (name->string (cons 'z (pattern-base-or-name real))) "))"))
                    (hashtable-set! table visible (cons dep-id real)))))
              (cadr dep))))
        deps)
      (for-each
        (lambda (own) (hashtable-set! table own (cons key-id own)))
        (if (eq? key-id 'core) provides (append provides generated-libraries-pattern)))
      (let-values ([(keys values) (hashtable-entries table)])
        (linker-make-view id
          (append
            (map (lambda (visible entry) (cons visible entry))
              (vector->list keys) (vector->list values))
            (if no-std?
              '()
              (list (cons '(*) (cons 'std '(*))))))))))

  (define (pattern-base-or-name pattern)
    (if (pattern-prefix? pattern) (pattern-base pattern) pattern))

  ;;; The libmap

;; The libmap of the packages as a datum. FASL-DIRS are the directories with the compiled files
  ;; and SOURCE-ROOTS `(dir . key-id)` or `(dir key-id edition)` lists for the source directory
  ;; of each package (KEY-ID is the package id its libraries have in their keys, `core` and `std`
  ;; for the toolchain's; EDITION picks between the `.sld` and `.sls` of one library).
  ;; VIEWS is a list of views made by `make-package-view`. DIRECTORY is the directory the libmap
  ;; file is in, the paths in it are relative to that.
  ;;
  ;; NATIVE-DIRS are the directories with the native extensions built for the packages
  ;; (`<package>/native`), see `native-entries`.
  (define (build-libmap fasl-dirs source-roots views directory . maybe-native-dirs)
    (vector '|#%libmap| 2 (%compiler-id)
      (library-entries fasl-dirs source-roots directory)
      (map (lambda (view) (cons (vector-ref view 1) view)) views)
      (native-entries (if (pair? maybe-native-dirs) (car maybe-native-dirs) '()) directory)))

  ;; The native extensions in the directories DIRS as `(key path identity)` entries, the paths
  ;; relative to DIRECTORY. An extension is a shared library with a descriptor beside it,
  ;; `NAME.native`, which holds `(native KEY IDENTITY FILE ...)`: FILE is the name of the shared
  ;; library in the directory. The directory of a package that has no extensions need not exist.
  (define (native-entries dirs directory)
    (apply append
      (map
        (lambda (dir)
          (if (and (file-exists? dir) (file-directory? dir))
            (filter values
              (map
                (lambda (name)
                  (and (string-suffix? name ".native")
                    (let ([d (call-with-input-file (string-append dir "/" name) read)])
                      (and (pair? d) (eq? (car d) 'native)
                        (list (cadr d)
                          (relative-path (string-append dir "/" (cadddr d)) directory)
                          (caddr d))))))
                (list-sort string<? (directory-list dir))))
            '()))
        dirs)))

;; The libmap of a package manager: ENTRIES are `(key source fasl index interface-hash wpo)`
  ;; lists (the paths are relative to the directory of the file, or absolute) and VIEWS the views
  ;; of the packages.
  (define (make-libmap entries views . maybe-native)
    (vector '|#%libmap| 2 (%compiler-id) entries
      (map (lambda (view) (cons (vector-ref view 1) view)) views)
      (if (pair? maybe-native) (car maybe-native) '())))

  ;; The libmap in the file PATH, which has to be made by this compiler.
  (define (read-libmap path)
    (let ([data (fasl-read path)])
      (cond
        [(not (and (vector? data) (= (vector-length data) 6) (eq? (vector-ref data 0) '|#%libmap|)
                (eqv? (vector-ref data 1) 2)))
          (error 'read-libmap "not a libmap" path)]
        [(not (equal? (vector-ref data 2) (%compiler-id)))
          (error 'read-libmap "the libmap was made by another build of the compiler" path)]
        [else data])))

  (define (libmap-compiler-id libmap) (vector-ref libmap 2))
  (define (libmap-entries libmap) (vector-ref libmap 3))
  ;; ((package-id . view) ...)
  (define (libmap-views libmap) (vector-ref libmap 4))
  ;; ((key path identity) ...): the native extensions, see `native-entries`. The paths are relative
  ;; to the directory of the libmap file, or absolute.
  (define (libmap-native libmap) (vector-ref libmap 5))
  (define (libmap-view libmap package)
    (let ([found (assq package (libmap-views libmap))])
      (and found (cdr found))))

  (define (view-library-key view name) (linker-view-key view name))
  (define (view-package view) (linker-view-package view))
  (define (view-entries view) (linker-view-entries view))

  ;; The compiled libraries in the file FASL as `(key index decl wpo)`, see `file-libraries`.
  (define (fasl-libraries fasl) (file-libraries fasl))

  (define (decl-key decl) (library-decl-key decl))
  ;; ((external internal kind origin) ...): `kind` is `macro` or `variable`; `origin` is the
  ;; `(key . name)` a re-export resolves to, else #f.
  (define (decl-exports decl) (library-decl-exports decl))
  ;; ((name procedure arity-mask) | (name constant datum) ...): what an importer may rely on.
  (define (decl-knowns decl) (library-decl-knowns decl))
  (define (decl-source-hash decl) (library-decl-source-hash decl))
  (define (decl-interface-hash decl) (library-decl-interface-hash decl))
  (define (decl-deps decl) (library-decl-deps decl))
  (define (decl-compiler-id decl) (library-decl-compiler-id decl))
  ;; ((path . hash) ...): the files the expansion of the library read, see `note-compile-dependency!`
  (define (decl-compile-deps decl) (library-decl-compile-deps decl))
  ;; Whether the compile-time dependencies DEPS of the library whose source is SOURCE are as they
  ;; were: the files have the hashes they had, and the native extensions the identities they had.
  ;; Relative files are found from SOURCE. NATIVE-IDENTITY, if it is given, gives the identity an
  ;; extension has now (a procedure of its key); otherwise the extensions of the libmap that is
  ;; installed are the ones looked at.
  (define decl-compile-deps-current?
    (case-lambda
      [(deps source) (compile-dependencies-current? deps source)]
      [(deps source native-identity) (compile-dependencies-current?/native deps native-identity source)]))
  (define (file-hash path) (%file-hash path))

  ;; Writes the libmap LIBMAP to the file PATH: to another file first, so that a process that
  ;; reads the libmap meanwhile sees the old one or the new one.
  (define (write-libmap-file! path libmap)
    (let ([tmp (string-append path ".tmp")])
      (fasl-write libmap tmp)
      (rename-file tmp path)
      path))

  ;; Writes the libmap to the file PATH.
  (define (write-libmap! path fasl-dirs source-roots views . maybe-native-dirs)
    (let ([directory (let loop ([i (- (string-length path) 1)])
                       (cond
                         [(< i 0) "."]
                         [(char=? (string-ref path i) #\/) (if (= i 0) "/" (substring path 0 i))]
                         [else (loop (- i 1))]))])
      (fasl-write
        (apply build-libmap fasl-dirs source-roots views directory maybe-native-dirs)
        path)
      path)))
