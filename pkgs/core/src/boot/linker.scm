;;; -*- mode: scheme; coding: utf-8; -*-
;;; The linker
;;;
;;; A compilation unit is a thunk returning a linklet, a namespace linklet, a linklet
;;; directory of several of them, or any other values. Running a unit instantiates its
;;; linklets in the current library registry: the imports of a linklet are required first (a
;;; library that is not instantiated yet is loaded), then the instance of the linklet is made,
;;; its body runs, and the instance is registered under the name of the linklet.
;;;
;;; The linker runs very early. It is the first boot file, it needs nothing but the native
;;; primitives (so its header is empty), and it loads every other boot file, from FASL or from
;;; source, through `linker-require!`. Two things it cannot have yet are installed later:
;;;
;;; - The environments. Until `boot/environments.scm` is instantiated, linklets are
;;;   instantiated without environments and their exports are kept in `linker-early-exports`;
;;;   `linker-install-environments!` (called by the environments) puts them into the core
;;;   environment, and from then on the exports of every linklet are mirrored into the
;;;   environments, see `linker-instantiate!`.
;;; - The expander. Until `linker-use-psyntax!`, the source of a boot file is expanded by the
;;;   Rust boot expander.
;;;
;;; `boot/main.scm` instantiates this file by hand, since there is no linker to do it, and then
;;; requires `(boot init)`.
;;;
;;; The libraries being required are tracked in a continuation mark, so they are forgotten when
;;; a load fails by an exception or an escape, and loading the library again later works.

(|#%require|)
(|#%provide| all-defined)

;;; Library keys

;; Whether KEY names a boot library, `(boot ...)`.
(define (linker-boot-key? key)
  (and (pair? key) (eq? (car key) 'boot)))

;; Whether META is the meta of a boot linklet: `(#%boot key)`, KEY being #f when the file is
;; expanded in the core environment.
(define (linker-boot-meta? meta)
  (and (pair? meta) (eq? (car meta) '|#%boot|)))

;; The knowns of a boot linklet, the third element of its `(#%boot key knowns)` meta, or '().
(define (linker-boot-knowns meta)
  (if (and (linker-boot-meta? meta) (pair? (cdr meta)) (pair? (cddr meta)))
    (car (cddr meta))
    '()))

;; The boot key a `|#%require|` spec of a boot file names, or #f.
(define (linker-spec-boot-key spec)
  (let ([key (if (and (pair? spec) (eq? (car spec) 'only)) (car (cdr spec)) spec)])
    (if (linker-boot-key? key)
      key
      #f)))

(define (linker-string-suffix? s suffix)
  (let ([n (string-length s)] [m (string-length suffix)])
    (and (>= n m) (string=? (substring s (- n m) n) suffix))))

;;; Names and files
;;;
;;; The one policy that turns a library name into a file. A library name is a list of symbols
;;; and non-negative integers, and its file is searched in the load path under the name
;;; `a/b/c`, with each of the load extensions.
;;;
;;; - A boot library `(boot x)` is the file `boot/x.scm`.
;;; - A few libraries are defined by the boot file of another name (`linker-virtual-keys`).
;;; - A SRFI has several spellings: `(srfi 1)`, `(srfi :1)` (the R6RS spelling, SRFI 97) and
;;;   `(srfi :1 lists)` (with a descriptive name). All of them name one library, found by
;;;   `linker-canonical-key`: the key is normalized to the candidates
;;;
;;;     (srfi :N rest ...)  (srfi N rest ...)  and, when REST is only a descriptive name,
;;;     (srfi :N)  (srfi N)
;;;
;;;   and the canonical key is the first candidate that has a file. Every spelling therefore
;;;   resolves to the same key, the same file and the same instance. The key as written stands
;;;   when no candidate has a file, or when the registry has an instance of it already.

;; The boot libraries that are defined by the file of another name.
(define linker-virtual-keys
  '(((core intrinsics) . "boot/libraries.scm")
    ((core primitives) . "boot/libraries.scm")
    ((core syntax-case) . "boot/libraries.scm")
    ((core match) . "boot/match.scm")))

(define (linker-virtual-key-file key)
  (let loop ([entries linker-virtual-keys])
    (cond
      [(null? entries) #f]
      [(equal? (car (car entries)) key) (cdr (car entries))]
      [else (loop (cdr entries))])))

;; Whether the file of KEY is a boot file: the library is a boot library or one a boot file
;; defines.
(define (linker-boot-file-key? key)
  (or (linker-boot-key? key) (and (linker-virtual-key-file key) #t)))

(define (linker-name-part->string part)
  (cond
    [(symbol? part) (symbol->string part)]
    [(and (exact-integer? part) (not (negative? part))) (number->string part)]
    [else (linker-error 'library "invalid library name component" part)]))

;; The (directory . file) parts of the name KEY: `((rnrs . base))` is `("rnrs/" . "base")`.
(define (linker-name->path-parts key)
  (let* ([reverse-name (reverse key)]
         [file (linker-name-part->string (car reverse-name))]
         [dir (let loop ([parts (reverse (cdr reverse-name))] [out ""])
                (if (null? parts)
                  out
                  (loop (cdr parts)
                    (string-append out (linker-name-part->string (car parts)) "/"))))])
    (cons dir file)))

;; The name KEY as a path, without an extension: `(rnrs base)` is "rnrs/base".
(define (linker-name->path key)
  (let ([parts (linker-name->path-parts key)])
    (string-append (car parts) (cdr parts))))

;; The file of the library KEY, relative to the load path: with its extension for a boot
;; file, without one for any other library (the load extensions are tried).
(define (linker-library-file key)
  (cond
    [(linker-virtual-key-file key)]
    [(linker-boot-key? key)
      (string-append "boot/" (symbol->string (car (cdr key))) ".scm")]
    [else (linker-name->path key)]))

;;; Libraries defined in more than one language
;;
;; Capy reads R6RS and R7RS, so one directory can hold both `foo.sls` and `foo.sld` for the
;; library (foo). `%load-extensions` puts the extensions of the current mode first, so the
;; mode picks the file. A file that loses only to a host or `capy` variant of its own
;; family (`foo.macos.sld` over `foo.sld`) is a deliberate override. A file of the other
;; family is not, so the linker reports it and names the file it chose.

;; The family of a load extension: r7rs, r6rs, or common (`scm` and the like, written in the
;; mode's language).
(define (linker-extension-family ext)
  (cond
    [(linker-string-suffix? ext "sld") 'r7rs]
    [(or (linker-string-suffix? ext "sls") (linker-string-suffix? ext "sps")) 'r6rs]
    [else 'common]))

;; The load extension of PATH: the longest entry of `%load-extensions` it ends with, or #f.
(define (linker-path-extension path)
  (let loop ([exts %load-extensions] [best #f])
    (cond
      [(null? exts) best]
      [(and (linker-string-suffix? path (string-append "." (car exts)))
            (or (not best) (> (string-length (car exts)) (string-length best))))
        (loop (cdr exts) (car exts))]
      [else (loop (cdr exts) best)])))

;; Is EXT the extension of a host or implementation variant (`macos.sld`, `capy.sls`)? A
;; variant is an explicit override, so it is never reported, as the chosen file or as the
;; shadowed one.
(define (linker-variant-extension? ext)
  (let loop ([i 0])
    (and (< i (string-length ext))
      (or (char=? (string-ref ext i) #\.) (loop (+ i 1))))))

;; The files next to CHOSEN that define the same library in another family.
(define (linker-shadowed-sources chosen)
  (let ([ext (linker-path-extension chosen)])
    (if (or (not ext) (linker-variant-extension? ext))
      '()
      (let ([stem (substring chosen 0 (- (string-length chosen) (string-length ext) 1))]
            [family (linker-extension-family ext)])
        (let loop ([exts %load-extensions] [out '()])
          (if (null? exts)
            (reverse out)
            (let ([path (string-append stem "." (car exts))])
              (loop (cdr exts)
                (if (and (not (string=? path chosen))
                         (not (linker-variant-extension? (car exts)))
                         (not (eq? (linker-extension-family (car exts)) family))
                         (file-exists? path))
                  (cons path out)
                  out)))))))))

;; Called with the key, the chosen file and the shadowed files when a library is found in two
;; families. It is a hook because the linker runs before the ports: init.scm installs it.
(define linker-ambiguous-source-hook (lambda (key chosen shadowed) #f))
(define (linker-set-ambiguous-source-hook! hook) (set! linker-ambiguous-source-hook hook))

;; The keys already reported, so a library is reported once.
(define linker-reported-ambiguities (make-core-hash-equal))

;; The full path of the source file of the library KEY in the load path, or #f. Reports the
;; files of the other family it does not take.
(define (linker-search-library-file key)
  (let ([found (%search-load-path (linker-library-file key))])
    (and found
      (let* ([chosen (car (cdr found))]
             [shadowed (linker-shadowed-sources chosen)])
        (if (and (pair? shadowed)
                 (not (core-hash-ref linker-reported-ambiguities key #f)))
          (begin
            (core-hash-put! linker-reported-ambiguities key #t)
            (linker-ambiguous-source-hook key chosen shadowed)))
        chosen))))

;; The path of the source file of the library KEY: the one the libmap lists, else the one found
;; in the load path, or #f.
(define (linker-find-library-file key)
  (or (linker-libmap-source key)
    (linker-search-library-file key)
    (linker-output-file key)))

;; The directory libraries that code generated while a library was compiled (`c-include`) are in,
;; or #f: set by `(boot environments)` to `(compile-output-directory)`. It is a source root of
;; the load, like the load path: a program or library compiled earlier that imports a generated
;; library finds it there when it is loaded again without being expanded, so nothing has to
;; register the library again.
(define linker-output-directory-hook (lambda () #f))
(define (linker-set-output-directory-hook! hook) (set! linker-output-directory-hook hook))

;; The file of the library KEY below the output directory, or #f.
(define (linker-output-file key)
  (let ([dir (linker-output-directory-hook)])
    (and (string? dir)
      (let ([base (string-append dir "/" (linker-library-file key))])
        (let loop ([extensions %load-extensions])
          (and (pair? extensions)
            (let ([path (string-append base "." (car extensions))])
              (if (file-exists? path) path (loop (cdr extensions))))))))))

;; What to hand the loader to find the source of the library KEY: the file the libmap lists,
;; else the name of the file, which the loader searches the load path for, else the file of the
;; output directory.
(define (linker-library-source key)
  (or (linker-libmap-source key)
    (if (linker-search-library-file key)
      (linker-library-file key)
      (or (linker-output-file key) (linker-library-file key)))))

;; The number N of a SRFI number component, `N` or `:N`, or #f.
(define (linker-srfi-number part)
  (cond
    [(and (exact-integer? part) (not (negative? part))) part]
    [(symbol? part)
      (let* ([s (symbol->string part)] [n (string-length s)])
        (and (> n 1)
          (char=? (string-ref s 0) #\:)
          (let loop ([i 1] [value 0])
            (if (= i n)
              value
              (let ([d (- (char->integer (string-ref s i)) 48)])
                (and (>= d 0) (<= d 9) (loop (+ i 1) (+ (* value 10) d))))))))]
    [else #f]))

(define (linker-srfi-key? key)
  (and (pair? key) (eq? (car key) 'srfi) (pair? (cdr key)) (linker-srfi-number (car (cdr key)))
    #t))

;; The candidate keys of the SRFI key KEY, see above.
(define (linker-srfi-candidates key)
  (let* ([n (linker-srfi-number (car (cdr key)))]
         [colon (string->symbol (string-append ":" (number->string n)))]
         [rest (cdr (cdr key))])
    (append
      (list (cons 'srfi (cons colon rest)) (cons 'srfi (cons n rest)))
      (if (and (pair? rest) (null? (cdr rest)) (symbol? (car rest)))
        (list (list 'srfi colon) (list 'srfi n))
        '()))))

(define linker-canonical-keys (make-core-hash-equal))

;; The canonical key of KEY: the key every spelling of the library is resolved to. Keys that
;; are not SRFIs are their own canonical key.
(define (linker-canonical-key key)
  (if (not (linker-srfi-key? key))
    key
    (or (core-hash-ref linker-canonical-keys key #f)
      (let ([reg (linker-registry)])
        (if (library-instance reg key)
          key
          (let loop ([candidates (linker-srfi-candidates key)])
            (cond
              [(null? candidates) key]
              [(linker-library-known? (car candidates))
                (core-hash-put! linker-canonical-keys key (car candidates))
                (car candidates)]
              [else (loop (cdr candidates))])))))))

;;; Library keys of packages, and views
;;;
;;; A library of the packages `core` and `std` (everything in `lib/`) has a plain key, its
;;; name: `(rnrs base)`. A library of any other package has the key
;;;
;;;   (#%pkg <package-id> . <name>)       e.g. (#%pkg |github.com/x/json@1| json)
;;;
;;; where the package id names a package and one major version of it. Decls, linklet
;;; imports, the registry, interface hashes and `.wpo` files only ever hold such keys. The
;;; names a package writes in its sources are not keys: they are mapped to keys by the view of
;;; the package, `linker-view-key`, when the expander resolves an import (see `current-library-view`
;;; in environments.scm).
;;;
;;; A view is `#(#%view <package-id> <entries>)`, and an entry is `(<visible> <package-id> .
;;; <real>)`: a library name (a list of symbols and integers) the package can write, the package
;;; that provides the library and the name the library has there. The package of a view
;;; always appears in its own entries. A name that ends in `*` stands for every longer name
;;; with the same beginning (and not for the beginning itself): the entry
;;; `((z json *) |github.com/y/json@2| json *)` maps `(z json parser)` to `(json parser)`
;;; in that package, and the entry `((capy) core capy)` only `(capy)`. When several entries
;;; match a name an exact one wins, then the longest beginning. A name no entry matches is not
;;; visible, and a name that already is a key stands for itself.

(define (linker-package-key package name)
  (if (or (eq? package 'core) (eq? package 'std))
    name
    (cons '|#%pkg| (cons package name))))

;; Whether KEY is the key of a library of a package other than `core` and `std`.
(define (linker-package-key? key)
  (and (pair? key) (eq? (car key) '|#%pkg|) (pair? (cdr key)) (symbol? (car (cdr key)))))

;; The package id of the package key KEY and the name of the library in that package.
(define (linker-key-package key) (car (cdr key)))
(define (linker-key-name key) (cdr (cdr key)))

(define (linker-make-view package entries) (vector '|#%view| package entries))

(define (linker-view? x)
  (and (vector? x) (= (vector-length x) 3) (eq? (vector-ref x 0) '|#%view|)))

(define (linker-view-package view) (vector-ref view 1))
(define (linker-view-entries view) (vector-ref view 2))

;; Whether a name pattern is a prefix pattern: it ends in `*`.
(define (linker-pattern-prefix? pattern)
  (and (pair? pattern)
    (if (null? (cdr pattern)) (eq? (car pattern) '*) (linker-pattern-prefix? (cdr pattern)))))

;; The pattern PATTERN without its `*`.
(define (linker-pattern-base pattern)
  (if (null? (cdr pattern)) '() (cons (car pattern) (linker-pattern-base (cdr pattern)))))

;; The rest of NAME after the list BASE, or #f when BASE is not the beginning of NAME.
(define (linker-name-tail base name)
  (cond
    [(null? base) name]
    [(and (pair? name) (equal? (car base) (car name)))
      (linker-name-tail (cdr base) (cdr name))]
    [else #f]))

;; The match of the entry ENTRY with NAME as (score . name-in-package), or #f. An exact match has
;; the highest score, then the longest beginning.
(define (linker-entry-match entry name)
  (let ([visible (car entry)] [real (cdr (cdr entry))])
    (if (linker-pattern-prefix? visible)
      (let ([tail (linker-name-tail (linker-pattern-base visible) name)])
        (and (pair? tail)
          (cons (length visible)
            (append (linker-pattern-base real) tail))))
      (and (equal? visible name) (cons 1000000 real)))))

;; The key of the library the view VIEW sees under the name NAME, or #f when the view has no
;; such name. No view (#f) is the identity.
(define (linker-view-key view name)
  (cond
    [(not view) name]
    [(or (linker-package-key? name) (and (pair? name) (eq? (car name) '%program))) name]
    [else
      (let loop ([entries (linker-view-entries view)] [best #f] [best-score -1])
        (if (null? entries)
          (and best (linker-package-key (car (cdr best)) (cdr (linker-entry-match best name))))
          (let ([m (linker-entry-match (car entries) name)])
            (if (and m (> (car m) best-score))
              (loop (cdr entries) (car entries) (car m))
              (loop (cdr entries) best best-score)))))]))

;; The names the view VIEW sees the library KEY under, as a list.
(define (linker-view-names view key)
  (let ([package (if (linker-package-key? key) (linker-key-package key) #f)]
        [name (if (linker-package-key? key) (linker-key-name key) key)])
    (let loop ([entries (linker-view-entries view)] [out '()])
      (if (null? entries)
        (reverse out)
        (let* ([entry (car entries)]
               [real (cdr (cdr entry))]
               [entry-package (car (cdr entry))]
               [visible (car entry)]
               [seen
                 (and (if package
                        (eq? entry-package package)
                        (or (eq? entry-package 'core) (eq? entry-package 'std)))
                   (if (linker-pattern-prefix? real)
                     (let ([tail (linker-name-tail (linker-pattern-base real) name)])
                       (and (pair? tail)
                         (if (linker-pattern-prefix? visible)
                           (append (linker-pattern-base visible) tail)
                           #f)))
                     (and (equal? real name)
                       (if (linker-pattern-prefix? visible) #f visible))))])
          (loop (cdr entries) (if (and seen (equal? (linker-view-key view seen) key)) (cons seen out) out)))))))

;;; The libmap
;;;
;;; The libmap is the one policy that says where the compiled form of a library is, besides the
;;; load path (which is for libraries nobody mapped: the user's own, found by `-L`). It is a
;;; FASL data file, so that the linker can read it with a native primitive before any other
;;; boot file is loaded (it names the boot files too), written by the tool that compiled the
;;; libraries it lists (`(capy libmap)`):
;;;
;;;   #(#%libmap 2 compiler-id libraries views native)
;;;
;;;   libraries   ((key source fasl index interface-hash wpo) ...) for every library of every
;;;               package, compiled or not. SOURCE is the source file that defines the library.
;;;               FASL is the compiled file, INDEX the place of the library in the directory of
;;;               the file, INTERFACE-HASH the interface hash of its decl and WPO the `.wpo`
;;;               file of the compiled file; all four are #f for a library that was not
;;;               compiled (and the hash for a boot file). The paths are relative to the
;;;               directory of the libmap file, or absolute.
;;;   views       ((package-id . view) ...) the view of each package, see above
;;;   native      ((key path identity) ...) the native extensions of the packages: KEY is
;;;               `(#%native <package-id> <name>)`, PATH the shared library (relative to the
;;;               directory of the libmap file, or absolute) and IDENTITY the hash of what it
;;;               was built from (its sources, its Cargo.lock and its flags). See "Native
;;;               extensions" below.
;;;
;;; A libmap is trusted: the tool that wrote it checked the files it lists, so a library it
;;; lists with a compiled file is loaded from that file without searching the load path, hashing
;;; the source or looking at the libraries it imports. The compiler id of the libmap is checked,
;;; so one of another build of the compiler is not installed. A library it lists without a
;;; compiled file is "this key is that source file": it is loaded from the source the way a
;;; file of the load path is, through the compile cache and the checks of
;;; `fasl-needs-recompile?`, but without searching anything. The same source file is what
;;; the static linker expands.

(define linker-libmap #f)
(define linker-libmap-table (make-core-hash-equal))
(define linker-libmap-directory "")
;; Why the libmap could not be installed, or #f.
(define linker-libmap-failure #f)
(define (linker-libmap-problem) linker-libmap-failure)

;; The directory part of PATH, with its trailing slash.
(define (linker-path-directory path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) ""]
      [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
      [else (loop (- i 1))])))

;; Installs the libmap file PATH, the way libraries are found from now on. Returns whether
;; it is installed: it is not when it was made by another compiler, see
;; `linker-libmap-failure`.
(define (linker-install-libmap! path)
  (let ([data (fasl-read path)])
    (cond
      [(not (and (vector? data) (= (vector-length data) 6) (eq? (vector-ref data 0) '|#%libmap|)
              (eqv? (vector-ref data 1) 2)))
        (set! linker-libmap-failure "not a libmap")
        #f]
      [(not (equal? (vector-ref data 2) (%compiler-id)))
        (set! linker-libmap-failure "made by another compiler build")
        #f]
      [else
        (core-hash-clear! linker-libmap-table)
        (let loop ([entries (vector-ref data 3)])
          (if (pair? entries)
            (begin
              (core-hash-put! linker-libmap-table (car (car entries)) (cdr (car entries)))
              (loop (cdr entries)))))
        (set! linker-libmap data)
        (set! linker-libmap-directory (linker-path-directory path))
        (set! linker-libmap-failure #f)
        #t])))

;; The entry `(source fasl index interface-hash wpo)` of the library KEY, or #f.
(define (linker-libmap-entry key)
  (core-hash-ref linker-libmap-table key #f))

;; Lists the library KEY with the source file PATH (an absolute path), the way the libmap lists a
;; library it has no compiled file of, for the libraries that are made while a library is
;; compiled (see `register-generated-library!`). A library that is listed already keeps its entry.
(define (linker-libmap-register-source! key path)
  (if (not (core-hash-ref linker-libmap-table key #f))
    (core-hash-put! linker-libmap-table key (list path #f #f #f #f))))

;; PATH with its `x/..` segments removed, so that a file of the libmap is the same string whatever
;; directory the libmap is in: `stage-1/../pkgs/core/src/x.scm` is `pkgs/core/src/x.scm`. The
;; compiled files record the source paths the libmap gives them, and these have to be the same
;; for a toolchain built in `stage-1` and one built in `stage-2`. Leading `..` segments stay.
(define (linker-normalize-path path)
  (let ([n (string-length path)])
    ;; segments, last first, of the path
    (define (split)
      (let loop ([i 0] [start 0] [out '()])
        (cond
          [(= i n) (cons (substring path start n) out)]
          [(char=? (string-ref path i) #\/) (loop (+ i 1) (+ i 1) (cons (substring path start i) out))]
          [else (loop (+ i 1) start out)])))
    (define (join segments)
      (let loop ([s (cdr segments)] [out (car segments)])
        (if (null? s) out (loop (cdr s) (string-append (car s) "/" out)))))
    (let loop ([segments (reverse (split))] [out '()])
      (cond
        [(null? segments) (if (null? out) "" (join out))]
        [(and (string=? (car segments) "..") (pair? out)
              (not (string=? (car out) ".."))
              (not (string=? (car out) "")))
          (loop (cdr segments) (cdr out))]
        [else (loop (cdr segments) (cons (car segments) out))]))))

(define (linker-libmap-file path)
  (if (and (> (string-length path) 0) (char=? (string-ref path 0) #\/))
    path
    (linker-normalize-path (string-append linker-libmap-directory path))))

;; The source file of the library KEY the libmap lists, or #f.
(define (linker-libmap-source key)
  (let ([entry (linker-libmap-entry key)])
    (and entry (car entry) (linker-libmap-file (car entry)))))

;; The compiled file of the library KEY the libmap lists, or #f when it lists none.
(define (linker-libmap-fasl key)
  (let ([entry (linker-libmap-entry key)])
    (and entry (car (cdr entry)) (linker-libmap-file (car (cdr entry))))))

;; The `.wpo` file of the library KEY the libmap lists, or #f.
(define (linker-libmap-wpo key)
  (let ([entry (linker-libmap-entry key)])
    (and entry (car (cdr (cdr (cdr (cdr entry)))))
      (linker-libmap-file (car (cdr (cdr (cdr (cdr entry)))))))))

;; The keys of the libraries of the libmap.
(define (linker-libmap-keys)
  (if linker-libmap
    (let loop ([entries (vector-ref linker-libmap 3)])
      (if (null? entries) '() (cons (car (car entries)) (loop (cdr entries)))))
    '()))

;; The view of the package PACKAGE in the libmap, or #f.
(define (linker-libmap-view package)
  (and linker-libmap
    (let loop ([views (vector-ref linker-libmap 4)])
      (cond
        [(null? views) #f]
        [(eq? (car (car views)) package) (cdr (car views))]
        [else (loop (cdr views))]))))

;;; Native extensions
;;;
;;; A package can have native extensions (`native` in its manifest): shared libraries built from
;;; Rust (`capy-sni`) or C sources that Scheme code loads with `(load-native-extension SPEC)`.
;;; SPEC is
;;;
;;;   "path/to/lib.dylib"         a file, as it is
;;;   (#%native <package-id> <name>)   the key of an extension of the libmap
;;;   <name>                      a symbol: the one extension of that name, whatever its package
;;;
;;; A key is looked up in the native section of the libmap. A program that was linked
;;; with `capyc --static --exe` has no libmap: its extensions are in `lib/<package-id>/<name>.<dll>`
;;; beside the executable, where the static linker put them. One that was linked with
;;; `--static-native` has them in the executable: the key is looked up in the table of the
;;; extensions linked into it, under the text <package-id>/<name>, before any file is opened.
;;;
;;; While a library is being compiled, the extensions it loads by key are compile-time
;;; dependencies of it: the hook `linker-native-hook`, set by the environments, notes the key and
;;; the identity of the extension, so that the library is compiled again when the extension changes.

(define linker-native-hook #f)
(define (linker-set-native-hook! hook) (set! linker-native-hook hook))

;; The native entries of the libmap: ((key path identity) ...).
(define (linker-native-entries)
  (if linker-libmap (vector-ref linker-libmap 5) '()))

(define (linker-native-key? x)
  (and (pair? x) (eq? (car x) '|#%native|) (pair? (cdr x)) (symbol? (car (cdr x)))
    (pair? (cdr (cdr x))) (symbol? (car (cdr (cdr x)))) (null? (cdr (cdr (cdr x))))))

;; The keys of the entries ENTRIES.
(define (linker-native-keys entries)
  (if (null? entries) '() (cons (car (car entries)) (linker-native-keys (cdr entries)))))

;; The entry `(key path identity)` the libmap has for the key KEY, or #f.
(define (linker-native-entry key)
  (let loop ([entries (linker-native-entries)])
    (cond
      [(null? entries) #f]
      [(equal? (car (car entries)) key) (car entries)]
      [else (loop (cdr entries))])))

;; The absolute path of the shared library of the entry ENTRY.
(define (linker-native-file entry) (linker-libmap-file (car (cdr entry))))

;; The entry that SPEC, a key or a symbol, names; #f when there is none.
(define (linker-native-lookup spec)
  (cond
    [(linker-native-key? spec) (linker-native-entry spec)]
    [(symbol? spec)
      (let loop ([entries (linker-native-entries)] [found '()])
        (cond
          [(null? entries)
            (cond
              [(null? found) #f]
              [(null? (cdr found)) (car found)]
              [else (linker-error 'load-native-extension
                      "several packages have an extension of that name, use its key" spec
                      (linker-native-keys found))])]
          [(eq? (car (cdr (cdr (car (car entries))))) spec) (loop (cdr entries) (cons (car entries) found))]
          [else (loop (cdr entries) found)]))]
    [else #f]))

;; The file a static executable has the extension KEY in: lib/<package-id>/<name>.<dll> in the
;; directory of the executable. #f when the key is not one, or there is no such file.
(define (linker-native-beside-executable key)
  (and (linker-native-key? key)
    (let ([exe (executable-path)])
      (and exe
        (let ([file (string-append (linker-path-directory exe) "lib/"
                      (symbol->string (car (cdr key))) "/"
                      (symbol->string (car (cdr (cdr key)))) (dll-suffix))])
          (and (file-exists? file) file))))))

;; For the macro `require-native-extension`: checks that the libmap has the extension SPEC, notes it
;; as a compile-time dependency of the library being compiled, and returns its key.
(define (linker-require-native! spec)
  (let ([entry (linker-native-lookup spec)])
    (if (not entry)
      (linker-error 'require-native-extension "no such native extension" spec))
    (if linker-native-hook (linker-native-hook (car entry) (car (cdr (cdr entry)))))
    (car entry)))

;; The shared library of the extension SPEC (a key or a name) when there is one: the libmap lists it
;; or it is beside the executable, and the file exists. #f otherwise. It does not load anything.
(define (native-extension-file spec)
  (let ([entry (linker-native-lookup spec)])
    (if entry
      (let ([file (linker-native-file entry)]) (and (file-exists? file) file))
      (linker-native-beside-executable spec))))

;; The text a static executable lists the extension KEY under in its table: <package-id>/<name>.
(define (linker-native-key-text key)
  (string-append (symbol->string (car (cdr key))) "/" (symbol->string (car (cdr (cdr key))))))

;; The text of SPEC when it names an extension that was linked into the executable
;; (`capyc --static --static-native`), else #f.
(define (linker-static-native-text spec)
  (and (linker-native-key? spec)
    (let ([text (linker-native-key-text spec)])
      (and (%static-native-extension? text) text))))

;; Loads the native extension SPEC, see above.
(define (load-native-extension spec)
  (cond
    [(string? spec) (%load-native-extension spec)]
    [(linker-static-native-text spec) (%load-native-extension (linker-static-native-text spec))]
    [else
      (let ([entry (linker-native-lookup spec)])
        (cond
          [entry
            (if linker-native-hook (linker-native-hook (car entry) (car (cdr (cdr entry)))))
            (%load-native-extension (linker-native-file entry))]
          [else
            (let ([beside (linker-native-beside-executable spec)])
              (if beside
                (%load-native-extension beside)
                (linker-error 'load-native-extension "no such native extension" spec)))]))]))

;; Whether the library KEY has a file: the libmap lists it, or it is on the load path.
(define (linker-library-known? key)
  (or (and (linker-libmap-entry key) #t)
    (and (linker-find-library-file key) #t)))

;; Loads the library KEY from its compiled file in the libmap, when it lists one: the file is
;; run, which instantiates the libraries in it. Returns whether it did. A library the libmap
;; lists without a compiled file is loaded from its source, see `linker-libmap-source`.
(define (linker-libmap-load! key)
  (let ([fasl (linker-libmap-fasl key)])
    (and fasl
      (let ([thunk (load-thunk-in-vicinity fasl #t)])
        (receive vals (thunk)
          (linker-run-values vals #f))
        #t))))

;;; Errors

(define linker-error-procedure #f)

;; Raises an error. Before the environments install `error` there is nothing to build a
;; condition with, and the irritants are raised as they are.
(define (linker-error who message . irritants)
  (if linker-error-procedure
    (apply linker-error-procedure who message irritants)
    (%raise (cons who (cons message irritants)))))

;;; Environment services

;; Procedures of the environments, installed by `linker-install-environments!`. While they
;; are not installed (`linker-environments?` is #f) there is no environment: linklets are
;; instantiated in the root registry and their exports wait in `linker-early-exports`.
(define linker-environments? #f)
(define linker-current-registry #f)
(define linker-current-expansion-env #f)
(define linker-expansion-env #f)
(define linker-current-namespace #f)
(define linker-ensure-env #f)
(define linker-env-instance #f)
(define linker-env-library-instantiated! #f)
(define linker-env-mirror-exports! #f)
(define linker-env-add-pending-imports! #f)
(define linker-library-decl? #f)
(define linker-env-boot-knowns! #f)

;; The #(instance exports meta) of the linklets instantiated before the environments existed,
;; latest first.
(define linker-early-exports '())

;; Called by the environments once they can be used. CORE-ENV is the core environment: the
;; exports of the linklets instantiated so far are put into it.
(define (linker-install-environments! error-procedure
          current-registry current-expansion-env expansion-env current-namespace
          ensure-env env-instance env-library-instantiated! env-mirror-exports!
          env-add-pending-imports! library-decl? env-boot-knowns! core-env)
  (set! linker-error-procedure error-procedure)
  (set! linker-current-registry current-registry)
  (set! linker-current-expansion-env current-expansion-env)
  (set! linker-expansion-env expansion-env)
  (set! linker-current-namespace current-namespace)
  (set! linker-ensure-env ensure-env)
  (set! linker-env-instance env-instance)
  (set! linker-env-library-instantiated! env-library-instantiated!)
  (set! linker-env-mirror-exports! env-mirror-exports!)
  (set! linker-env-add-pending-imports! env-add-pending-imports!)
  (set! linker-library-decl? library-decl?)
  (set! linker-env-boot-knowns! env-boot-knowns!)
  (let loop ([entries (reverse linker-early-exports)])
    (if (pair? entries)
      (let ([inst (vector-ref (car entries) 0)])
        (env-mirror-exports! core-env inst (vector-ref (car entries) 1))
        (env-boot-knowns! (instance-name inst) (linker-boot-knowns (vector-ref (car entries) 2)))
        (loop (cdr entries)))))
  (set! linker-early-exports '())
  ;; main.scm instantiates the linker by hand, so its own exports are not among the early ones.
  (let ([self (library-instance (root-library-registry) '(boot linker))])
    (if self
      (env-mirror-exports! core-env self (instance-symbols self))))
  (set! linker-environments? #t))

;; The registry linklets are instantiated in: the one of the current namespace, and the root
;; registry until the environments (and so the namespaces) exist.
(define (linker-registry)
  (if linker-environments?
    (linker-current-registry)
    (root-library-registry)))

;;; Libraries being required

(define linker-requiring-key (list 'linker-requiring))

;; The keys of the libraries being required, the latest first.
(define (linker-requiring)
  (continuation-mark-set-first (current-continuation-marks) linker-requiring-key '()))

(define (linker-key-member? key keys)
  (and (pair? keys)
    (or (equal? (car keys) key) (linker-key-member? key (cdr keys)))))

;; Calls THUNK with KEY among the libraries being required. It is an error when KEY already is
;; one, except as the latest: loading the file of a library instantiates its linklet, which
;; is the same requirement.
(define (linker-entering key thunk)
  (let ([keys (linker-requiring)])
    (cond
      [(and (pair? keys) (equal? (car keys) key)) (thunk)]
      [(linker-key-member? key keys)
        (linker-error 'linker-require! "cyclic library dependency" key (reverse keys))]
      [else
        (with-continuation-mark linker-requiring-key (cons key keys)
          (thunk))])))

;;; Requiring libraries

;; The procedure that runs the source of a boot file, or #f for the Rust boot expander (see
;; `linker-use-psyntax!`).
(define linker-source-loader #f)

;; Switches the source of boot files to LOADER, a procedure of a file name that expands it
;; with the expander of the library system and runs it with `linker-run-thunk`.
(define (linker-use-psyntax! loader)
  (set! linker-source-loader loader))

(define (linker-load-source file)
  (if linker-source-loader
    (linker-source-loader file)
    (linker-run-thunk (load-thunk-in-vicinity file #t))))

;; Instantiates the boot library KEY: its FASL is run, which requires the imports of its
;; linklet, or else its source is, after the boot libraries its header requires.
(define (linker-require-boot! key)
  (or (linker-libmap-load! key)
    (linker-require-boot-file! key)))

(define (linker-require-boot-file! key)
  (let* ([file (linker-library-file key)]
         [path (or (linker-libmap-source key)
                 (let ([found (%search-load-path file)])
                   (and found (car (cdr found)))))])
    (if (not path)
      (linker-error 'linker-require! "no boot file for key" key file))
    (if (linker-string-suffix? path ".fasl")
      (let ([thunk (load-thunk-in-vicinity file #t)])
        (receive vals (thunk)
          (linker-run-values vals #f)))
      (begin
        (let loop ([specs (or (%boot-header-requires path) '())])
          (if (pair? specs)
            (let ([dep (linker-spec-boot-key (car specs))])
              (if dep (linker-require! dep))
              (loop (cdr specs)))))
        (linker-load-source path)))))

;; Loads any other library through the resolver of the registry REG.
(define (linker-require-resolved! reg key)
  (let ([resolver (registry-resolver reg)])
    (if resolver
      (resolver key)
      (linker-error 'linker-require! "library not available" key))))

;; Instantiates the library KEY unless it is, and returns its instance. KEY is canonicalized
;; first (see `linker-canonical-key`). A boot library is found by its name in the load path or
;; the libmap, any other with the resolver of the current registry. It is an error when KEY is
;; being required already (a cycle), when the file of the library does not register an instance
;; under the key that was required, and the libraries that were being required are forgotten
;; when the load fails.
(define (linker-require! requested)
  (let ([reg (linker-registry)]
        [key (linker-canonical-key requested)])
    (or (library-instance reg key)
      (begin
        (linker-traced key
          (lambda ()
            (linker-entering key
              (lambda ()
                (if (linker-boot-file-key? key)
                  (linker-require-boot! key)
                  (linker-require-resolved! reg key))))))
        (or (library-instance reg key)
          (linker-error 'linker-require! "the file of the library registered no instance of it"
            key (linker-library-file key)))))))

;;; Boot trace
;;;
;;; With `CAPY_BOOT_TRACE` set, the time every library takes to require is kept as
;;; `(key total-us own-us)`, newest first, and boot/init.scm prints where the time went at the
;;; end of the boot.

(define linker-trace?
  (let ([flag (getenv "CAPY_BOOT_TRACE")])
    (and flag (not (string=? flag "")) (not (string=? flag "0")))))
(define linker-trace-log '())
(define linker-trace-children 0)

;; Calls THUNK, and notes its time (microseconds, with and without the libraries it required)
;; as the time of the library KEY.
(define (linker-traced key thunk)
  (if (not linker-trace?)
    (thunk)
    (let ([start (microsecond)] [outer linker-trace-children])
      (set! linker-trace-children 0)
      (thunk)
      (let ([total (- (microsecond) start)])
        (set! linker-trace-log
          (cons (list key total (- total linker-trace-children)) linker-trace-log))
        (set! linker-trace-children (+ outer total))))))

(define (linker-trace-entries) linker-trace-log)

;;; Instantiating linklets

;; The import instances of a linklet in the registry REG, one per import group. The libraries
;; the linklet imports are required first.
(define (linker-import-instances reg imports)
  (if (null? imports)
    '()
    (let ([inst (linker-require! (car (car imports)))])
      (cons inst (linker-import-instances reg (cdr imports))))))

;; Runs the body of a linklet with the expansion environment of `meta` current, after the
;; exports are put into that environment.
(define (linker-run-body-in-env meta exports inst body slots)
  (linker-env-boot-knowns! (instance-name inst) (linker-boot-knowns meta))
  (let ([env (let ([key (if (linker-boot-meta? meta) (car (cdr meta)) meta)])
               (if (pair? key) (linker-ensure-env key) (linker-expansion-env)))]
        [saved-env (linker-current-expansion-env)])
    (linker-current-expansion-env env)
    (linker-env-mirror-exports! env inst exports)
    (apply body slots)
    (linker-current-expansion-env saved-env)))

;; Runs the body of a library linklet. The library gets no expansion environment here: when
;; psyntax built one while compiling the library in this process, it is given the instance
;; variables, and otherwise it is built from the meta when something expands against the
;; library.
(define (linker-run-library-body meta exports inst body slots)
  (let ([env (linker-env-library-instantiated! meta inst exports)])
    (if env
      (let ([saved-env (linker-current-expansion-env)])
        (linker-current-expansion-env env)
        (apply body slots)
        (linker-current-expansion-env saved-env))
      (apply body slots))))

;; Instantiates the linklet LK in the current registry. Its imports are required, then its
;; body runs, and the instance is registered under the name of the linklet: it is an error
;; when that name has an instance already.
(define (linker-instantiate! lk)
  (let* ([reg (linker-registry)]
         [name (linklet-name lk)]
         [imports (linklet-imports lk)]
         [exports (linklet-exports lk)]
         [body (linklet-body lk)]
         [meta (linklet-meta lk)])
    (linker-entering name
      (lambda ()
        (let* ([import-instances (linker-import-instances reg imports)]
               [inst (or (take-placeholder-instance! reg name) (make-linklet-instance name #f))]
               [slots (linklet-slots reg lk import-instances inst)])
          (if linker-environments?
            (if (linker-library-decl? meta)
              (linker-run-library-body meta exports inst body slots)
              (linker-run-body-in-env meta exports inst body slots))
            (begin
              (set! linker-early-exports (cons (vector inst exports meta) linker-early-exports))
              (apply body slots)))
          (register-library-instance! reg name inst))))))

;; The import instances of the groups of a namespace linklet: the namespace itself, then the
;; libraries.
(define (linker-namespace-import-instances reg imports ns)
  (let ([own (linker-env-instance ns)])
    (let loop ([syms (cdr (car imports))])
      (if (pair? syms)
        (begin
          (instance-ensure-variable! own (car syms))
          (loop (cdr syms)))))
    (cons own (linker-import-instances reg (cdr imports)))))

;; Runs a namespace linklet in the namespace NS.
(define (linker-run-namespace-linklet ns lk)
  (let* ([reg (linker-registry)]
         [imports (linklet-imports lk)]
         [meta (linklet-meta lk)]
         [body (linklet-body lk)]
         [insts (linker-namespace-import-instances reg imports ns)]
         [slots (linklet-slots reg lk insts (linker-env-instance ns))])
    ; The imports of the unit are applied to the namespace when it is first used to expand
    ; something (`eval`, the REPL), not now.
    (linker-env-add-pending-imports! ns (cdr (cdr meta)))
    (apply body slots)))

;;; Running units

;; Runs the linklet LK in the namespace NS (or #f for the current one).
(define (linker-run-linklet lk ns)
  (let ([meta (linklet-meta lk)])
    (if (and (pair? meta) (eq? (car meta) '|#%namespace|))
      (linker-run-namespace-linklet
        (or ns (linker-current-namespace) (linker-expansion-env)) lk)
      (linker-instantiate! lk))))

;; Runs the values VALS of a unit thunk in the namespace NS (or #f for the current one): a
;; linklet is instantiated, a linklet directory runs its linklets in order, and any other
;; values are returned.
(define (linker-run-values vals ns)
  (cond
    [(and (pair? vals) (null? (cdr vals)) (linklet? (car vals)))
      (linker-run-linklet (car vals) ns)]
    [(and (pair? vals) (null? (cdr vals)) (linklet-directory? (car vals)))
      (let loop ([lks (linklet-directory->list (car vals))] [result (if #f #f)])
        (if (null? lks)
          result
          (loop (cdr lks) (linker-run-linklet (car lks) ns))))]
    [else (apply values vals)]))

;; Runs UNIT in the namespace NS (or #f for the current one): a linklet, or a linklet directory,
;; or else the unit itself is returned.
(define (linker-run unit ns)
  (linker-run-values (list unit) ns))

;; Calls the unit thunk THUNK and runs the values it returns, in the namespace given as the
;; optional argument.
(define (linker-run-thunk thunk . maybe-ns)
  (receive vals (thunk)
    (linker-run-values vals (and (pair? maybe-ns) (car maybe-ns)))))
