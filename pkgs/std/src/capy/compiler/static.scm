;;; Whole-program static linker.
;;;
;;; `(static-link file out options)` compiles the program FILE together with every linklet it
;;; depends on, the boot linklets included, into ONE linklet written to OUT.
;;;
;;; How it works
;;; ------------
;;; 1. Collect. The program is expanded by psyntax to its linklet term, `(%program <file>)`.
;;;    Every other linklet it depends on is found through the linker: the keys of its import
;;;    groups, the libraries its meta imports and, for a boot file, the keys of its `|#%require|`
;;;    header. A depth-first walk gives the order the linklets run in. The linklets of a library
;;;    come from the `.wpo` file next to its compiled file when that is fresh (the tree-IL the
;;;    compiler kept, see `library-wpo-linklets` in boot/eval.scm), otherwise the source is
;;;    expanded again. `--static-report` says which one each came from.
;;;
;;; 2. Merge. Every linklet exports its definitions as slots. In the merged term those become
;;;    ordinary lexical variables of one `letrec*`:
;;;      - an import slot is replaced by the variable of the providing linklet, found in the
;;;        export table of the provider.
;;;      - a definition (`global-set` on an own export slot, once, at the top level of the
;;;        body) is a binding of the `letrec*`, and any other `global-set` of a variable is an
;;;        `lset` on a variable that is initialised to void.
;;;      - every other top-level form is an effect binding, in order.
;;;    So the existing `fix-letrec` sees calls between linklets as direct calls.
;;;
;;; 3. Shaking (capy compiler static shake). Every top-level binding and effect is classified
;;;    pure, impure or "attached" to the variables it changes (an effect lattice with
;;;    allocation analysis, to a fixpoint). Impure bindings are live. The rest
;;;    is live only when a variable it uses or changes is.
;;;
;;; 4. Closed vs. open. If no entry of the expander is live the link is closed: the expander
;;;    and the library tables are gone. Otherwise the program uses `eval`, `load`, etc, and
;;;    the link is open, which registers every linklet with the library tables.
;;;    `--static-report` shows what stays and why, `--static-why NAME` the chain that keeps
;;;    NAME, `--static-mode` forces a mode and `--static-no-shake` keeps everything.
;;;    `--static-size` can show you which part of the program uses what amount of bytes.
;;;
;;; 5. Prelude and entry. After the linklets, `initialize-io-system` runs. The program body
;;;    itself is a thunk run by `with-exception-handler`, whose handler prints the condition and
;;;    exits.
;;;

(library (capy compiler static)
  (export static-link)
  (import (rnrs)
    (rnrs hashtables)
    (capy)
    (core pretty-print)
    (core compiler tree-il terms)
    (only (core compiler tree-il) tree-il->scheme)
    (core compiler tree-il primitives)
    (only (core compiler pipeline) compile-il-to-file)
    (core compiler tree-il fix-letrec)
    (core compiler tree-il well-known-procs)
    (core compiler tree-il assignment-elimination)
    (capy compiler static effects)
    (capy compiler static shake))

  ;;; Access to the boot internals that (capy) does not export
  (define (boot-ref key name)
    (instance-ref (library-instance (root-library-registry) key) name))

  (define term-map-children (boot-ref '(boot units) 'term-map-children))
  (define hoist-private-variables (boot-ref '(boot units) 'hoist-private-variables))
  (define il->linklets (boot-ref '(boot eval) 'il->linklets))
  (define library-wpo-linklets (boot-ref '(boot eval) 'library-wpo-linklets))
  (define boot-wpo-linklets (boot-ref '(boot eval) 'boot-wpo-linklets))
  (define call-with-fasl-epoch (boot-ref '(boot eval) 'call-with-fasl-epoch))
  (define env-origin (boot-ref '(boot environments) 'env-origin))
  (define global-environment (boot-ref '(boot environments) 'global-environment))
  (define env-public-interface (boot-ref '(boot environments) 'env-public-interface))
  (define the-core-env (boot-ref '(boot environments) 'the-core-env))
  (define env-clone (boot-ref '(boot environments) 'env-clone))
  (define library-decl? (boot-ref '(boot environments) 'library-decl?))
  (define library-decl-deps (boot-ref '(boot environments) 'library-decl-deps))
  (define call-with-program-load (boot-ref '(boot eval) 'call-with-program-load))
  (define capy-execution-mode (boot-ref '(boot library-manager) 'capy:execution-mode))
  (define capy-call-with-file-language
    (boot-ref '(boot library-manager) 'capy:call-with-file-language))

  ;; The linker is the one place that knows how a library name becomes a file.
  (define key-text (boot-ref '(boot library-manager) 'library-key->string))
  (define linker-native-entry (boot-ref '(boot linker) 'linker-native-entry))
  (define linker-native-file (boot-ref '(boot linker) 'linker-native-file))
  (define linker-native-key-text (boot-ref '(boot linker) 'linker-native-key-text))
  (define find-library-file (boot-ref '(boot linker) 'linker-find-library-file))

  (define static-key '(boot static))
  (define capy-key '(|#%capy|))

  ;; The linklets whose live bindings make the bundle open. Currently that is macro expander
  ;; and load/eval
  (define expander-keys '((boot psyntax-exp) (boot eval)))
  
  ;; The bindings that expand or compile code at run time. The bundle is open when one is live
  ;; 
  ;; NOTE: `syntax->datum` is special cased to NOT require psyntax loaded
  (define expander-entries
    '(macroexpand compile-tree-il compile-file load %load load-program %eval eval expand-file-units))

  ;; The procedures of the expander that only change what it does when it runs: it keeps
  ;; the meaning of the core forms in tables. Calling one is an effect the shaker keeps when
  ;; an entry of the expander is kept, and not otherwise.
  (define expander-state-procs
    '(((boot psyntax-exp) global-extend)
      ((boot library-manager) capy:update-load-extensions!)))

  (define expander-state-owners '((boot environments)))

  ;; The directory part of PATH, with its trailing slash, or "" for none.
  (define (directory-of path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) ""]
        [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
        [else (loop (- i 1))])))

  (define (launcher-path)
    (let* ([self (or (executable-path) (error 'static-link "cannot find the running executable"))]
           [sibling (string-append (directory-of self) "capy-rt")])
      (if (file-executable? sibling) sibling self)))

  ;; Writes OUT, the launcher with the bundle in the file TMP embedded (`--exe`). The launcher
  ;; loads libcapy from its own directory, so a copy goes next to OUT when there is none. The
  ;; copy is taken from next to the launcher, or from ../lib of it (an install).
  ;; Returns the size of OUT.
  (define (write-executable! out tmp)
    (let* ([launcher (launcher-path)]
           [lib-name (string-append (dll-prefix) "capy" (dll-suffix))]
           [lib (let ([beside (string-append (directory-of launcher) lib-name)])
                  (if (file-exists? beside)
                    beside
                    (string-append (directory-of launcher) "../lib/" lib-name)))]
           [lib-out (string-append (directory-of out) lib-name)])
      (write-executable launcher tmp out)
      (cond
        [(file-exists? lib-out) (if #f #f)]
        [(file-exists? lib)
          (let ([bytes (call-with-port (open-file-input-port lib) get-bytevector-all)])
            (call-with-port (open-file-output-port lib-out (file-options no-fail))
              (lambda (port) (put-bytevector port bytes))))
          (format (current-error-port) ";; copied ~a next to ~a~%" lib-name out)]
        [else
          (format (current-error-port) ";; warning: ~a is not next to ~a and was not found next to ~a~%"
            lib-name out launcher)])
      (file-size-in-bytes out)))

  ;; Puts the shared libraries of the native extensions ENTRIES (`(key path identity)` of the libmap)
  ;; beside the executable OUT, in lib/<package-id>/<name>.<dll>. A file that is there is replaced
  ;; (a signed library is not overwritten in place).
  (define (copy-natives! out entries)
    (for-each
      (lambda (entry)
        (let* ([key (car entry)]
               [directory (string-append (directory-of out) "lib/" (symbol->string (cadr key)) "/")]
               [target (string-append directory (symbol->string (caddr key)) (dll-suffix))]
               [source (linker-native-file entry)])
          (create-directory* directory)
          (when (file-exists? target) (delete-file target))
          (let ([bytes (call-with-port (open-file-input-port source) get-bytevector-all)])
            (call-with-port (open-file-output-port target (file-options no-fail))
              (lambda (port) (put-bytevector port bytes))))
          (format (current-error-port) ";; copied the native extension ~a next to ~a~%" (caddr key) out)))
      entries))

  ;;; Fully static executables (`--static-native`)

  ;; The Strings of TEXT between white space.
  (define (words text)
    (let loop ([i 0] [start #f] [out '()])
      (define (flush) (if start (cons (substring text start i) out) out))
      (cond
        [(= i (string-length text)) (reverse (flush))]
        [(memv (string-ref text i) '(#\space #\newline #\tab #\return)) (loop (+ i 1) #f (flush))]
        [else (loop (+ i 1) (or start i) out)])))

  (define (join-strings strings separator)
    (if (null? strings)
      ""
      (fold-left (lambda (out s) (string-append out separator s)) (car strings) (cdr strings))))

  ;; S as one word of a shell command.
  (define (shell-quote s)
    (string-append "'"
      (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s)))
      "'"))

  ;; S as a C string literal.
  (define (c-string s)
    (string-append "\""
      (apply string-append
        (map (lambda (c) (cond [(char=? c #\") "\\\""] [(char=? c #\\) "\\\\"] [else (string c)])) (string->list s)))
      "\""))

  (define (c-compiler)
    (let ([cc (getenv "CC")]) (if (and cc (not (string=? cc ""))) cc "cc")))

  (define (macos?) (string=? (dll-suffix) ".dylib"))

  ;; The directory that has libcapy_static.a, the runtime as a static library: next to the launcher
  ;; (a build tree) or in ../lib of it (an install).
  (define (static-runtime-directory)
    (let* ([launcher (launcher-path)]
           [name "libcapy_static.a"]
           [beside (directory-of launcher)])
      (cond
        [(file-exists? (string-append beside name)) beside]
        [(file-exists? (string-append beside "../lib/" name)) (string-append beside "../lib/")]
        [else
          (error 'static-link
            (string-append name " is not next to the launcher or in ../lib of it, so a fully static executable"
              " cannot be linked (./bootstrap.sh builds it for the stages and for stage-2/fhs)")
            launcher)])))

  ;; The static form of the native extension ENTRY (`(key file identity)` of the libmap), made by
  ;; `capy build --static-native` (see `(capy pkg native)`): `(key init archive flags)`.
  (define (native-static-form entry)
    (let* ([key (car entry)]
           [file (linker-native-file entry)]
           [suffix (dll-suffix)]
           [base (if (and (>= (string-length file) (string-length suffix))
                      (string=? (substring file (- (string-length file) (string-length suffix)) (string-length file)) suffix))
                  (substring file 0 (- (string-length file) (string-length suffix)))
                  file)]
           [descriptor (string-append base ".link")]
           [datum (and (file-exists? descriptor) (guard (e [#t #f]) (call-with-input-file descriptor read)))])
      (unless (and datum (list? datum) (= (length datum) 6) (eq? (car datum) 'static-native)
                (equal? (cadr datum) key) (equal? (caddr datum) (caddr entry))
                (file-exists? (string-append (directory-of file) (list-ref datum 4))))
        (error 'static-link
          (string-append "the native extension " (symbol->string (caddr key)) " of " (symbol->string (cadr key))
            " has no static form that is current: build its package with `capy build --static-native` "
            "(a prebuilt extension needs an archive, (prebuilt \"libx.dylib\" \"libx.a\"))")
          key))
      (list key (list-ref datum 3) (string-append (directory-of file) (list-ref datum 4)) (list-ref datum 5))))

  ;; The C file that lists the extensions of FORMS (`(key init archive flags)`): the table that `main`
  ;; of libcapy_static.a registers with the runtime.
  (define (table-source forms)
    (string-append
      "/* The native extensions linked into this executable, from capyc --static-native. */\n"
      "struct capy_static_row { const char *key; int (*init)(void *, void *); };\n"
      (apply string-append
        (map (lambda (form) (string-append "extern int " (cadr form) "(void *, void *);\n")) forms))
      "static const struct capy_static_row capy_static_rows[] = {\n"
      (apply string-append
        (map (lambda (form)
              (string-append "  {" (c-string (linker-native-key-text (car form))) ", " (cadr form) "},\n"))
          forms))
      "  {0, 0}\n};\n"
      "const struct capy_static_row *capy_static_extension_table(void) { return capy_static_rows; }\n"))

  ;; Writes OUT, an executable that is linked from libcapy_static.a, the static forms of the
  ;; extensions ENTRIES and a table of them, with the bundle in the file TMP embedded (`--exe
  ;; --static-native`). Nothing but system libraries is loaded at run time. Returns the size of OUT.
  (define (write-native-executable! out tmp entries)
    (let* ([forms (map native-static-form entries)]
           [runtime (static-runtime-directory)]
           [libs-file (string-append runtime "libcapy_static.libs")]
           [runtime-libs (if (file-exists? libs-file)
                          (words (call-with-input-file libs-file (lambda (port) (get-string-all port))))
                          (error 'static-link "libcapy_static.libs is missing next to libcapy_static.a" runtime))]
           [table (string-append out ".table.c")]
           [launcher (string-append out ".launcher")]
           [log (string-append out ".link.log")])
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (when (file-exists? launcher) (delete-file launcher))
          (call-with-output-file table (lambda (port) (display (table-source forms) port)))
          (let* ([arguments
                   (append (list (c-compiler) "-o" launcher table)
                     (apply append (map (lambda (form) (cons (caddr form) (cadddr form))) forms))
                     (list (string-append runtime "libcapy_static.a"))
                     runtime-libs
                     ;; `--exe` adds a section to a copy of this file, which needs room among the load commands
                     (if (macos?) (list "-Wl,-headerpad,0x1000") '()))]
                 [status (system (string-append (join-strings (map shell-quote arguments) " ")
                                  " > " (shell-quote log) " 2>&1"))])
            (unless (eqv? status 0)
              (error 'static-link
                (string-append "linking the executable failed (" (join-strings arguments " ") ")\n"
                  (if (file-exists? log) (call-with-input-file log (lambda (port) (get-string-all port))) ""))
                out)))
          (write-executable launcher tmp out)
          (format (current-error-port) ";; linked ~a with ~a native extension~a~%" out (length forms)
            (if (= (length forms) 1) "" "s"))
          (file-size-in-bytes out))
        (lambda ()
          (for-each (lambda (file) (when (file-exists? file) (delete-file file))) (list table launcher log))))))

  (define (sym-append . parts)
    (string->symbol
      (apply string-append
        (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))

  (define (key->string key) (format #f "~s" key))

  ;; The name of a binding as a symbol. A linklet expanded from its source names its
  ;; private definitions with the identifiers of the source, and the tables of the
  ;; shaker (`expander-state-procs`, the effect tables) and `--static-why` look the
  ;; names up as symbols.
  (define (binding-name id)
    (if (symbol? id)
      id
      (let ([datum (syntax->datum id)])
        (if (symbol? datum) datum id))))

  (define (make-equal-hashtable) (make-hashtable equal-hash equal?))

  (define (filter-map f ls)
    (let loop ([ls ls] [out '()])
      (cond
        [(null? ls) (reverse out)]
        [(f (car ls)) => (lambda (v) (loop (cdr ls) (cons v out)))]
        [else (loop (cdr ls) out)])))

  (define (last-pair ls)
    (if (pair? (cdr ls)) (last-pair (cdr ls)) ls))


  (define (unique-keys keys)
    (let loop ([keys keys] [seen '()])
      (cond
        [(null? keys) (reverse seen)]
        [(member (car keys) seen) (loop (cdr keys) seen)]
        [else (loop (cdr keys) (cons (car keys) seen))])))

  ;; Visits every subterm of T, T first.
  (define (walk t f)
    (f t)
    (term-map-children t (lambda (c) (walk c f) c))
    (if #f #f))

    (define (read-forms path)
    (call-with-input-file path
      (lambda (in)
        (let loop ([acc '()])
          (let ([x (read-syntax in)])
            (if (eof-object? x)
              (reverse acc)
              (loop (cons x acc))))))))

  ;; The keys of the libraries the library of META imported when it was expanded. The decl
  ;; holds canonical keys (see `library-decl-deps`), so there is nothing to resolve here.
  (define (meta-import-keys meta)
    (if (library-decl? meta)
      (map car (library-decl-deps meta))
      '()))

  (define-record-type node
    (fields key (mutable term) file boot? (mutable state) (mutable origin)))

  (define (node-exports n) (linklet-term-exports (node-term n)))
  (define (node-imports n) (linklet-term-imports (node-term n)))
  (define (node-meta n) (linklet-term-meta (node-term n)))

  (define (boot-meta? meta) (and (pair? meta) (eq? (car meta) '|#%boot|)))


  ;; The libraries found current in the extent of one link are checked once.
  (define (static-link file out options)
    (call-with-fasl-epoch (lambda () (static-link* file out options))))

  (define (static-link* file out options)
    (define report? (and (memq 'report options) #t))
    (define size? (and (memq 'size options) #t))
    (define shebang? (not (memq 'no-shebang options)))
    (define forced-mode (cond [(memq 'open options) 'open] [(memq 'closed options) 'closed] [else #f]))
    (define verbose? (and (memq 'verbose options) #t))
    (define exe? (and (memq 'exe options) #t))
    (define static-native? (and (memq 'static-native options) #t))
    (define dump-tree-il? (and (memq 'tree-il options) #t))
    (define dump-bytecode? (and (memq 'bytecode options) #t))
    (define dry-run? (and (memq 'dry-run options) #t))
    (define why-names (filter-map (lambda (o) (and (pair? o) (eq? (car o) 'why) (cdr o))) options))
    (define shake-options (append (if verbose? '(trace-unknown) '()) (if (memq 'no-shake options) '(no-shake) '())))
    (define interesting
      (let ([t (make-eq-hashtable)])
        (for-each (lambda (s) (hashtable-set! t s #t)) interesting-primitive-names)
        t))
    (define (list-head l n) (if (or (= n 0) (null? l)) '() (cons (car l) (list-head (cdr l) (- n 1)))))

    (define nodes (make-equal-hashtable))
    (define missing (make-equal-hashtable))
    (define loaded-files (make-equal-hashtable))
    (define skipped-keys '())

    (define (note fmt . args)
      (when verbose?
        (apply format (current-error-port) fmt args)))

    ;;; Expanding

    (define (expand-file path env)
      (note ";; static: expanding ~a (~a mode)~%" path (capy-execution-mode))
      (capy-call-with-file-language path (capy-execution-mode)
        (lambda ()
          (il->linklets (compile-tree-il (read-forms path) env path)))))

    ;; ORIGIN says where the terms come from: `wpo` for the tree-IL kept in the `.wpo` file of a
    ;; compiled library, `source` for the expansion of the source file.
    (define (register-linklets! path boot? terms0 origin)
      (define terms (map hoist-private-variables terms0))
      (for-each
        (lambda (t)
          (let ([key (linklet-term-name t)])
            (unless (hashtable-ref nodes key #f)
              (hashtable-set! nodes key (make-node key t path boot? 'new origin)))))
        terms))

    (define (clone-env env) (env-clone env))

    ;; Expanding psyntax-exp.scm in the running expander corrupts it for every expansion that
    ;; follows (even in a copy of the environment), so it is expanded last, after the
    ;; dependencies are discovered. 
    ;; Limitation: the cause is not understood; the workaround is this ordering.
    (define deferred-stems '("psyntax-exp"))
    (define pending-deferred '())     ; ((key . path) ...)

    (define (deferred-path? path)
      (exists (lambda (stem) (string-suffix? (string-append "/" stem ".scm") path)) deferred-stems))

    (define (string-suffix? suffix str)
      (let ([n (string-length str)] [m (string-length suffix)])
        (and (>= n m) (string=? (substring str (- n m) n) suffix))))

    ;; Loads the file PATH of the library KEY. A file whose compiled file has a current
    ;; `.wpo` file is taken from it, the others (the files that were not compiled with
    ;; `--wpo`, or changed since) are expanded from their source.
    (define (load-file! path key)
      (unless (hashtable-ref loaded-files path #f)
        (hashtable-set! loaded-files path #t)
        (let ([boot? (and (%boot-header-requires path) #t)])
          (cond
            [(and boot? (boot-wpo-linklets key path))
              => (lambda (wpo)
                   (note ";; static: ~a from ~a~%" path (car wpo))
                   (register-linklets! path #t (cdr wpo) 'wpo))]
            [(and boot? (deferred-path? path))
              (let* ([stem (let ([m (string-length path)])
                             (substring path (+ 1 (let loop ([i (- m 1)]) (if (char=? (string-ref path i) #\/) i (loop (- i 1)))))
                               (- m 4)))]
                     [key (list 'boot (string->symbol stem))])
                (set! pending-deferred (cons (cons key path) pending-deferred))
                (hashtable-set! nodes key (make-node key #f path #t 'new 'source)))]
            [(and (not boot?) (library-wpo-linklets key))
              => (lambda (wpo)
                   (note ";; static: ~a from ~a~%" path (car wpo))
                   (register-linklets! path #f (cdr wpo) 'wpo))]
            [else
              (register-linklets! path boot?
                (expand-file path (if boot? (clone-env the-core-env) (current-namespace)))
                'source)]))))

    (define (expand-deferred!)
      (for-each
        (lambda (entry)
          (let ([terms (expand-file (cdr entry) (clone-env the-core-env))])
            (for-each
              (lambda (t0)
                (let* ([t (hoist-private-variables t0)]
                       [n (hashtable-ref nodes (linklet-term-name t) #f)])
                  (if (and n (not (node-term n)))
                    (node-term-set! n t)
                    (register-linklets! (cdr entry) #t (list t) 'source))))
              terms)))
        (reverse pending-deferred))
      (set! pending-deferred '()))


    ;; Loads the libraries of the `.wpo` file WPO, a pair of its path and the terms in it.
    (define (load-wpo! wpo)
      (unless (hashtable-ref loaded-files (car wpo) #f)
        (hashtable-set! loaded-files (car wpo) #t)
        (note ";; static: ~a~%" (car wpo))
        (register-linklets! (car wpo) #f (cdr wpo) 'wpo)))

    ;; The node of the library KEY. KEY is a canonical key: the decls and the linklet imports
    ;; hold nothing else. The library comes from its `.wpo` file when it has a current one
    ;; (found through the libmap, or next to the compiled file in the load path), otherwise
    ;; from the source file in the load path. A library of a package has no source file there.
    (define (find-node key strict?)
      (or (hashtable-ref nodes key #f)
        (and (not (hashtable-ref missing key #f))
          (let ([path (find-library-file key)])
            (cond
              [path (load-file! path key)]
              [(library-wpo-linklets key) => load-wpo!])
            (or (hashtable-ref nodes key #f)
              (begin (hashtable-set! missing key #t) #f))))
        (begin
          (when strict?
            (error 'static-link (string-append "no linklet found for " (key-text key)) key))
          (set! skipped-keys (cons key skipped-keys))
          #f)))

    ;;; The program

    (define program-node
      (capy-call-with-file-language file (capy-execution-mode)
        (lambda ()
          (let* ([forms (read-forms file)]
                 [code (call-with-program-load #t
                         (lambda () (compile-tree-il forms (current-namespace) file)))]
                 [terms (il->linklets code)])
            (unless (and (pair? terms) (null? (cdr terms))
                      (pair? (linklet-term-name (car terms)))
                      (eq? (car (linklet-term-name (car terms))) '%program))
              (error 'static-link "not a program: it needs an (import ...) form" file))
            (let ([n (make-node (linklet-term-name (car terms)) (car terms) file #f 'new 'source)])
              (hashtable-set! nodes (node-key n) n)
              n)))))

    ;;; Dependencies of a node: a list of (key . strict?).
    (define (node-dependencies n)
      (define t (node-term n))
      (define meta (if t (linklet-term-meta t) '()))
      (define header (if (node-boot? n) (or (%boot-header-requires (node-file n)) '()) '()))
      (define header-keys
        (filter-map
          (lambda (spec)
            (cond
              [(and (pair? spec) (eq? (car spec) 'only)) (cadr spec)]
              [(pair? spec) spec]
              [else #f]))
          header))
      (define group-keys (if t (map car (linklet-term-imports t)) '()))
      (define (ok? key) (and (not (equal? key capy-key)) (not (equal? key (node-key n)))))
      (append
        (map (lambda (k) (cons k #t)) (filter ok? (unique-keys header-keys)))
        (map (lambda (k) (cons k #f)) (filter ok? (meta-import-keys meta)))
        (map (lambda (k) (cons k #t)) (filter ok? (unique-keys group-keys)))))

    ;;; The (capy) environment
    (define prim-names
      (let ([table (make-eq-hashtable)])
        (for-each (lambda (sym) (hashtable-set! table sym #t))
          (instance-symbols (library-instance (root-library-registry) capy-key)))
        table))

    (define capy-interface (env-public-interface (global-environment '(capy))))
    (define (capy-origin name) (env-origin capy-interface name))

    ;;; The link, once for a mode. Returns a vector of the results.
    (define prelude-names
      '(initialize-io-system with-exception-handler flush-output-port current-output-port
        current-error-port print-condition exit))

    ;; The linklets that install the runtime hooks the Rust runtime calls instead of looking
    ;; names up (`install-runtime-hook!`). An open bundle also has the linker, which `load`
    ;; and `eval` in `(boot eval)` use to run what they compile.
    (define (hook-installers open?)
      (if open?
        '((boot exceptions) (boot raise) (boot base) (boot expand) (boot linker))
        '((boot exceptions) (boot raise) (boot base))))

    ;; The nodes in the order they run
    (define (visit-all! open?)
      (let-values ([(keys vals) (hashtable-entries nodes)])
        (vector-for-each (lambda (n) (node-state-set! n 'new)) vals))
      (let ([order '()])
        (define (visit! key strict?)
          (let ([n (find-node key strict?)])
            (when (and n (eq? (node-state n) 'new))
              (node-state-set! n 'gray)
              (for-each (lambda (d) (visit! (car d) (cdr d))) (node-dependencies n))
              (node-state-set! n 'done)
              (set! order (cons n order)))))
        (for-each
          (lambda (name)
            (let ([o (capy-origin name)])
              (when (and o (not (equal? (car o) capy-key)))
                (visit! (car o) #t))))
          prelude-names)
        (set! order (cons 'init-io order))
        (for-each (lambda (key) (visit! key #t)) (hook-installers open?))
        (visit! (node-key program-node) #t)
        (reverse order)))

    (define (plan open?)
      ;;; Order
      (define order (visit-all! open?))
      (let ([program-last (and (pair? order) (eq? (car (last-pair order)) program-node))])
        (unless program-last
          (error 'static-link "the program must run last")))
      (define dep-nodes
        (filter (lambda (e) (and (node? e) (not (eq? e program-node)))) order))
      (define all-nodes (append dep-nodes (list program-node)))

      ;;; Primitive precedence
      (define shadow-owner (make-eq-hashtable))   ; name -> node
      (for-each
        (lambda (n)
          (when (node-boot? n)
            (for-each
              (lambda (sym)
                (when (or (hashtable-ref prim-names sym #f) (hashtable-ref interesting sym #f))
                  (hashtable-set! shadow-owner sym n)))
              (node-exports n))))
        dep-nodes)

      ;;; Targets
      ;; A target says what a slot of some linklet stands for:
      ;;   (lvar name)      a lexical variable of the merged letrec*
      ;;   (prim sym slot)  a variable of the (#%capy) instance
      (define capy-slots (make-eq-hashtable))
      (define capy-slot-order '())
      (define (capy-target sym)
        (or (hashtable-ref capy-slots sym #f)
          (let ([tg (list 'prim sym (sym-append "%capy:" sym))])
            (hashtable-set! capy-slots sym tg)
            (set! capy-slot-order (cons tg capy-slot-order))
            tg)))

      (define export-tables (make-eq-hashtable))   ; node -> (eq-hashtable sym -> target)
      (for-each
        (lambda (n)
          (let ([table (make-eq-hashtable)])
            (for-each
              (lambda (sym)
                (hashtable-set! table sym
                  (list 'lvar (sym-append "%" (key->string (node-key n)) ":" sym))))
              (node-exports n))
            (hashtable-set! export-tables n table)))
        all-nodes)

      (define (export-target key sym from)
        (if (equal? key capy-key)
          (capy-target sym)
          (let ([provider (hashtable-ref nodes key #f)])
            (unless (and provider (hashtable-ref export-tables provider #f))
              (error 'static-link "the linklet is not part of the link" key from))
            (or (hashtable-ref (hashtable-ref export-tables provider #f) sym #f)
              (error 'static-link "linklet does not export" key sym from)))))

      ;; The slot table of a node: slot -> target.
      (define slot-tables (make-eq-hashtable))
      (for-each
        (lambda (n)
          (let ([t (node-term n)]
                [table (make-eq-hashtable)])
            (let loop ([groups (linklet-term-imports t)] [slots (linklet-term-slots t)])
              (cond
                [(null? groups)
                  (for-each
                    (lambda (sym slot)
                      (hashtable-set! table slot (hashtable-ref (hashtable-ref export-tables n #f) sym #f)))
                    (linklet-term-exports t)
                    slots)]
                [else
                  (let inner ([syms (cdr (car groups))] [slots slots])
                    (if (null? syms)
                      (loop (cdr groups) slots)
                      (begin
                        (hashtable-set! table (car slots)
                          (export-target (car (car groups)) (car syms) (node-key n)))
                        (inner (cdr syms) (cdr slots)))))]))
            (hashtable-set! slot-tables n table)))
        all-nodes)

      (define own-slot-tables (make-eq-hashtable))
      (for-each
        (lambda (n)
          (let* ([t (node-term n)]
                 [table (make-eq-hashtable)])
            (for-each (lambda (slot) (hashtable-set! table slot #t))
              (list-tail (linklet-term-slots t)
                (- (length (linklet-term-slots t)) (length (linklet-term-exports t)))))
            (hashtable-set! own-slot-tables n table)))
        all-nodes)

      ;;; Splitting the bodies
      ;; A body is flattened to the `fix` bindings of the local definitions and the top-level
      ;; forms, in order.
      (define (split-body n)
        (let ([ids '()] [lhs '()] [rhs '()] [forms '()])
          (let loop ([t (linklet-term-body (node-term n))])
            (cond
              [(sequence? t) (loop (sequence-head t)) (loop (sequence-tail t))]
              [(void? t) (if #f #f)]
              [(fix? t)
                (set! ids (append (reverse (fix-ids t)) ids))
                (set! lhs (append (reverse (fix-lhs t)) lhs))
                (set! rhs (append (reverse (fix-rhs t)) rhs))
                (loop (fix-body t))]
              [else (set! forms (cons t forms))]))
          (vector (reverse ids) (reverse lhs) (reverse rhs) (reverse forms))))

      (define splits (make-eq-hashtable))
      (for-each (lambda (n) (hashtable-set! splits n (split-body n))) all-nodes)

      (define (lookup-target n slot)
        (hashtable-ref (hashtable-ref slot-tables n #f) slot #f))

      ;; The definition a top-level form is: (target . value) for a global-set on an own slot.
      (define (own-definition n form)
        (and (global-set? form)
          (let ([tg (lookup-target n (global-set-slot form))])
            (and tg
              (hashtable-ref (hashtable-ref own-slot-tables n #f) (global-set-slot form) #f)
              (cons tg (global-set-value form))))))

      ;;; Counting the assignments of every variable
      (define total-sets (make-eq-hashtable))
      (define spine-sets (make-eq-hashtable))
      (define (bump! table name) (hashtable-set! table name (+ 1 (hashtable-ref table name 0))))
      (define (count-sets! n t)
        (walk t
          (lambda (x)
            (when (global-set? x)
              (let ([tg (lookup-target n (global-set-slot x))])
                (when (and tg (eq? (car tg) 'lvar))
                  (bump! total-sets (cadr tg))))))))
      (for-each
        (lambda (n)
          (let ([split (hashtable-ref splits n #f)])
            (for-each (lambda (rhs) (count-sets! n rhs)) (vector-ref split 2))
            (for-each
              (lambda (form)
                (let ([def (own-definition n form)])
                  (cond
                    [(and def (eq? (car (car def)) 'lvar))
                      (bump! total-sets (cadr (car def)))
                      (bump! spine-sets (cadr (car def)))
                      (count-sets! n (cdr def))]
                    [else (count-sets! n form)])))
              (vector-ref split 3))))
        all-nodes)
      (define (simple? name)
        (and (= 1 (hashtable-ref total-sets name 0))
          (= 1 (hashtable-ref spine-sets name 0))))

      (define (lvar-term src name lvar)
        (make-lref src name lvar))

      ;;; Rewriting
      (define (target-ref t tg)
        (case (car tg)
          [(lvar) (lvar-term (term-src t) (global-ref-name t) (cadr tg))]
          [else (make-global-ref (term-src t) capy-key (cadr tg) (caddr tg))]))
      (define (target-set t tg value)
        (case (car tg)
          [(lvar)
            (make-lset (term-src t) (global-set-name t) (cadr tg) value)]
          [else (make-global-set (term-src t) capy-key (cadr tg) (caddr tg) value)]))

      ;; The export of the boot linklet that shadows the primitive SYM, as a target.
      (define (shadow-target sym)
        (let ([owner (hashtable-ref shadow-owner sym #f)])
          (and owner (hashtable-ref (hashtable-ref export-tables owner #f) sym #f))))

      (define (defines? n sym)
        (and (hashtable-ref (hashtable-ref export-tables n #f) sym #f) #t))

      (define (rewrite-ref n t)
        (let ([tg (lookup-target n (global-ref-slot t))])
          (cond
            [(not tg) t]
            [(and (eq? (car tg) 'prim) (not (defines? n (cadr tg))) (shadow-target (cadr tg)))
              => (lambda (shadow) (target-ref t shadow))]
            [else (target-ref t tg)])))

      ;; A `.wpo` file holds the tree-IL after the primitives were resolved, so a name that
      ;; is a primitive only to the expander (call/cc, call-with-current-continuation,
      ;; call/1cc: the boot defines them) is a `primref` or a `primcall` there. Left alone,
      ;; the bundle looks the name up in the primitive instance when it runs, and nothing
      ;; defines it there. It refers to the boot linklet that defines it, as the global
      ;; reference of an expanded source does.
      (define native-names
        (let ([table (make-eq-hashtable)])
          (for-each (lambda (s) (hashtable-set! table s #t)) (native-primitive-names))
          table))
      (define (boot-defined-target n sym)
        (and (not (hashtable-ref native-names sym #f))
          (not (defines? n sym))
          (shadow-target sym)))

      (define (rewrite n t)
        (let rw ([t t])
          (cond
            [(global-ref? t) (rewrite-ref n t)]
            [(and (primref? t) (boot-defined-target n (primref-prim t)))
              => (lambda (tg) (lvar-term (term-src t) (primref-prim t) (cadr tg)))]
            [(and (primcall? t) (boot-defined-target n (primcall-prim t)))
              => (lambda (tg)
                   (make-application (term-src t)
                     (lvar-term (term-src t) (primcall-prim t) (cadr tg))
                     (map rw (primcall-args t))))]
            [(global-set? t)
              (let ([tg (lookup-target n (global-set-slot t))]
                    [value (rw (global-set-value t))])
                (if tg
                  (target-set t tg value)
                  (make-global-set (term-src t) (global-set-key t) (global-set-name t)
                    (global-set-slot t) value)))]
            [else (term-map-children t rw)])))

      (define (origin-term name)
        (let ([o (capy-origin name)])
          (unless o (error 'static-link "the prelude needs a name (capy) does not export" name))
          (let ([tg (cond
                      [(equal? (car o) capy-key)
                        (or (shadow-target (cdr o)) (capy-target (cdr o)))]
                      [else (hashtable-ref (hashtable-ref export-tables (find-node (car o) #t) #f) (cdr o) #f)])])
            (unless tg (error 'static-link "no variable for" name o))
            (case (car tg)
              [(lvar) (lvar-term #f name (cadr tg))]
              [else (make-global-ref #f capy-key (cadr tg) (caddr tg))]))))

      (define effect-counter 0)
      (define (effect-name)
        (set! effect-counter (+ effect-counter 1))
        (sym-append "%effect-" (number->string effect-counter)))

      (define void-bindings '())   ; the variables that are assigned: bound to void first
      (for-each
        (lambda (n)
          (for-each
            (lambda (sym)
              (let ([tg (hashtable-ref (hashtable-ref export-tables n #f) sym #f)])
                (when (and (eq? (car tg) 'lvar) (not (simple? (cadr tg))))
                  (set! void-bindings
                    (cons (make-sbinding (node-key n) sym (cadr tg) (make-void #f)) void-bindings)))))
            (node-exports n)))
        all-nodes)
      (set! void-bindings (reverse void-bindings))

      (define (node-bindings n)
        (let* ([split (hashtable-ref splits n #f)]
               [out '()]
               [add! (lambda (id lhs rhs)
                       (set! out (cons (make-sbinding (node-key n) (binding-name id) lhs rhs) out)))])
          (for-each (lambda (id lhs rhs) (add! id lhs (rewrite n rhs)))
            (vector-ref split 0) (vector-ref split 1) (vector-ref split 2))
          (for-each
            (lambda (form)
              (let ([def (own-definition n form)])
                (cond
                  [(and def (eq? (car (car def)) 'lvar) (simple? (cadr (car def))))
                    (add! (global-set-name form) (cadr (car def)) (rewrite n (cdr def)))]
                  [else
                    (for-each (lambda (entry) (add! (car entry) (cadr entry) (caddr entry)))
                      (flatten-effect (rewrite n form) effect-name))])))
            (vector-ref split 3))
          (reverse out)))

      (define (trace-binding key mark)
        (let ([bv (string->utf8 (string-append mark (format #f "~s" key) "\n"))])
          (make-sbinding #f '%effect (effect-name)
            (make-application #f (origin-term 'syscall:write)
              (list (make-constant #f 2) (make-constant #f bv)
                (make-constant #f (bytevector-length bv)) (make-constant #f 0))))))

      ;; The bindings of the dependencies in order
      ;; Open mode registers every linklet as a library instance, like the boot does when it
      ;; instantiates one, so that `eval`, `environment` and the library manager find them
      (define (prim-term name)
        (unless (hashtable-ref prim-names name #f)
          (error 'static-link "the registration needs a primitive that does not exist" name))
        (let ([tg (capy-target name)])
          (make-global-ref #f capy-key (cadr tg) (caddr tg))))

      (define (boot-term key name)
        (let ([tg (export-target key name 'registration)])
          (case (car tg)
            [(lvar) (lvar-term #f name (cadr tg))]
            [else (make-global-ref #f capy-key (cadr tg) (caddr tg))])))

      (define (target-value name tg)
        (case (car tg)
          [(lvar) (lvar-term #f name (cadr tg))]
          [else (make-global-ref #f capy-key (cadr tg) (caddr tg))]))
          
      ;; The registration of the node N: the bindings that make its instance, fill it and
      ;; register it, and the one that fills the variables that are assigned after their
      ;; definition (or #f). Filling copies the value of a variable into the instance, so
      ;; those are filled at the end, after every top-level assignment; the others are as
      ;; they are once defined.
      (define (registered? n)
        (and (not (equal? (node-key n) static-key))
          (or (boot-meta? (node-meta n)) (library-decl? (node-meta n)))))
      (define (registration n)
        (let* ([key (node-key n)]
               [exports (node-exports n)]
               [table (hashtable-ref export-tables n #f)]
               [inst (sym-append "%static-instance:" (key->string key))]
               [inst-ref (lambda () (make-lref #f 'instance inst))]
               [fill (lambda (sym)
                       (make-application #f (prim-term 'variable-set!)
                         (list
                           (make-application #f (prim-term 'instance-ensure-variable!)
                             (list (inst-ref) (make-constant #f sym)))
                           (target-value sym (hashtable-ref table sym #f)))))]
               [defined? (lambda (sym)
                           (let ([tg (hashtable-ref table sym #f)])
                             (and (eq? (car tg) 'lvar) (simple? (cadr tg)))))]
               [now (filter defined? exports)]
               [later (filter (lambda (sym) (not (defined? sym))) exports)]
               [after
                 (if (boot-meta? (node-meta n))
                   (make-application #f (boot-term '(boot environments) 'env-mirror-exports!)
                     (list (boot-term '(boot environments) 'the-core-env)
                       (inst-ref) (make-constant #f exports)))
                   (make-application #f (boot-term '(boot environments) 'env-library-instantiated!)
                     (list (make-constant #f (node-meta n)) (inst-ref)
                       (make-constant #f exports))))]
               [register
                 (make-application #f (prim-term 'register-library-instance!)
                   (list (make-application #f (boot-term '(boot environments) 'current-library-registry) '())
                     (make-constant #f key) (inst-ref)))]
               [sequence (lambda (terms last) (fold-right (lambda (e rest) (make-sequence #f e rest)) last terms))])
          (values
            (list
              (make-sbinding #f '%instance inst
                (make-application #f (prim-term 'make-linklet-instance)
                  (list (make-constant #f key) (make-constant #f #f))))
              (make-sbinding #f '%effect (effect-name)
                (sequence (map fill now) (make-sequence #f after register))))
            (and (pair? later)
              (make-sbinding #f '%effect (effect-name)
                (sequence (map fill (cdr later)) (fill (car later))))))))

      ;; The bindings of the dependencies in order. Open mode registers every linklet as a
      ;; library instance, like the boot does when it instantiates one, so that `eval`,
      ;; `environment` and the library manager find them. A linklet is registered right
      ;; after its bindings, so that its variables are not live to the end of the program
      ;; (they were, and for a big program the compiler has every one of them live in every
      ;; block); the ones up to the last boot linklet after it, since registering needs
      ;; the boot.
      (define dep-entries (filter (lambda (e) (not (eq? e program-node))) order))
      (define last-boot
        (let loop ([es dep-entries] [i 0] [last -1])
          (cond
            [(null? es) last]
            [(and (node? (car es)) (node-boot? (car es))) (loop (cdr es) (+ i 1) i)]
            [else (loop (cdr es) (+ i 1) last)])))
      (define dep-bindings
        (let loop ([es dep-entries] [i 0] [out '()] [pending '()] [late '()])
          (if (null? es)
            (append (reverse out) (reverse pending) (reverse late))
            (let* ([e (car es)]
                   [out (append
                          (reverse
                            (if (eq? e 'init-io)
                              (list (make-sbinding #f '%effect (effect-name)
                                      (make-application #f (origin-term 'initialize-io-system) '())))
                              (node-bindings e)))
                          out)])
              (let-values ([(now later)
                            (if (and open? (node? e) (registered? e))
                              (registration e)
                              (values '() #f))])
                (let* ([late (if later (cons later late) late)]
                       [pending (append (reverse now) pending)])
                  (if (>= i last-boot)
                    (loop (cdr es) (+ i 1) (append pending out) '() late)
                    (loop (cdr es) (+ i 1) out pending late))))))))
      (define program-bindings (node-bindings program-node))

      ;;; The entry
      (define handler-arg '%static-condition)
      (define handler
        (let ([c (make-lref #f 'condition handler-arg)]
              [call (lambda (name . args) (make-application #f (origin-term name) args))])
          (make-proc #f (list handler-arg)
            (make-sequence #f
              (call 'flush-output-port (call 'current-output-port))
              (make-sequence #f
                (call 'print-condition c (call 'current-error-port))
                (call 'exit (make-constant #f 70))))
            '() '(condition))))

      ;; What the entry itself needs.
      (define entry-roots
        (list handler
          (origin-term 'with-exception-handler)
          (origin-term 'exit)))

      ;;; Shaking
      (define all-bindings (append void-bindings dep-bindings program-bindings))
      (define observers
        (filter-map
          (lambda (b)
            (and (sbinding-owner b) (member (sbinding-owner b) expander-keys)
              (memq (sbinding-id b) expander-entries)
              (sbinding-lhs b)))
          all-bindings))

      (define expander-state
        (filter-map
          (lambda (b)
            (and (sbinding-owner b)
              (or (member (sbinding-owner b) expander-state-owners)
                (member (list (sbinding-owner b) (sbinding-id b)) expander-state-procs))
              (sbinding-lhs b)))
          all-bindings))

      (define shaken (shake all-bindings entry-roots
                       (cons* (cons 'observed-by-expander observers)
                         (cons 'export-owner static-key)
                         (cons 'forced-owner (node-key program-node))
                         (cons 'expander-state expander-state)
                         shake-options)))

      (define (live? b) (shaken-live? shaken b))

      (define expander-blocker
        (find
          (lambda (b)
            (and (sbinding-owner b) (live? b)
              (member (sbinding-owner b) expander-keys)
              (memq (sbinding-id b) expander-entries)))
          all-bindings))
      (define expander-live? (and expander-blocker #t))
      (define blocker-chain (and expander-blocker (retention-chain shaken expander-blocker)))

      (define (build-letrec* bindings body)
        (make-let #f 'letrec*
          (map sbinding-id bindings)
          (map sbinding-lhs bindings)
          (map sbinding-rhs bindings)
          body))
      (define (build-body)
        (let* ([program-thunk
                 (make-proc #f '()
                   (build-letrec* (filter live? program-bindings) (make-void #f))
                   '() '())]
               [final
                 (make-sequence #f
                   (make-application #f (origin-term 'with-exception-handler) (list handler program-thunk))
                   (make-application #f (origin-term 'exit) (list (make-constant #f 0))))])
          (build-letrec* (filter live? (append void-bindings dep-bindings)) final)))

      ;; Value references to primitives that have to be native procedures: a name that is not
      ;; defined by a boot linklet is looked up in the primitive instance when it runs. A boot
      ;; linklet defines a `(#%capy)` name either by exporting it (see `shadow-target`) or by
      ;; assigning the `(#%capy)` variable, as `(define (call/cc f) ...)` in `boot/control.scm`
      ;; does; such a name is as good as a native one.
      (define (audit-primitives!)
        (let ([native (make-eq-hashtable)]
              [bad (make-eq-hashtable)])
          (for-each (lambda (s) (hashtable-set! native s #t)) (native-primitive-names))
          (for-each
            (lambda (b)
              (when (live? b)
                (let scan ([t (sbinding-rhs b)])
                  (when (and (global-set? t) (equal? (global-set-key t) capy-key))
                    (hashtable-set! native (global-set-name t) #t))
                  (term-map-children t (lambda (c) (scan c) c)))))
            all-bindings)
          (for-each
            (lambda (b)
              (when (live? b)
                (let scan ([t (sbinding-rhs b)] [operator? #f])
                  (cond
                    [(and (global-ref? t) (equal? (global-ref-key t) capy-key) (not operator?))
                      (let ([name (global-ref-name t)])
                        (unless (hashtable-ref native name #f)
                          (hashtable-set! bad name (sbinding-id b))))]
                    [(application? t)
                      (scan (application-operator t) #t)
                      (for-each (lambda (x) (scan x #f)) (application-operands t))]
                    [else (term-map-children t (lambda (c) (scan c #f) c))]))))
            all-bindings)
          (let-values ([(names owners) (hashtable-entries bad)])
            (when (> (vector-length names) 0)
              (error 'static-link
                "primitives referenced as values are neither native nor defined by a bundled boot linklet"
                (vector->list names) (vector->list owners))))))

      (vector open? dep-nodes order capy-slot-order
        build-body all-bindings shaken expander-live?
        audit-primitives! program-bindings blocker-chain))

    ;;; Choosing the mode

    ;; Discover every file first, then expand the expander's own.
    (define t-start (current-jiffy))
    (visit-all! #t)
    (expand-deferred!)
    (define t-collected (current-jiffy))

    (define (check-effect-tables! library-check?)
      (let ([primitives (make-eq-hashtable)]
            [library (make-eq-hashtable)])
        (for-each (lambda (s) (hashtable-set! primitives s #t)) (native-primitive-names))
        (let-values ([(keys vals) (hashtable-entries nodes)])
          (vector-for-each
            (lambda (n)
              (when (and (node-term n) (system-owner? (node-key n)))
                (for-each (lambda (s) (hashtable-set! library s #t)) (node-exports n))))
            vals))
        (for-each
          (lambda (entry)
            (let* ([known? (if (eq? (car entry) 'primitive)
                             (lambda (s)
                               (or (hashtable-ref primitives s #f) (hashtable-ref prim-names s #f)
                                 (hashtable-ref interesting s #f)))
                             (lambda (s) (hashtable-ref library s #f)))]
                   [stale (filter (lambda (s) (not (known? s))) (unique-keys (cdr entry)))])
              (unless (or (null? stale) (and (eq? (car entry) 'library) (not library-check?)))
                (format (current-error-port) ";; static: ~a names in the effect tables that nothing defines: ~s~%"
                  (car entry) stale))))
          (effect-table-names))))
    (define first-plan (plan (eq? forced-mode 'open)))
    (define chosen
      (cond
        [forced-mode first-plan]
        [(vector-ref first-plan 7) (plan #t)]
        [else first-plan]))
    (define t-planned (current-jiffy))

    (define open? (vector-ref chosen 0))
    (define dep-nodes (vector-ref chosen 1))
    (define capy-slot-order (reverse (vector-ref chosen 3)))
    (define all-bindings (vector-ref chosen 5))
    (define shaken (vector-ref chosen 6))
    (define (live? b) (shaken-live? shaken b))
    (define program-bindings (vector-ref chosen 9))
    (when verbose? (check-effect-tables! open?))

    (define (seconds from to) (/ (round (/ (* 100.0 (- to from)) (jiffies-per-second))) 100.0))

    (define (print-report)
      (define by-owner (make-equal-hashtable))   ; key -> (live . total)
      (for-each
        (lambda (b)
          (let* ([key (sbinding-owner b)]
                 [c (hashtable-ref by-owner key (cons 0 0))])
            (hashtable-set! by-owner key
              (cons (+ (car c) (if (live? b) 1 0)) (+ (cdr c) 1)))))
        all-bindings)
      (let* ([stats (map (lambda (n)
                           (let ([c (hashtable-ref by-owner (node-key n) (cons 0 0))])
                             (cons n c)))
                      (append dep-nodes (list program-node)))]
             [kept (filter (lambda (s) (> (cadr s) 0)) stats)]
             [dropped (filter (lambda (s) (= (cadr s) 0)) stats)]
             [live-count (length (filter live? all-bindings))])
        (format #t ";; static-report: ~a mode~%" (if open? "open" "closed"))
        (when (and open? (not forced-mode))
          (format #t ";; static-report: closed mode keeps an expander entry alive, so the link was redone open:~%")
          (print-chain (vector-ref first-plan 10)))
        (format #t ";; linklets: ~a collected, ~a kept, ~a dropped~%"
          (length stats) (length kept) (length dropped))
        (format #t ";; bindings: ~a total, ~a live~%" (length all-bindings) live-count)
        (format #t ";; effects on objects nothing else uses: ~a kept, ~a dropped~%"
          (shaken-conditional-live shaken) (shaken-conditional-dropped shaken))
        (let ([count (lambda (origin)
                       (length (filter (lambda (s) (eq? (node-origin (car s)) origin)) stats)))])
          (format #t ";; linklet sources: ~a from .wpo files, ~a from source~%"
            (count 'wpo) (count 'source)))
        (format #t ";; kept:~%")
        (for-each
          (lambda (s)
            (format #t ";;   ~a ~a/~a  [~a]~%" (key-text (node-key (car s))) (cadr s) (cddr s)
              (node-origin (car s))))
          kept)
        (format #t ";; dropped:~%")
        (for-each
          (lambda (s)
            (format #t ";;   ~a (~a bindings)  [~a]~%" (key-text (node-key (car s))) (cddr s)
              (node-origin (car s))))
          dropped)
        (format #t ";; largest retained linklets:~%")
        (for-each
          (lambda (s)
            (format #t ";;   ~a  ~a~%" (cadr s) (key-text (node-key (car s)))))
          (list-head (list-sort (lambda (a b) (> (cadr a) (cadr b))) kept) (min 10 (length kept))))
        (format #t ";; root causes (effects that keep the most alive: bindings only they retain / all they retain):~%")

        (let ([lines '()])   ; ((text . count) ...), in order
          (for-each
            (lambda (entry)
              (let* ([b (caddr entry)]
                     [text (format #f "~a / ~a  ~s ~s, impure because ~s" (car entry) (cadr entry)
                             (sbinding-owner b)
                             (if (eq? (sbinding-id b) '%effect) (describe-term (sbinding-rhs b)) (sbinding-id b))
                             (shaken-culprit shaken b))]
                     [seen (assoc text lines)])
                (if seen
                  (set-cdr! seen (+ (cdr seen) 1))
                  (set! lines (append lines (list (cons text 1)))))))
            (shaken-root-causes shaken (if verbose? 40 10)))
          (for-each
            (lambda (line)
              (format #t ";;   ~a~a~%"
                (if (= (cdr line) 1) "" (format #f "~a x " (cdr line)))
                (car line)))
            lines))
        (unless (null? skipped-keys)
          (format #t ";; imports without a library file (virtual libraries): ~a~%" (map key-text (unique-keys skipped-keys))))
        (format #t ";; psyntax: ~a~%"
          (let ([entry (assoc '(boot psyntax-exp)
                         (map (lambda (st) (cons (node-key (car st)) (cadr st))) stats))])
            (cond
              [(not entry) "dropped (never collected)"]
              [(> (cdr entry) 0) "kept"]
              [else "dropped"])))))

    (define (print-chain chain)
      (for-each
        (lambda (link)
          (format #t ";;   ~s ~a~a~%" (sbinding-owner link) (sbinding-id link)
            (if (eq? (sbinding-id link) '%effect)
              (format #f "  [~s; impure because ~s]" (describe-term (sbinding-rhs link))
                (shaken-culprit shaken link))
              "")))
        chain))

    ;; --static-why NAME: the chain of bindings that keeps NAME alive
    (define (print-why name)
      (let ([matches (filter
                       (lambda (b)
                         (let ([id (format #f "~a" (sbinding-id b))])
                           (or (string=? id name)
                             (string=? (format #f "~s/~a" (sbinding-owner b) id) name))))
                       all-bindings)])
        (if (null? matches)
          (format #t ";; static-why: no binding is called ~a~%" name)
          (for-each
            (lambda (b)
              (if (not (live? b))
                (format #t ";; static-why: ~s ~a is not in the bundle (dropped by the shaker)~%" (sbinding-owner b) (sbinding-id b))
                (begin
                  (format #t ";; static-why: ~a is retained by:~%" name)
                  (print-chain (retention-chain shaken b)))))
            matches))))
    ;; --static-size: where the bytes of the image FASL go
    (define (pad-left s width)
      (if (>= (string-length s) width)
        s
        (string-append (make-string (- width (string-length s)) #\space) s)))
    (define (pad-right s width)
      (if (>= (string-length s) width)
        s
        (string-append s (make-string (- width (string-length s)) #\space))))
    (define (percent n total)
      (if (= total 0) "0.0" (number->string (/ (round (/ (* 1000.0 n) total)) 10.0))))
    (define (print-size-report fasl)
      (let* ([stats (fasl-stats fasl)]
             [ref (lambda (key) (cdr (assq key stats)))]
             [lites (ref 'lites)]
             [rows (append
                     (map (lambda (s) (list (symbol->string (car s)) (cadr s) (caddr s)))
                       (ref 'sections))
                     (list (list "lites (outside the payload)" (cadr lites) (caddr lites))))]
             [rows (list-sort (lambda (a b) (> (caddr a) (caddr b))) rows)]
             [gzip-total (apply + (map caddr rows))]
             [debug-names '("spans" "srcmap" "metadata")]
             [debug-gzip (apply + (map caddr (filter (lambda (r) (member (car r) debug-names)) rows)))])
        (format #t ";; static-size: ~a bytes on disk, ~a uncompressed, ~a code blocks, ~a symbols and strings~%"
          (ref 'file) (ref 'uncompressed) (ref 'blocks) (car lites))
        (format #t ";; static-size: payload ~a bytes ~a~%" (ref 'stored-payload)
          (if (ref 'compressed) "(gzip)" "(stored raw)"))
        (format #t ";;   ~a ~a ~a ~a~%" (pad-right "section" 28) (pad-left "raw" 9) (pad-left "gzip alone" 11) (pad-left "% of gzip" 10))
        (for-each
          (lambda (r)
            (format #t ";;   ~a ~a ~a ~a~%" (pad-right (car r) 28) (pad-left (number->string (cadr r)) 9)
              (pad-left (number->string (caddr r)) 11) (pad-left (percent (caddr r) gzip-total) 10)))
          rows)
        (format #t ";; static-size: debug info (spans, srcmap, metadata) is ~a of ~a bytes compressed alone (~a%). Use --strip-spans to drop the spans and the srcmap~%"
          debug-gzip gzip-total (percent debug-gzip gzip-total))))

    (when report? (print-report) (flush-output-port (current-output-port)))
    (for-each print-why why-names)
    (flush-output-port (current-output-port))

    (when dry-run? (exit 0))

    ;;; The linklet

    (define body ((vector-ref chosen 4)))
    ((vector-ref chosen 8))

    ;; Only the primitives the bundle still refers to are imported. A primitive the bundle
    ;; assigns (a boot linklet's `(define (assertion-violation ...) ...)`) is defined by it.
    (define capy-used (make-eq-hashtable))
    (define capy-assigned (make-eq-hashtable))
    (walk body
      (lambda (x)
        (cond
          [(and (global-ref? x) (equal? (global-ref-key x) capy-key))
            (hashtable-set! capy-used (global-ref-name x) #t)]
          [(and (global-set? x) (equal? (global-set-key x) capy-key))
            (hashtable-set! capy-used (global-set-name x) #t)
            (hashtable-set! capy-assigned (global-set-name x) #t)])))
    (define used-capy (filter (lambda (tg) (hashtable-ref capy-used (cadr tg) #f)) capy-slot-order))

    (let ([native (make-eq-hashtable)])
      (for-each (lambda (s) (hashtable-set! native s #t)) (native-primitive-names))
      (let ([bad (filter (lambda (tg)
                           (not (or (hashtable-ref native (cadr tg) #f)
                                  (hashtable-ref interesting (cadr tg) #f)
                                  (hashtable-ref capy-assigned (cadr tg) #f))))
                   used-capy)])
        (unless (null? bad)
          (error 'static-link "the bundle refers to primitives that are not native and that no boot linklet defines"
            (map cadr bad)))))
    ;; The native extensions the program loads by key: the key is a constant of the code that
    ;; loads it (`(load-native-extension '(|#%native| <package-id> <name>))`). An executable gets
    ;; the shared libraries beside it, in lib/<package-id>/<name>.<dll>, where the key is looked up
    ;; when it has no libmap (`load-native-extension` in boot/linker.scm).
    (define native-entries
      (let ([found '()])
        (walk body
          (lambda (x)
            (when (and (constant? x) (pair? (constant-value x)))
              (let ([entry (linker-native-entry (constant-value x))])
                (when (and entry (not (member entry found)))
                  (set! found (cons entry found)))))))
        (reverse found)))

    (define linklet
      (let* ([capy-syms (map cadr used-capy)]
             [capy-slots (map caddr used-capy)])
        (make-linklet-term #f static-key
          (list (cons capy-key capy-syms))
          '()
          capy-slots
          body
          (list '|#%boot| static-key))))

    ;; --static-dump-tree-il: the merged body as Scheme, next to OUT.
    (when dump-tree-il?
      (call-with-output-file (string-append out ".tree-il.scm")
        (lambda (port)
          (pretty-print (tree-il->scheme body) port)
          (newline port))))

    ;;; Compile and write.
    (define tmp (string-append out ".part.fasl"))
    (define (remove-part!)
      (when (file-exists? tmp) (delete-file tmp)))
    (define t-compiled #f)
    (let ([size
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (let* ([code linklet]
                       [code (resolve-primitives code #f)]
                       [code (expand-primitives code)]
                       [code (fix-letrec code)]
                       [code (expand-well-known-procs code)]
                       [code (eliminate-assignments code)])
                  (when dump-tree-il?
                    (call-with-output-file (string-append out ".tree-il.opt.scm")
                      (lambda (port)
                        (write (tree-il->scheme code) port)
                        (newline port))))
                  (compile-il-to-file code tmp))
                (set! t-compiled (current-jiffy))
                (when size?
                  (print-size-report tmp)
                  (flush-output-port (current-output-port)))
                (if exe?
                  (if static-native?
                    (write-native-executable! out tmp native-entries)
                    (let ([size (write-executable! out tmp)])
                      (copy-natives! out native-entries)
                      size))
                  (let ([bytes (call-with-port (open-file-input-port tmp) get-bytevector-all)])
                    (call-with-port (open-file-output-port out (file-options no-fail))
                      (lambda (port)
                        (when shebang?
                          (put-bytevector port (string->utf8 "#!/usr/bin/env capy\n")))
                        (put-bytevector port bytes)))
                    (+ (bytevector-length bytes) (if shebang? 20 0)))))
              remove-part!)])
      (when (or report? verbose?)
        (format (if report? (current-output-port) (current-error-port))
          ";; wrote ~a (~a bytes); link time ~as: collect and expand ~a, shake ~a, compile ~a~%"
          out size
          (seconds t-start t-compiled) (seconds t-start t-collected)
          (seconds t-collected t-planned) (seconds t-planned t-compiled))
        (flush-output-port (current-output-port)))
      (unless exe?
        (system (string-append "chmod +x '" out "'")))
      (if #f #f))))
