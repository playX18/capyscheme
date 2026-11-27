;;; Building a package
(library (capy pkg build)
  (export make-build-settings
    build-settings?
    build!
    build-result?
    build-result-root
    build-result-roots
    build-result-libmap
    build-result-out
    build-result-programs
    build-result-directory
    result-program-name
    result-program-static?
    result-program-path
    result-program-package
    profile-directory)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) file-exists? create-directory* file-stat-mtime microsecond %compiler-id current-directory
      canonicalize-path-string)
    (capy libmap)
    (capy path)
    (capy process)
    (capy pkg graph)
    (capy pkg manifest)
    (capy pkg native)
    (only (capy pkg resolve) resolve-dependencies)
    (capy pkg toolchain)
    (capy pkg diag)
    (capy pkg ui)
    (only (capy term paint) paint))


  (define-record-type build-settings
    (fields profile jobs static? verbose? ui toolchain stage options static-native?)
    (protocol
      (lambda (new)
        (case-lambda
          [(profile jobs static? verbose? ui toolchain stage options)
            (new profile jobs static? verbose? ui toolchain stage options #f)]
          [(profile jobs static? verbose? ui toolchain stage options static-native?)
            (new profile jobs static? verbose? ui toolchain stage options static-native?)]))))

  ;; ROOTS are the packages that were asked for (the members selected in a workspace), ROOT the
  ;; last of them; OUT is the out dir of ROOT.
  (define-record-type build-result
    (fields roots root libmap programs directory out))

  ;; A program of the result is (NAME STATIC? PATH PACKAGE).
  (define (result-program-name p) (car p))
  (define (result-program-static? p) (cadr p))
  (define (result-program-path p) (caddr p))
  (define (result-program-package p) (cadddr p))

  (define (profile-directory root profile) (path-join root (path-join "target" profile)))

  (define (package-output-directory directory name)
    (path-join directory (path-join "build" (path-join (symbol->string name) "out"))))

  (define-record-type node
    (fields label title package (mutable deps) (mutable state) prepare finish order))

  (define-record-type report
    (fields failed? diagnostics residual))

  (define (seconds-text microseconds)
    (let* ([tenths (div (+ microseconds 5000) 10000)]
           [whole (div tenths 100)]
           [fraction (mod tenths 100)])
      (string-append (number->string whole) "." (if (< fraction 10) "0" "") (number->string fraction) "s")))

  (define (package-text pkg)
    (let ([manifest (package-manifest pkg)])
      (string-append (symbol->string (manifest-name manifest)) " v" (manifest-version manifest)
        " (" (if (eq? (package-name pkg) (manifest-name manifest))
              (package-directory pkg)
              (symbol->string (package-name pkg)))
        ")")))

  (define (mtime path) (if (file-exists? path) (file-stat-mtime path) -1))

  ;; Whether OUT may predate SOURCE. Modification times are whole seconds, so a source saved in
  ;; the second its output was written may be the newer of the two: that counts as stale.
  (define (stale-against? out source) (<= (mtime out) (mtime source)))

  (define (same-set? a b)
    (and (= (length a) (length b))
      (for-all (lambda (x) (member x b)) a)))

  ;; Everything the build of a package needs, and does.
  (define build!
    (case-lambda
      [(root-directory settings) (build-selected! root-directory (list root-directory) settings)]
      [(root-directory selected settings) (build-selected! root-directory selected settings)]))

  (define (build-selected! root-directory selected settings)
    (let* ([ui (build-settings-ui settings)]
           [toolchain (build-settings-toolchain settings)]
           [stage (build-settings-stage settings)]
           [started (microsecond)]
           [packages (if stage
                      (resolve-package-tree root-directory)
                      (resolve-packages selected
                        (resolve-dependencies root-directory (build-settings-options settings) ui)))]
           [roots (if stage
                    (list (car (reverse packages)))
                    (let ([wanted (map canonicalize-path-string selected)])
                      (filter (lambda (pkg) (member (canonicalize-path-string (package-directory pkg)) wanted))
                        packages)))]
           [root (car (reverse roots))])
      (make-views! packages toolchain)
      (for-each
        (lambda (pkg)
          (package-out-directory-set! pkg
            (if stage
              (path-join stage (path-join (symbol->string (package-name pkg)) "out"))
              (package-output-directory
                (profile-directory root-directory (build-settings-profile settings))
                (package-name pkg)))))
        packages)
      (scan-packages! packages)
      (let* ([units (library-graph packages toolchain)]
             ;; (package . program) of the programs to build: the root's, or all of the toolchain's
             [programs (if stage
                        (apply append
                          (map (lambda (pkg)
                                (map (lambda (program) (cons pkg program))
                                  (manifest-programs (package-manifest pkg))))
                            packages))
                        (apply append
                          (map (lambda (pkg)
                                 (map (lambda (program) (cons pkg program))
                                   (manifest-programs (package-manifest pkg))))
                            roots)))]
             [static-programs (if (build-settings-static? settings)
                               programs
                               (filter (lambda (p) (program-static? (cdr p))) programs))]
             [_ (check-executable-names! static-programs)]
             ;; the toolchain's libraries keep their tree-IL, for the programs that are linked
             [wpo? (or (and stage #t) (pair? static-programs))]
             ;; the programs that have their native extensions linked into them
             [static-native? (exists (lambda (p) (program-static-native?* settings (cdr p))) static-programs)]
             ;; `capy check` also lints: it warns about calls with the wrong number of arguments
             [lint? (and (not stage) (equal? (build-settings-profile settings) "check"))]
             [directory (or stage (profile-directory root-directory (build-settings-profile settings)))]
             [libmap-path (path-join directory "capy.libmap")]
             [runtime (toolchain-runtime toolchain)]
             [compiler-id (if stage
                           (%compiler-id)
                           (libmap-compiler-id (toolchain-libmap toolchain)))]
             [toolchain-time (max (if stage -1 (mtime (toolchain-libmap-path toolchain))) (mtime runtime))]
             ;; the native extensions of the packages, built before anything is compiled: a library
             ;; can load one while it is expanded (c-include does). (key file identity) lists; the
             ;; toolchain's own are in the libmap of the toolchain.
             [natives (build-package-natives! packages directory stage runtime compiler-id
                        (build-settings-profile settings) root-directory ui (and static-native? (not stage)))]
             [toolchain-natives (toolchain-native-entries toolchain)]
             ;; key -> (key source fasl index interface-hash wpo) of the libraries of the graph
             [entries (make-hashtable equal-hash equal?)]
             [changed (make-eq-hashtable)]    ; unit -> #t: its interface changed
             [rebuilt (make-eq-hashtable)]    ; unit -> #t: it was compiled in this build
             [announced '()]
             [dirty #t]
             [all-units '()]
             [running '()]
             [results '()])

        ;; node -> report of the compiler process of the node
        (define reports (make-eq-hashtable))
        (define node-count 0)
        (define (next-order!)
          (set! node-count (+ node-count 1))
          node-count)

        ;; What the compiler is told for the diagnostics: to write them as JSON for the driver
        ;; (the toolchain build prints them as they are), and to lint.
        (define (diagnostic-flags)
          (if stage
            '()
            (append (list "--error-format" "json") (if lint? (list "--lint") '()))))

        ;; The diagnostics of a file that is not compiled again are the ones it had when it was,
        ;; kept next to it
        (define (replay! node path)
          (when lint?
            (let ([ds (read-diagnostic-file path)])
              (when (pair? ds)
                (hashtable-set! reports node (make-report #f ds ""))))))

        (define (keep! node path)
          (when lint?
            (let ([report (hashtable-ref reports node #f)])
              (write-diagnostic-file! path (if report (report-diagnostics report) '())))))

        ;; The identity an extension has now: one the package graph builds, else one of the toolchain.
        (define (native-identity key)
          (cond
            [(assoc key natives) => caddr]
            [(assoc key toolchain-natives) => caddr]
            [else #f]))

        ;; The native section of the libmap, `(key path identity)`: the paths of a toolchain build are
        ;; relative to its directory, like the others.
        (define (native-libmap-entries)
          (append toolchain-natives
            (map (lambda (entry)
                  (list (car entry) (if stage (path-relative (cadr entry) directory) (cadr entry)) (caddr entry)))
              natives)))

        (define (compiling! pkg verb name reason)
          (unless (memq pkg announced)
            (set! announced (cons pkg announced)))
          (let ([painter (ui-painter ui)])
            (ui-status! ui verb
              (string-append (symbol->string (manifest-name (package-manifest pkg)))
                (paint painter '(dim) " > ")
                (paint painter '(cyan) name)
                (if (build-settings-verbose? settings)
                  (paint painter '(dim) (string-append " · " reason))
                  "")))))

        (define (current-hash key)
          (let ([own (hashtable-ref entries key #f)])
            (cond
              [own (list-ref own 4)]
              [(toolchain-entry toolchain key) => (lambda (entry) (list-ref entry 4))]
              [else #f])))

        (define (write-libmap!)
          (when dirty
            (create-directory* directory)
            (let-values ([(keys values) (hashtable-entries entries)])
              (write-libmap-entries! libmap-path (vector->list values)))
            (set! dirty #f)))

        (define (write-libmap-entries! path list)
          (write-libmap-file! path
            (make-libmap
              (append (toolchain-entries toolchain)
                ;; the libmap of a toolchain is relative to its directory, so that the
                ;; toolchain can be moved with the tree it was built from
                (if stage (map libmap-relative-entry list) list))
              (append (if stage '() (map cdr (libmap-views (toolchain-libmap toolchain))))
                (map package-view packages))
              (native-libmap-entries))))

        ;; The path of a source file as the compiler is told it. The compiled files record it, so
        ;; in a build of the toolchain it is relative to the directory the build runs in
        ;; (`pkgs/core/src/boot/base.scm`), as the runtime names the files it loads from there:
        ;; the compiled files do not depend on where the tree is.
        (define (source-argument path)
          (if stage (path-relative path (current-directory)) path))

        (define (libmap-relative-entry entry)
          (define (relative path) (and path (path-relative path directory)))
          (list (list-ref entry 0) (relative (list-ref entry 1)) (relative (list-ref entry 2))
            (list-ref entry 3) (list-ref entry 4) (relative (list-ref entry 5))))

        ;; units

        ;; the boot files, and the libraries defined in them
        (define (boot-unit? unit) (not (memq (unit-kind unit) '(library source))))

        (define (unit-fasl unit)
          (path-join directory
            (path-join (symbol->string (package-name (unit-package unit)))
              (path-join "fasl" (string-append (unit-relative-path unit) ".fasl")))))

        (define (libraries-of fasl)
          (guard (e [#t #f]) (fasl-libraries fasl)))

        ;; The reason the unit has to be compiled, or #f.
        (define (unit-stale-reason unit fasl)
          (if (boot-unit? unit)
            (boot-stale-reason unit fasl)
            (library-stale-reason unit fasl)))

        ;; A boot file has no declaration to compare hashes with, so it is compiled again when it,
        ;; the compiler or a unit it needs is newer than its compiled file.
        (define (boot-stale-reason unit fasl)
          (cond
            [(not (file-exists? fasl)) "no compiled file"]
            [(not (libraries-of fasl)) "the compiled file cannot be read"]
            [(stale-against? fasl (unit-path unit)) "the source changed"]
            [(< (mtime fasl) toolchain-time) "another compiler made the compiled file"]
            [(exists (lambda (dep) (hashtable-ref rebuilt dep #f)) (unit-dependencies unit))
              "a unit it needs was compiled again"]
            [else #f]))

        (define (library-stale-reason unit fasl)
          (let ([libs (and (file-exists? fasl) (libraries-of fasl))])
            (cond
              [(not (file-exists? fasl)) "no compiled file"]
              [(not libs) "the compiled file cannot be read"]
              [(not (same-set? (map car libs) (unit-keys unit))) "its libraries changed"]
              [(exists (lambda (lib) (not (caddr lib))) libs) "the compiled file has no declaration"]
              [(exists (lambda (lib) (not (equal? (decl-compiler-id (caddr lib)) compiler-id))) libs)
                "another compiler made the compiled file"]
              [(let ([hash (file-hash (unit-path unit))])
                 (exists (lambda (lib) (not (equal? (decl-source-hash (caddr lib)) hash))) libs))
                "the source changed"]
              [(exists (lambda (lib) (not (decl-compile-deps-current? (decl-compile-deps (caddr lib)) (unit-path unit) native-identity))) libs)
                "a file it read, or a native extension it loaded, changed"]
              [(exists
                (lambda (lib)
                  (exists
                    (lambda (dep)
                      (and (not (member (car dep) (unit-keys unit)))
                        (cdr dep)
                        (let ([hash (current-hash (car dep))])
                          (and hash (not (equal? hash (cdr dep)))))))
                    (decl-deps (caddr lib))))
                  libs)
                "a library it imports changed"]
              [(and wpo? (exists (lambda (lib) (not (cadddr lib))) libs)) "no wpo file"]
              [else #f])))

        (define (register! unit fasl)
          (let ([libs (or (libraries-of fasl)
                        (fail "the compiled file cannot be read:" fasl))])
            (unless (or (boot-unit? unit) (same-set? (map car libs) (unit-keys unit)))
              (fail (string-append (unit-path unit) " does not define the libraries it is expected to define")))
            (for-each
              (lambda (lib)
                (hashtable-set! entry-units (car lib) unit)
                (hashtable-set! entries (car lib)
                  (list (car lib) (unit-path unit) fasl (cadr lib)
                    (and (caddr lib) (decl-interface-hash (caddr lib))) (cadddr lib))))
              libs)
            (set! dirty #t)))

;; What the libmap lists before anything is compiled, in a build of the toolchain: every
        ;; library with its source and no compiled file. A library the compiler needs that no
        ;; library imports (the ones every library is expanded with) is found like that, and
        ;; compiled when it is needed.
        (define (list-sources!)
          (for-each
            (lambda (unit)
              (for-each
                (lambda (key)
                  (hashtable-set! entries key (list key (unit-path unit) #f #f #f #f)))
                (if (and (unit-boot-name unit) (null? (unit-keys unit)) (boot-unit? unit))
                  (list (list 'boot (unit-boot-name unit)))
                  (unit-keys unit))))
            all-units)
          (set! dirty #t))

        (define (unit-hashes unit fasl)
          (let ([libs (and (file-exists? fasl) (libraries-of fasl))])
            (if libs
              (map (lambda (lib) (cons (car lib) (and (caddr lib) (decl-interface-hash (caddr lib))))) libs)
              '())))

        (define unit-nodes (make-eq-hashtable))
        ;; The edges of the nodes of UNITS, once every node exists: an ordering edge can point at a
        ;; unit that comes later in the list.
        (define (link-unit-nodes! units)
          (for-each
            (lambda (unit)
              (node-deps-set! (nodes-of unit)
                (map nodes-of
                  (append (if (memq unit core-units) '() (unit-dependencies unit))
                    (ordering-dependencies unit)))))
            units))
        (define entry-units (make-hashtable equal-hash equal?)) ; library key -> the unit that defines it
        (define (nodes-of unit) (hashtable-ref unit-nodes unit #f))
        (define live-nodes '())     ; the nodes the scheduler runs: more are added while it runs

        ;; The libraries that compiling something generated into an output directory since the
        ;; last look become units with nodes the scheduler runs in this build. A program that
        ;; uses `c-include` generates them too, and runs with them.
        (define (adopt-generated!)
          (let ([fresh (scan-generated! packages)])
            (unless (null? fresh)
              (set! all-units (library-graph packages toolchain))
              (for-each
                (lambda (u) (hashtable-set! unit-nodes u (unit-node u nodes-of)))
                fresh)
              (link-unit-nodes! fresh)
              (set! live-nodes (append live-nodes (map nodes-of fresh))))))

        ;; A unit has a compiled file: lists its libraries in the libmap, and finds what compiling
        ;; it generated. The code of a package can generate libraries into its output directory
        ;; (`c-include`), which the compiled file imports without the source of the unit saying so:
        ;; they are units of the package that are compiled too, and what imports the unit waits
        ;; for them.
        (define (finished! unit fasl)
          (register! unit fasl)
          (adopt-generated!)
          (let ([needed
                  (filter
                    (lambda (u) (and (unit-built? u) (not (or (eq? u unit) (memq u (unit-dependencies unit))))))
                    (let ([libs (or (libraries-of fasl) '())])
                      (filter values
                        (map
                          (lambda (key)
                            (find (lambda (u) (member key (unit-keys u))) all-units))
                          (apply append
                            (map (lambda (lib) (if (caddr lib) (map car (decl-deps (caddr lib))) '()))
                              libs))))))])
            (unless (null? needed)
              (for-each
                (lambda (node)
                  (when (and (eq? (node-state node) 'pending) (memq (nodes-of unit) (node-deps node)))
                    (node-deps-set! node
                      (append (node-deps node)
                        (filter (lambda (n) (not (memq n (node-deps node)))) (map nodes-of needed))))))
                live-nodes))))

        (define core-units
          (if stage
            (filter (lambda (unit) (and (unit-built? unit) (manifest-boot-package? (package-manifest (unit-package unit)))))
              units)
            '()))

        (define (ordering-dependencies unit)
          (if (or (null? core-units) (memq unit core-units))
            '()
            core-units))

        (define (unit-node unit nodes-of)
          (let ([fasl (unit-fasl unit)]
                [pkg (unit-package unit)]
                [before '()])
            (make-node
              (string-append (string-join-names (unit-names unit)) " in " (symbol->string (package-name pkg)))
              (string-join-names (unit-names unit))
              pkg
              '()                       ; see `link-unit-nodes!`
              'pending
              (lambda (node)
                (let ([reason (unit-stale-reason unit fasl)])
                  (cond
                    [reason
                      (set! before (unit-hashes unit fasl))
                      (compiling! pkg "Compiling" (string-join-names (unit-names unit)) reason)
                      (create-directory* (path-directory fasl))
                      (cond
                        [(eq? (unit-kind unit) 'expander)
                          ;; the boot files: their `.wpo` files spare the static linker
                          ;; expanding them again (see `boot-wpo-linklets` in boot/eval.scm)
                          (append (list "--compile")
                            (if wpo? (list "--wpo") '())
                            (list "-o" fasl (source-argument (unit-path unit))))]
                        [(memq unit core-units)
                          (append
                            (list "--compile"
                              "--compile-output-directory" (package-out-directory pkg))
                            (diagnostic-flags)
                            (if wpo? (list "--wpo") '())
                            (list "-o" fasl (source-argument (unit-path unit))))]
                        [else
                          (append
                            (list "--compile" "--libmap" libmap-path
                              "--view" (symbol->string (package-name pkg))
                              "--compile-output-directory" (package-out-directory pkg))
                            (diagnostic-flags)
                            (if wpo? (list "--wpo") '())
                            (list "-o" fasl (source-argument (unit-path unit))))])]
                    [else
                      (finished! unit fasl)
                      (replay! node (string-append fasl ".diag"))
                      #f])))
              (lambda (node process)
                (finished! unit fasl)
                (hashtable-set! rebuilt unit #t)
                (keep! node (string-append fasl ".diag"))
                (let ([after (unit-hashes unit fasl)])
                  (unless (and (same-set? before after) (not (null? before)))
                    (hashtable-set! changed unit #t))))
              (next-order!))))

        ;; programs

        (define (program-node entry nodes-of)
          (let* ([pkg (car entry)]
                 [program (cdr entry)]
                 [source (path-join (package-directory pkg) (program-file program))]
                 [static? (or (build-settings-static? settings) (program-static? program))]
                 [native? (and static? (program-static-native?* settings program))]
                 ;; tells a build with the extensions linked in from one without them
                 [mode-file (string-append (path-join (if stage directory (path-join directory "bin")) (symbol->string (program-name program))) ".static-native")]
                 [out (if static?
                       (path-join (if stage directory (path-join directory "bin")) (symbol->string (program-name program)))
                       (path-join (path-join directory (symbol->string (package-name pkg)))
                         (path-join "programs" (string-append (symbol->string (program-name program)) ".fasl"))))]
                 [all-and-direct
                   (begin
                     (unless (file-exists? source)
                       (fail (string-append "the program " (symbol->string (program-name program))
                              " of the package " (symbol->string (package-name pkg)) " has no source file:") source))
                     (call-with-values (lambda () (program-dependencies pkg source all-units toolchain)) cons))]
                 [deps (car all-and-direct)]
                 ;; A static executable has every library in it. A program that loads them
                 ;; depends on the interface of the ones it imports.
                 [relevant (if static? deps (cdr all-and-direct))])
            (set! results (cons (list (program-name program) static? out pkg) results))
            (make-node
              (string-append "the program " (symbol->string (program-name program)))
              (symbol->string (program-name program))
              pkg
              (map nodes-of deps)
              'pending
              (lambda (node)
                (let ([reason (cond
                               [(not (file-exists? out)) "no compiled program"]
                               [(stale-against? out source) "the source changed"]
                               [(< (mtime out) toolchain-time) "another compiler made it"]
                               [(not (eq? native? (file-exists? mode-file))) "the native extensions are linked in another way"]
                               [(and static? (exists (lambda (unit) (< (mtime out) (mtime (unit-fasl unit)))) deps))
                                 "a library in it was compiled again"]
                               [(exists (lambda (unit) (hashtable-ref (if static? rebuilt changed) unit #f)) relevant)
                                 "a library it imports changed"]
                               [else #f])])
                  (cond
                    [reason
                      (compiling! pkg (if static? "Linking" "Compiling") (symbol->string (program-name program)) reason)
                      (create-directory* (path-directory out))
                      (append
                        (list "--compile" "--libmap" libmap-path "--view" (symbol->string (package-name pkg))
                          "--compile-output-directory" (package-out-directory pkg))
                        (diagnostic-flags)
                        (if static? (list "--static" "--exe") '())
                        (if native? (list "--static-native") '())
                        (list "-o" out (source-argument source)))]
                    [else
                      (replay! node (string-append out ".diag"))
                      #f])))
              (lambda (node process)
                (keep! node (string-append out ".diag"))
                (adopt-generated!)
                (cond
                  [(and native? (not (file-exists? mode-file)))
                    (call-with-output-file mode-file (lambda (port) (display "static-native\n" port)))]
                  [(and (not native?) (file-exists? mode-file)) (delete-file mode-file)]))
              (next-order!))))

        ;; What the compilers said, in the order of the nodes: the diagnostics, then a summary. A
        ;; build with errors ends here.
        (define (report-build!)
          (let* ([nodes (list-sort (lambda (a b) (< (node-order a) (node-order b)))
                         (vector->list (hashtable-keys reports)))]
                 [errors 0]
                 [warnings 0])
            (for-each
              (lambda (node)
                (let* ([report (hashtable-ref reports node #f)]
                       [ds (report-diagnostics report)]
                       [error-count (count-severity ds 'error)])
                  (report-diagnostics! ui ds)
                  (set! warnings (+ warnings (count-severity ds 'warning)))
                  (set! errors (+ errors error-count))
                  (when (report-failed? report)
                    ;; a failure with no diagnostic of its own (the compiler could not write
                    ;; them, or died) is an error that shows what the compiler printed
                    (when (= error-count 0)
                      (set! errors (+ errors 1))
                      (let ([text (report-residual report)])
                        (put-string (ui-port ui)
                          (ui-error-text ui
                            (string-append "could not compile " (node-label node)
                              (if (string=? text "") "" (string-append "\n" text))))))))))
              nodes)
            (report-summary! ui
              (if (null? (cdr roots))
                (symbol->string (package-name root))
                (string-append "the workspace " (path-file-name root-directory)))
              errors warnings)
            (when (> errors 0)
              (fail-reported))))

        ;; scheduling

        (define (run-nodes!)
          (let ([pool (make-process-pool (build-settings-jobs settings))])
            (define (show-progress!)
              (ui-progress! ui
                (length (filter (lambda (node) (memq (node-state node) '(done failed))) live-nodes))
                (length live-nodes)
                (map (lambda (pair) (node-title (car pair))) (reverse running))))
            (define (ready? node)
              (and (eq? (node-state node) 'pending)
                (for-all (lambda (dep) (eq? (node-state dep) 'done)) (node-deps node))))
            (let loop ()
              (let ([node (and (not (process-pool-full? pool)) (find ready? live-nodes))])
                (cond
                  [node
                    (let ([args ((node-prepare node) node)])
                      (cond
                        [args
                          (write-libmap!)
                          (node-state-set! node 'running)
                          (process-pool-start! pool node
                            (lambda ()
                              (let ([p (spawn runtime args #:stdout 'string #:stderr 'stdout)])
                                (set! running (cons (cons node p) running))
                                p)))
                          (show-progress!)]
                        [else (node-state-set! node 'done)]))
                    (loop)]
                  [(> (process-pool-count pool) 0)
                    (let-values ([(node process) (process-pool-wait! pool)])
                      (set! running (filter (lambda (pair) (not (eq? (car pair) node))) running))
                      (let ([failed? (not (eqv? (process-exit-status process) 0))])
                        (let-values ([(ds residual) (parse-compiler-output (process-stdout-string process))])
                          (hashtable-set! reports node (make-report failed? ds residual)))
                        ;; the nodes that do not depend on this one go on: the build reports every
                        ;; error it can find. The ones that do stay pending
                        (cond
                          [failed? (node-state-set! node 'failed)]
                          [else
                            ((node-finish node) node process)
                            (node-state-set! node 'done)]))
                      (show-progress!)
                      (loop))]
                    [(and (not (let-values ([(keys reps) (hashtable-entries reports)])
                                 (exists report-failed? (vector->list reps))))
                        (exists (lambda (node) (not (eq? (node-state node) 'done))) live-nodes))
                      (begin
                        (ui-progress-done! ui)
                        (fail "internal error: the libraries could not be ordered"))]
                    [else (ui-progress-done! ui)])))))

        (for-each
          (lambda (unit) (hashtable-set! unit-nodes unit (unit-node unit nodes-of)))
          (filter unit-built? units))
        (link-unit-nodes! (filter unit-built? units))
        (set! all-units units)
        (when stage (list-sources!))
        (set! live-nodes (map nodes-of (filter unit-built? units)))
        (run-nodes!)
        (let* ([program-list (map (lambda (program) (program-node program nodes-of)) programs)])
          (set! live-nodes program-list)
          (run-nodes!)
            (write-libmap!)
            (report-build!)
            (when (build-settings-verbose? settings)
              (for-each
                (lambda (pkg)
                  (unless (memq pkg announced)
                    (ui-status! ui "Fresh" (package-text pkg))))
                packages))
            (ui-status! ui "Finished"
              (string-append "`" (build-settings-profile settings) "` profile in "
                (seconds-text (- (microsecond) started))))
            (make-build-result roots root libmap-path (reverse results) directory (package-out-directory root))))))

  ;; The executables of a build are target/<profile>/bin/NAME: two programs of one name, of two
  ;; members, would be the same file.
  (define (check-executable-names! programs)
    (let loop ([ps programs] [seen '()])
      (when (pair? ps)
        (let* ([name (program-name (cdr (car ps)))]
               [other (assq name seen)])
          (when other
            (fail (string-append "the packages " (symbol->string (package-name (cdr other))) " and "
                   (symbol->string (package-name (car (car ps)))) " both have a program "
                   (symbol->string name) ", and their executables would be the same file; build one of them with -p")))
          (loop (cdr ps) (cons (cons name (car (car ps))) seen))))))

  ;; Builds the native extensions of PACKAGES (see `(capy pkg native)`). A package build puts them in
  ;; DIRECTORY/native/<package>, the build of the toolchain in STAGE/<package>/native, from where an
  ;; install copies them. The libraries of the packages of the toolchain have the ids `core` and `std`
  ;; in their keys, and so have its extensions.
  ;; Whether PROGRAM, or the whole build, has the native extensions linked into the executable.
  (define (program-static-native?* settings program)
    (or (build-settings-static-native? settings) (program-static-native? program)))

  (define (build-package-natives! packages directory stage runtime compiler-id profile root-directory ui static?)
    (let ([with-extensions
            (filter (lambda (pkg) (pair? (manifest-native (package-manifest pkg)))) packages)]
          [include-directories
            (filter (lambda (dir) (file-exists? (path-join dir "capy.h")))
              (list (path-join (path-directory runtime) "include")
                (path-join (path-directory runtime) "../include")
                (path-join (path-directory runtime) "../c")
                (path-join root-directory "../c")))])
      (build-natives! with-extensions
        (lambda (pkg) (manifest-native (package-manifest pkg)))
        package-directory
        (lambda (pkg) (if stage (manifest-stage-key (package-manifest pkg)) (package-name pkg)))
        (lambda (pkg)
          (if stage
            (path-join stage (path-join (symbol->string (package-name pkg)) "native"))
            (path-join directory (path-join "native" (symbol->string (package-name pkg))))))
        (lambda (pkg)
          (if stage
            (path-join stage (path-join (symbol->string (package-name pkg)) "build"))
            (path-join directory (path-join "build" (path-join (symbol->string (package-name pkg)) "native")))))
        package-text
        (if stage "release" profile)
        runtime compiler-id ui include-directories static?)))

  (define (string-join-names names)
    (let loop ([names names] [out ""])
      (if (null? names)
        out
        (loop (cdr names) (string-append out (if (string=? out "") "" ", ") (library-name-text (car names))))))))
