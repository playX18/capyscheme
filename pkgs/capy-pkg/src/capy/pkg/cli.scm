;;; The command line of `capy`.
;;;
;;;   capy new NAME [--lib]       a new package in the directory NAME
;;;   capy init [--lib]           a package in the current directory
;;;   capy build [--release] [--static] [-j N]
;;;   capy build --toolchain DIR [--packages DIR] [-j N]
;;;                               builds core, std, the extras and the driver itself into DIR
;;;                               (the stages of ./bootstrap.sh)
;;;   capy run [PROGRAM] [-- ARGUMENT ...]
;;;   capy test [FILTER ...] [-p NAME | --workspace] [--list] [--no-fail-fast] [--test-threads N]
;;;                               runs the test files of tests/ (see "Tests" below)
;;;   capy install [--path DIR | URL [--version V | --rev R]] [--prefix P] [--force] [--list]
;;;                               builds the package for release and installs its executables in
;;;                               P/bin (P: $CAPY_HOME, or ~/.capy), recording the files
;;;   capy install --toolchain DIR --prefix P
;;;                               installs the toolchain of the stage DIR (./bootstrap.sh)
;;;   capy uninstall NAME [--prefix P]
;;;   capy dist [--format portable|deb|rpm] [--toolchain DIR] [--out DIR] [--name FILE] [--version V]
;;;                               a .tar.gz, deb or rpm of the package's executables, or of the toolchain
;;;   capy check                  compiles into target/check, for the errors and the lints
;;;   capy clean                  removes target/
;;;   capy add URL [--name N] [--version V | --rev R]
;;;   capy add --path DIR [--name N]
;;;                               adds a dependency to package.scm (the latest version tag of URL
;;;                               unless --version or --rev says otherwise) and locks it
;;;   capy remove NAME            removes a dependency
;;;   capy update [NAME ...]      moves the git dependencies to their latest version tags
;;;   capy fetch                  resolves the git dependencies, fetches them, writes package.lock
;;;   capy tree                   the packages the package depends on, as a tree
;;;   capy repl                   the REPL with the libraries of the package
;;;   capy [--offline] [--locked] FILE.scm [ARGUMENT ...]
;;;                               runs a script; the dependencies in its `#|capy ... |#` block are
;;;                               fetched and built first (see `(capy pkg script)`)
;;;   capy help [COMMAND] | capy --help | capy COMMAND --help | capy --version
;;;
;;; In a workspace (see `(capy pkg workspace)`) the commands work on the member they are run in,
;;; or on every member at the root of a virtual workspace; `-p NAME` and `--workspace` say otherwise.


(library (capy pkg cli)
  (export run-cli)
  (import (rnrs)
    (only (capy) current-directory file-exists? file-directory? directory-list delete-directory*
      create-directory* implementation-version getenv home-directory executable-path microsecond
      canonicalize-path-string)
    (only (core thread) go)
    (only (capy channel) make-channel channel-send channel-recv channel-recv/ok channel-close)
    (only (capy timer) call-with-timeout timeout-expired?)
    (capy args)
    (only (capy args grammar) grammar-allow-anything?-set!)
    (only (capy args runner) make-command-runner command-runner-add-command! command-runner-run
      command command-results)
    (capy path)
    (capy process)
    (capy libmap)
    (capy pkg build)
    (capy pkg dist)
    (only (capy pkg git) git-repository-id git-expand-revision git-fetch-commit! capy-cache-directory)
    (capy pkg graph)
    (capy pkg manifest)
    (capy pkg edit)
    (capy pkg new)
    (capy pkg resolve)
    (capy pkg script)
    (capy pkg toolchain)
    (capy pkg workspace)
    (capy pkg ui)
    (capy pkg util))

  (define commands '("new" "init" "build" "run" "test" "check" "clean" "repl"
                     "install" "uninstall" "dist"
                     "add" "remove" "update" "fetch" "tree" "help"))

  ;; What `capy`, `capy --help` and `capy help` print above the list of the commands.
  (define description-text
    (string-append
      "capy: the package manager and runtime of CapyScheme\n"
      "\n"
      "Also: capy [--offline] [--locked] FILE.scm [ARGUMENT ...]\n"
      "      capy [RUNTIME OPTIONS] FILE.scm [ARGUMENT ...]\n"
      "\n"
      "A script (`capy FILE.scm`) that starts with a #|capy (dependencies ...) |# block has its\n"
      "dependencies fetched and built first; --offline and --locked work as for `capy fetch`.\n"
      "Anything else is run by the runtime: `capy FILE.scm`, `capy -c EXPR`, `capy -L DIR FILE.scm`;\n"
      "`capy-rt --help` lists the options of the runtime, `capy repl` starts the REPL."))

  ;;; Arguments

  (define (split-at-dashes args)
    (let loop ([args args] [before '()])
      (cond
        [(null? args) (values (reverse before) '())]
        [(string=? (car args) "--") (values (reverse before) (cdr args))]
        [else (loop (cdr args) (cons (car args) before))])))

  (define (color-mode args)
    (let loop ([args args])
      (cond
        [(null? args) 'auto]
        [(and (string=? (car args) "--color") (pair? (cdr args))) (parse-color-argument (cadr args))]
        [(and (> (string-length (car args)) 8) (string=? (substring (car args) 0 8) "--color="))
          (parse-color-argument (substring (car args) 8 (string-length (car args))))]
        [(string=? (car args) "--") 'auto]
        [else (loop (cdr args))])))

  (define (parse-color-argument text)
    (cond
      [(assoc text '(("auto" . auto) ("always" . always) ("never" . never))) => cdr]
      [else (fail (string-append "--color is auto, always or never, not `" text "`"))]))

  (define (common-parser build?)
    (let ([parser (argparser)])
      (add-flag! parser "verbose" (abbreviation "v") not-negatable (help "Say why a file is compiled"))
      (add-option! parser "color" (defaults-to "auto") (allowed '("auto" "always" "never"))
        (help "When to color the output"))
      (when build? (add-resolve-flags! parser) (add-selection-flags! parser))
      (when build?
        (add-flag! parser "release" not-negatable (help "Build into target/release"))
        (add-flag! parser "static" not-negatable (help "Link the programs into executables"))
        (add-flag! parser "static-native" not-negatable
          (help "With --static, link the native extensions into the executables (a program with (static native) does it by itself)"))
        (add-option! parser "message-format" (defaults-to "human") (allowed '("human" "json"))
          (help "How diagnostics are written: human (rendered) or json (one object per line on stdout)"))
        (add-option! parser "jobs" (abbreviation "j") (defaults-to "4")
          (help "The number of files compiled at once")))
      parser))

  ;; `capy build` and `capy check`: the options of every build, and the toolchain.
  (define (build-parser)
    (let ([parser (common-parser #t)])
      (add-option! parser "toolchain" (defaults-to "") (value-help "DIR")
        (help "Build core, std and the driver into DIR, instead of the package (the stages of ./bootstrap.sh)"))
      (add-option! parser "packages" (defaults-to "pkgs") (value-help "DIR")
        (help "With --toolchain, the directory of the packages"))
      parser))

  ;; -p NAME (any number of them) and --workspace: the members of the workspace to work on.
  (define (add-selection-flags! parser)
    (add-multi-option! parser "package" (abbreviation "p") (value-help "NAME")
      (help "Work on the member NAME of the workspace (may be given more than once)"))
    (add-flag! parser "workspace" not-negatable (help "Work on every member of the workspace")))

  (define (selected-names results) (or (arg-results-ref results "package") '()))
  (define (all-members? results) (arg-results-ref results "workspace"))

  (define (add-resolve-flags! parser)
    (add-flag! parser "offline" not-negatable (help "Use only package.lock and the cache of fetched packages"))
    (add-flag! parser "locked" not-negatable (help "Fail if package.lock would change")))

  (define (resolve-options-of results)
    (make-resolve-options (arg-results-ref results "offline") (arg-results-ref results "locked") #f))

  (define (jobs-of results)
    (let ([n (string->number (arg-results-ref results "jobs"))])
      (unless (and n (exact-integer? n) (> n 0))
        (fail "-j takes a positive number, not" (arg-results-ref results "jobs")))
      n))

  ;;; The package

  (define (project-here) (find-project (current-directory)))

  (define (settings-of ui toolchain results profile)
    (make-build-settings profile (jobs-of results)
      (or (arg-results-ref results "static") (arg-results-ref results "static-native"))
      (arg-results-ref results "verbose") ui toolchain #f (resolve-options-of results)
      (arg-results-ref results "static-native")))

  (define (profile-of results)
    (if (arg-results-ref results "release") "release" "debug"))

  ;; Builds the package of the current directory, or the members of the workspace that the
  ;; command line selects (SELECTED, when given, overrides it). Returns the result and the
  ;; toolchain.
  (define build-here
    (case-lambda
      [(ui results profile)
        (build-here ui results profile
          (select-members (project-here) (selected-names results) (all-members? results)))]
      [(ui results profile selected)
        (let* ([toolchain (locate-toolchain)]
               [project (project-here)])
          (when (string=? (arg-results-ref results "message-format") "json")
            (ui-message-format-set! ui 'json))
          (values (build! (project-root project) selected (settings-of ui toolchain results profile))
            toolchain))]))

  ;; The member of the project in DIR (the package the command was run in) that a command of one
  ;; package works on. WHAT names the command in the error.
  (define (package-here what) (single-member (project-here) '() #f what))

  ;; Builds the package in the directory DIR with SETTINGS, as a member of its workspace when it
  ;; is in one: target/ and package.lock are the ones of the root of the project.
  (define (build-package! dir settings)
    (let ([project (find-project dir)])
      (build! (project-root project) (list (single-member project '() #f "the build")) settings)))

  (define (root-name result)
    (symbol->string (package-name (build-result-root result))))

  ;; The arguments of the runtime for the libraries of the package PKG of the build (its root by
  ;; default).
  (define runtime-arguments
    (case-lambda
      [(result) (runtime-arguments result (build-result-root result))]
      [(result pkg)
        (list "--libmap" (build-result-libmap result) "--view" (symbol->string (package-name pkg))
          "--compile-output-directory" (package-out-directory pkg))]))

  (define (exec! program argv)
    (exec-process program argv)
    (fail "cannot run" program))

  ;;; Commands

  (define (new-parser)
    (let ([p (common-parser #f)]) (add-flag! p "lib" not-negatable (help "Make a library package, not a program")) p))

  (define (command-new ui results init?)
    (let* ([rest (arg-results-rest results)]
           [lib? (arg-results-ref results "lib")])
      (cond
        [init?
          (unless (null? rest) (fail "capy init takes no arguments"))
          (let* ([dir (current-directory)]
                 [name (path-file-name dir)])
            (create-package-in-workspace! ui dir name lib?))]
        [else
          (unless (and (pair? rest) (null? (cdr rest))) (fail "usage: capy new NAME [--lib]"))
          (let* ([name (path-file-name (car rest))]
                 [dir (car rest)])
            (when (file-exists? dir) (fail "the destination exists already:" dir))
            (create-package-in-workspace! ui (path-join (current-directory) dir) name lib?))])
      0))

  ;; DIR (absolute) with its part that exists canonical.
  (define (absolute-canonical dir)
    (cond
      [(file-exists? dir) (canonicalize-path-string dir)]
      [(string=? (path-directory dir) dir) dir]
      [else (path-join (absolute-canonical (path-directory dir)) (path-file-name dir))]))

  ;; The workspace above the directory DIR (which need not exist yet), or #f.
  (define (workspace-above dir)
    (let loop ([up (path-directory dir)])
      (cond
        [(read-workspace-file (path-join up "package.scm")) => values]
        [(string=? (path-directory up) up) #f]
        [else (loop (path-directory up))])))

  ;; Creates the package NAME in DIR (absolute). Below the root of a workspace, the package is
  ;; added to its members unless they cover it already or it is excluded; a member has no
  ;; .gitignore of its own, the target/ is at the root.
  (define (create-package-in-workspace! ui dir name lib?)
    (let* ([dir (absolute-canonical dir)]
           [ws (workspace-above dir)]
           [relative (and ws (path-relative dir (workspace-directory ws)))]
           [excluded? (and ws (member relative (workspace-exclude ws)))]
           [member? (and ws (not excluded?))])
      (create-package! dir name lib? (not member?))
      (ui-status! ui "Created" (string-append (if lib? "library" "binary") " package `" name "`"))
      (when (and member? (not (workspace-covers? ws dir)))
        (let* ([path (workspace-path ws)]
               [text (read-text path)])
          (write-text! path (add-member-text text relative))
          (ui-status! ui "Added" (string-append "`" relative "` to the members of the workspace"))))))

;; `capy build --toolchain STAGE [--packages pkgs]` builds the toolchain itself, core, std and
  ;; the packages that ship with them (the driver among them), from the directory of the tree
  ;; they are in, into STAGE. The directories are made absolute: the runtime loads files by
  ;; absolute paths, and the compiled files name the sources they were made from by them.
  (define (build-toolchain ui results)
    (let ([stage (path-join (current-directory) (arg-results-ref results "toolchain"))]
          [packages (path-join (current-directory) (arg-results-ref results "packages"))])
      (unless (read-workspace-file (path-join packages "package.scm"))
        (fail (string-append packages "/package.scm is not a workspace; the toolchain is built from the members of "
                "(workspace (members ...)) there")))
      (build! packages
        (make-build-settings "toolchain" (jobs-of results) #f (arg-results-ref results "verbose") ui
          (make-bootstrap-toolchain) stage #f))))

  (define (command-build ui results check?)
    (let ()
      (unless (null? (arg-results-rest results)) (fail "unexpected argument" (car (arg-results-rest results))))
      (if (string=? (arg-results-ref results "toolchain") "")
        (build-here ui results (if check? "check" (profile-of results)))
        (build-toolchain ui results))
      0))

  (define (command-clean ui results)
    (let* ([target (path-join (project-root (project-here)) "target")])
      (cond
        [(file-exists? target)
          (delete-directory* target)
          (ui-status! ui "Removed" target)]
        [else (ui-status! ui "Clean" "nothing to remove")])
      0))

  ;; `member/program` in a workspace, `program` otherwise.
  (define (program-label result p)
    (if (null? (cdr (build-result-roots result)))
      (symbol->string (result-program-name p))
      (string-append (symbol->string (package-name (result-program-package p))) "/"
        (symbol->string (result-program-name p)))))

  ;; The members to build for `capy run NAME` with no -p: the members that have a program NAME.
  (define (members-with-program project name)
    (let ([owners (filter (lambda (m)
                            (exists (lambda (program) (string=? (symbol->string (program-name program)) name))
                              (manifest-programs (cdr m))))
                    (project-member-manifests project))])
      (cond
        [(null? owners)
          (fail (string-append "the " (if (project-workspace project) "workspace" "package")
                  " has no program " name))]
        [(pair? (cdr owners))
          (fail (string-append "several members have a program " name " ("
                  (let loop ([l owners] [out ""])
                    (if (null? l) out
                      (loop (cdr l) (string-append out (if (string=? out "") "" ", ")
                                      (symbol->string (manifest-name (cdr (car l)))) "/" name))))
                  "); name the member with -p"))]
        [else (list (car (car owners)))])))

  (define (command-run ui results program-arguments)
    (let ()
      (let* ([rest (arg-results-rest results)]
             [project (project-here)]
             [selected (if (and (pair? rest) (null? (selected-names results)) (not (all-members? results)))
                         (members-with-program project (car rest))
                         (select-members project (selected-names results) (all-members? results)))])
        (unless (or (null? rest) (null? (cdr rest))) (fail "capy run takes one program name"))
        (let-values ([(result toolchain) (build-here ui results (profile-of results) selected)])
          (let* ([programs (build-result-programs result)]
                 [what (if (null? (cdr (build-result-roots result))) "package" "workspace")]
                 [program
                   (cond
                     [(null? programs) (fail (string-append "the " what " has no programs; list them in `programs` of package.scm"))]
                     [(null? rest)
                       (if (null? (cdr programs))
                         (car programs)
                         (fail (string-append "the " what " has several programs ("
                                (let loop ([l programs] [out ""])
                                  (if (null? l) out
                                    (loop (cdr l) (string-append out (if (string=? out "") "" ", ")
                                                    (program-label result (car l))))))
                                "); name one: capy run NAME")))]
                     [(find (lambda (p) (string=? (symbol->string (result-program-name p)) (car rest))) programs) => values]
                     [else (fail (string-append "the " what " has no program " (car rest)))])]
                 [path (result-program-path program)])
            (ui-status! ui "Running" (string-append "`" path "`"))
            (if (result-program-static? program)
              (exec! path (cons path program-arguments))
              (exec! (toolchain-runtime toolchain)
                (append (list "capy") (runtime-arguments result (result-program-package program))
                  ;; `--` ends the runtime's own options: everything after it is the program's,
                  ;; so `capy run -- --backend x` is not mistaken for a runtime flag
                  (list "-s" path "--") program-arguments))))))))

  ;; Outside a package, `capy repl` is the REPL of the runtime, with the standard libraries only.
  (define (command-repl ui results)
    (if (find-project-or-false (current-directory))
      (let-values ([(result toolchain)
                    (build-here ui results (profile-of results)
                      (list (single-member (project-here) (selected-names results) (all-members? results) "capy repl")))])
        (exec! (toolchain-runtime toolchain)
          (cons "capy" (append (runtime-arguments result) (arg-results-rest results)))))
      (passthrough (arg-results-rest results))))

  ;; The number after LABEL in TEXT, or 0.
  (define (count-after text label)
    (let ([n (string-length text)] [m (string-length label)])
      (let find ([i 0])
        (cond
          [(> (+ i m) n) 0]
          [(string=? (substring text i (+ i m)) label)
            (let skip ([j (+ i m)])
              (cond
                [(>= j n) 0]
                ;; The reporter paints the number: step over the escape sequence
                [(char=? (string-ref text j) (integer->char 27))
                  (let end ([k j])
                    (cond
                      [(>= k n) 0]
                      [(char=? (string-ref text k) #\m) (skip (+ k 1))]
                      [else (end (+ k 1))]))]
                [(char<=? #\0 (string-ref text j) #\9)
                  (let digits ([k j])
                    (if (and (< k n) (char<=? #\0 (string-ref text k) #\9))
                      (digits (+ k 1))
                      (string->number (substring text j k))))]
                [(char=? (string-ref text j) #\newline) 0]
                [else (skip (+ j 1))]))]
          [else (find (+ i 1))]))))

  ;;; Tests
  ;;
  ;; `capy test` runs the test files of a package, like `cargo test` runs its test binaries. A
  ;; test file is a file of tests/ or a directory below it that ends in `.scm` (a script) or
  ;; `.sps` (an R6RS program, run with --r6rs and with the package directory in the load path,
  ;; so that tests/x/y.sls is the library (tests x y)). A file or directory whose name starts with
  ;; `_` or `.` is not a test: it holds helpers. Every file runs in its own runtime, in the
  ;; directory of the package, and passes when it exits with 0 and its SRFI 64 report counts no
  ;; unexpected failure and no unexpected success.
  ;;
  ;; A package of the toolchain (core, std, ...) is not built: its libraries are the toolchain's,
  ;; and the tests run with them. Any other package is built first.

  (define (test-parser)
    (let ([parser (common-parser #t)])
      (add-option! parser "filter" (defaults-to "") (value-help "NAME")
        (help "Run only the test of this name (the srfi 64 test name)"))
      (add-flag! parser "list" not-negatable (help "List the test files, and run none"))
      (add-flag! parser "no-fail-fast" not-negatable (help "Run every test file, even after one failed"))
      (add-option! parser "timeout" (defaults-to "300") (value-help "SECONDS")
        (help "Stop a test file that runs longer than this (0: never)"))
      (add-option! parser "test-threads" (defaults-to "1") (value-help "N")
        (help "The number of test files run at once"))
      parser))

  (define (positive-option results name allow-zero?)
    (let ([n (string->number (arg-results-ref results name))])
      (unless (and n (exact-integer? n) (if allow-zero? (>= n 0) (> n 0)))
        (fail (string-append "--" name " takes a " (if allow-zero? "non-negative" "positive") " number, not")
          (arg-results-ref results name)))
      n))

  (define (string-starts? s prefix)
    (and (>= (string-length s) (string-length prefix))
      (string=? (substring s 0 (string-length prefix)) prefix)))

  (define (string-ends? s suffix)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (string-contains? s part)
    (let ([n (string-length s)] [m (string-length part)])
      (let loop ([i 0])
        (cond [(> (+ i m) n) #f]
          [(string=? (substring s i (+ i m)) part) #t]
          [else (loop (+ i 1))]))))

  ;; The test files of the package in ROOT, relative to ROOT/tests, sorted.
  (define (test-files root)
    (let ([dir (path-join root "tests")])
      (define (helper? name) (or (string-starts? name "_") (string-starts? name ".")))
      (if (not (and (file-exists? dir) (file-directory? dir)))
        '()
        (list-sort string<?
          (let walk ([rel ""])
            (fold-left
              (lambda (out name)
                (let* ([r (if (string=? rel "") name (path-join rel name))]
                       [full (path-join dir r)])
                  (cond
                    [(helper? name) out]
                    [(file-directory? full) (append out (walk r))]
                    [(or (string-ends? name ".scm") (string-ends? name ".sps")) (append out (list r))]
                    [else out])))
              '()
              (directory-list (path-join dir rel))))))))

  ;; A test file to run: the package it is of, its name (relative to tests/), and how to run it.
  (define-record-type test-job (fields package name program args env cwd))

  ;; What running one came to. STATUS is the exit status, or `timeout`.
  (define-record-type test-outcome (fields job status passed failed output microseconds))

  ;; The counts of the SRFI 64 report in OUTPUT, the output of a test file.
  (define (report-counts output)
    (list (cons 'passed (count-after output "# of expected passes"))
      (cons 'failed (count-after output "# of unexpected failures"))
      (cons 'unexpected-successes (count-after output "# of unexpected successes"))
      (cons 'total-fail (count-after output "# of total FAIL"))))

  ;; Why the test file failed, as a string, or #f when it passed. STATUS is the exit status of
  ;; the runtime (negative: the signal that ended it) or `timeout`; COUNTS is `report-counts`.
  ;; The counts of the report come first: they say more than an exit status. A file that exits
  ;; with 0 and reports nothing passes, since a test file need not use SRFI 64.
  (define (test-failure status counts)
    (let* ([count (lambda (key) (cdr (assq key counts)))]
           [exit-text (lambda ()
                        (cond
                          [(eq? status 'timeout) "timed out"]
                          [(< status 0) (string-append "killed by signal " (number->string (- status)))]
                          [else (string-append "exit status " (number->string status))]))]
           [reported (filter values
                       (list
                         (and (> (count 'failed) 0) (plural (count 'failed) "unexpected failure" "unexpected failures"))
                         (and (> (count 'unexpected-successes) 0)
                           (plural (count 'unexpected-successes) "unexpected success" "unexpected successes"))
                         (and (> (count 'total-fail) 0) (string-append (number->string (count 'total-fail)) " FAIL"))))])
      (cond
        [(and (null? reported) (eqv? status 0)) #f]
        [(null? reported) (exit-text)]
        [(eqv? status 0) (join-text reported)]
        [else (string-append (join-text reported) ", " (exit-text))])))

  (define (join-text parts)
    (fold-left (lambda (out s) (string-append out ", " s)) (car parts) (cdr parts)))

  ;; The environment of a test file: the settings the test suites need (a deep stack, a heap
  ;; limit, the collector) unless the caller chose others, and `CAPY`, the driver, for tests that
  ;; run it.
  (define (test-environment extra)
    (append
      (filter (lambda (entry) (not (getenv (car entry))))
        (list (cons "MMTK_PLAN" "Immix") (cons "CAPY_GC_MAX_HEAP" "2G") (cons "RUST_MIN_STACK" "134217728")
          (cons "CAPY" (executable-path))))
      extra))

  ;; The test jobs of the package in ROOT (those whose name has one of WANTED in it, when
  ;; WANTED is not empty). A package of the toolchain runs with the toolchain's libraries; any
  ;; other is one of the roots of RESULT, the build of every package tested.
  (define (package-test-jobs results root wanted toolchain result)
    (let* ([manifest (read-manifest (path-join root "package.scm"))]
           [name (symbol->string (manifest-name manifest))]
           [files (filter (lambda (f) (or (null? wanted) (exists (lambda (w) (string-contains? f w)) wanted)))
                    (test-files root))]
           [filter-name (arg-results-ref results "filter")]
           [env (if (string=? filter-name "") '() (list (cons "CAPY_TEST_FILTER" filter-name)))])
      (cond
        [(null? files) '()]
        [else
          (let-values ([(base env)
                        (if (memq (manifest-name manifest) (toolchain-package-ids toolchain))
                          (values (list "--fresh-auto-compile")
                            (cons (cons "XDG_CACHE_HOME" (path-join root "target" "test-cache")) env))
                          (values
                            (runtime-arguments result
                              (find (lambda (pkg) (eq? (package-name pkg) (manifest-name manifest)))
                                (build-result-roots result)))
                            env))])
            (map (lambda (file)
                   (make-test-job name file (toolchain-runtime toolchain)
                     (append (list "capy") base
                       (if (string-ends? file ".sps") (list "--r6rs" "-L" root) '())
                       (list "-s" (path-join root "tests" file)))
                     (test-environment env)
                     root))
              files))])))

  (define (run-test-job job timeout)
    (let* ([start (microsecond)]
           [p (spawn (test-job-program job) (cdr (test-job-args job))
                #:env (test-job-env job) #:cwd (test-job-cwd job)
                #:stdin 'null #:stdout 'string #:stderr 'stdout)]
           [status (if (> timeout 0)
                     (guard (e [(timeout-expired? e) (process-kill p 'kill) (process-wait p) 'timeout])
                       (call-with-timeout timeout (lambda () (process-wait p))))
                     (process-wait p))]
           [output (process-stdout-string p)]
           [counts (report-counts output)])
      (make-test-outcome job status (cdr (assq 'passed counts))
        (test-failure status counts) output (- (microsecond) start))))

  ;; Runs JOBS, THREADS at once, and calls REPORT! with each outcome as it comes, in the thread of
  ;; the caller. With FAIL-FAST? no job starts after one failed. Returns the outcomes.
  (define (run-test-jobs! jobs threads timeout fail-fast? report!)
    (let ([queue (make-channel (max 1 (length jobs)))]
          [done (make-channel (max 1 (length jobs)))]
          [stop? #f])
      (for-each (lambda (job) (channel-send queue job)) jobs)
      (channel-close queue)
      (let start ([i 0])
        (when (< i (min threads (length jobs)))
          (go (lambda ()
                (let loop ()
                  (let-values ([(job ok?) (channel-recv/ok queue)])
                    (when ok?
                      ;; the worker stops the run itself: the collector would see the failure
                      ;; only after this worker had taken the next job
                      (channel-send done
                        (and (not stop?)
                          (let ([outcome (run-test-job job timeout)])
                            (when (and fail-fast? (test-outcome-failed outcome)) (set! stop? #t))
                            outcome)))
                      (loop))))))
          (start (+ i 1))))
      (let collect ([n (length jobs)] [out '()])
        (if (= n 0)
          (reverse out)
          (let ([outcome (channel-recv done)])
            (when outcome
              (report! outcome)
              (when (and fail-fast? (test-outcome-failed outcome)) (set! stop? #t)))
            (collect (- n 1) (if outcome (cons outcome out) out)))))))

  (define (seconds-text microseconds)
    (let ([tenths (div (+ microseconds 50000) 100000)])
      (string-append (number->string (div tenths 10)) "." (number->string (mod tenths 10)) "s")))

  (define (plural n one many) (string-append (number->string n) " " (if (= n 1) one many)))

  (define (command-test ui results)
    (let* ([toolchain (locate-toolchain)]
           [wanted (arg-results-rest results)]
           [threads (positive-option results "test-threads" #f)]
           [timeout (positive-option results "timeout" #t)]
           [selected (select-members (project-here) (selected-names results) (all-members? results))]
           ;; with several members, the ones that have tests
           [roots (if (null? (cdr selected))
                    selected
                    (filter (lambda (dir) (let ([t (path-join dir "tests")])
                                            (and (file-exists? t) (file-directory? t))))
                      selected))]
           [several? (and (pair? roots) (pair? (cdr roots)))]
           [label (lambda (job) (if several? (string-append (test-job-package job) ": " (test-job-name job)) (test-job-name job)))])
      (when (null? roots) (fail "no member of the workspace has a tests/ directory"))
      (let* ([ids (toolchain-package-ids toolchain)]
             ;; one build of every member tested: they share target/ and its libmap
             [built (filter (lambda (dir) (not (memq (manifest-name (read-manifest (path-join dir "package.scm"))) ids)))
                      roots)]
             [result (and (pair? built)
                       (build! (project-root (project-here)) built
                         (settings-of ui toolchain results (profile-of results))))]
             [jobs (apply append (map (lambda (root) (package-test-jobs results root wanted toolchain result)) roots))])
        (when (null? jobs)
          (fail (if (null? wanted) "no test files (tests/**/*.scm, tests/**/*.sps)" "no test file matches")))
        (cond
          [(arg-results-ref results "list")
            (for-each (lambda (job) (put-string (current-output-port) (string-append (label job) "\n"))) jobs)
            0]
          [else
            (ui-status! ui "Running" (plural (length jobs) "test file" "test files"))
            (let* ([start (microsecond)]
                   [outcomes
                     (run-test-jobs! jobs threads timeout (not (arg-results-ref results "no-fail-fast"))
                       (lambda (o)
                         (let ([reason (test-outcome-failed o)]
                               [time (seconds-text (test-outcome-microseconds o))])
                           (cond
                             [reason
                               (ui-status! ui "Testing" (string-append (label (test-outcome-job o)) " ... FAILED ("
                                                          reason ", " time ")"))
                               (put-string (current-error-port) (test-outcome-output o))]
                             [else
                               (ui-status! ui "Testing" (string-append (label (test-outcome-job o)) " ... ok ("
                                                          (number->string (test-outcome-passed o)) " passed, " time ")"))
                               (when (arg-results-ref results "verbose")
                                 (put-string (current-error-port) (test-outcome-output o)))]))))]
                   [failed (filter test-outcome-failed outcomes)]
                   [passed (fold-left + 0 (map test-outcome-passed outcomes))]
                   [not-run (- (length jobs) (length outcomes))]
                   [out (current-output-port)])
              (unless (null? failed)
                (put-string out "\nfailures:\n")
                (for-each (lambda (o) (put-string out (string-append "    " (label (test-outcome-job o)) "\n"))) failed))
              (put-string out
                (string-append "\ntest result: " (if (null? failed) "ok" "FAILED") ". "
                  (plural passed "test" "tests") " passed; "
                  (plural (length failed) "file" "files") " failed of " (number->string (length outcomes))
                  (if (> not-run 0) (string-append "; " (plural not-run "file" "files") " not run") "")
                  "; " (seconds-text (- (microsecond) start)) "\n"))
              (flush-output-port out)
              (if (null? failed) 0 1))]))))

  ;;; Install and dist

  ;; The prefix of `capy install` of a package: $CAPY_HOME, or ~/.capy.
  (define (default-install-prefix)
    (let ([home (getenv "CAPY_HOME")])
      (if (and home (not (string=? home ""))) home (path-join (home-directory) ".capy"))))

  (define (option-value results name)
    (let ([v (arg-results-ref results name)]) (and (not (string=? v "")) v)))

  (define (absolute path) (path-join (current-directory) path))

  (define (add-toolchain-options! parser what)
    (add-option! parser "toolchain" (defaults-to "") (value-help "DIR")
      (help (string-append what " the toolchain built in the stage directory DIR (./bootstrap.sh), not a package")))
    (add-option! parser "packages" (defaults-to "pkgs") (value-help "DIR")
      (help "With --toolchain, the directory of the packages")))

  ;; The options of the release build that `capy install` and `capy dist` make of a package.
  (define (add-release-build-options! parser)
    (add-resolve-flags! parser)
    (add-flag! parser "static-native" (defaults-to #t)
      (help "Link the native extensions and the runtime into the executables (--no-static-native: put them beside)"))
    (add-option! parser "jobs" (abbreviation "j") (defaults-to "4")
      (help "The number of files compiled at once")))

  (define (install-parser)
    (let ([parser (common-parser #f)])
      (add-release-build-options! parser)
      (add-option! parser "path" (defaults-to "") (value-help "DIR")
        (help "Install the package in DIR (default: the package of the current directory)"))
      (add-option! parser "version" (defaults-to "") (value-help "V")
        (help "With a URL, the version to install (the tag vV); the latest by default"))
      (add-option! parser "rev" (defaults-to "") (value-help "R")
        (help "With a URL, the commit, tag or branch to install"))
      (add-option! parser "prefix" (defaults-to "") (value-help "DIR")
        (help "Install into DIR/bin (default: $CAPY_HOME, or ~/.capy; required with --toolchain)"))
      (add-flag! parser "force" not-negatable
        (help "Replace files that another package or nothing installed"))
      (add-flag! parser "list" not-negatable
        (help "List the packages installed in the prefix"))
      (add-toolchain-options! parser "Install")
      parser))

  (define (uninstall-parser)
    (let ([parser (common-parser #f)])
      (add-option! parser "prefix" (defaults-to "") (value-help "DIR")
        (help "The prefix the package was installed in (default: $CAPY_HOME, or ~/.capy)"))
      parser))

  (define (dist-parser)
    (let ([parser (common-parser #f)])
      (add-release-build-options! parser)
      (add-option! parser "format" (defaults-to "portable") (allowed '("portable" "deb" "rpm"))
        (help "portable (a .tar.gz of an installed tree), deb or rpm"))
      (add-option! parser "out" (defaults-to "") (value-help "DIR")
        (help "Where the result goes (default: dist/ with --toolchain, target/dist/ for a package)"))
      (add-option! parser "name" (defaults-to "") (value-help "FILE")
        (help "The file name of the portable archive (default: NAME-VERSION-TARGET.tar.gz)"))
      (add-option! parser "version" (defaults-to "") (value-help "V")
        (help "The version the distribution says it has (default: the package's or the runtime's)"))
      (add-toolchain-options! parser "Package")
      parser))

  ;; A toolchain stage: its directory, the packages and the tree they are in, and the version of
  ;; its runtime (the directory under lib/capy where an installed runtime looks).
  (define (toolchain-of results)
    (let* ([stage (absolute (arg-results-ref results "toolchain"))]
           [pkgs (absolute (arg-results-ref results "packages"))]
           [runtime (path-join stage "capy-rt")])
      (unless (file-exists? runtime) (fail "not a toolchain directory (no capy-rt):" stage))
      (let ([version (guard (e [(process-failure? e) (fail "cannot run" runtime)])
                       (run/string runtime '("-c" "(display (implementation-version))")
                         #:env (list (cons "XDG_CACHE_HOME" (path-join stage "cache")))))])
        (values stage pkgs (path-directory pkgs) version))))

  ;; The release build of the package in ROOT that installs and distributions are made of: every
  ;; program linked into an executable. Returns the programs as (NAME . PATH) and the manifest.
  (define (release-build ui results root)
    (let* ([toolchain (locate-toolchain)]
           [settings (make-build-settings "release" (jobs-of results) #t (arg-results-ref results "verbose")
                       ui toolchain #f (resolve-options-of results) (arg-results-ref results "static-native"))]
           [result (build-package! root settings)]
           [programs (map (lambda (p) (cons (symbol->string (result-program-name p)) (result-program-path p)))
                       (build-result-programs result))])
      (when (null? programs)
        (fail "the package has no programs to install; list them in `programs` of package.scm"))
      (values programs (package-manifest (build-result-root result)))))

  ;; The directory of the package `capy install` installs, and what the record says it came from.
  (define (install-source ui results)
    (let ([rest (arg-results-rest results)] [path (option-value results "path")])
      (cond
        [(and path (pair? rest)) (fail "capy install takes a URL or --path, not both")]
        [path (values (absolute path) (string-append "path " (absolute path)))]
        [(null? rest)
          (when (or (option-value results "version") (option-value results "rev"))
            (fail "--version and --rev go with a URL"))
          (let ([root (package-here "capy install")]) (values root (string-append "path " root)))]
        [(pair? (cdr rest)) (fail "capy install takes one URL")]
        [else
          (let* ([url (car rest)]
                 [rev (cond
                        [(option-value results "rev") => values]
                        [(option-value results "version") => (lambda (v) (string-append "v" v))]
                        [(latest-version url #f) => (lambda (v) (string-append "v" v))]
                        [else (fail (string-append "the repository " url " has no tags of the form vX.Y.Z; "
                                      "give --rev to install a commit"))])]
                 [commit (or (git-expand-revision url rev) (fail (string-append "no revision " rev " in") url))])
            (ui-status! ui "Fetching" (string-append (git-repository-id url) " " rev))
            (let* ([tree (git-fetch-commit! url commit (list rev))]
                   ;; the build writes target/, and the fetched tree in the cache stays as it was fetched
                   [work (path-join (capy-cache-directory) "install" (git-repository-id url) commit)])
              (unless (file-exists? work)
                (create-directory* (path-directory work))
                (run "cp" (list "-R" tree work)))
              (values work (string-append "git " url " " commit))))])))

  (define (command-install ui results)
    (cond
      [(arg-results-ref results "list")
        (let ([prefix (or (option-value results "prefix") (default-install-prefix))])
          (for-each
            (lambda (record)
              (put-string (current-output-port)
                (string-append (car record) " " (cadr (assq 'version (cdr record)))
                  " (" (cadr (assq 'source (cdr record))) ")\n"))
              (for-each (lambda (f) (put-string (current-output-port) (string-append "    " f "\n")))
                (cdr (assq 'files (cdr record)))))
            (read-install-records prefix))
          0)]
      [(option-value results "toolchain")
        (let ([prefix (or (option-value results "prefix")
                        (fail "capy install --toolchain needs --prefix DIR"))])
          (let-values ([(stage pkgs tree version) (toolchain-of results)])
            (create-directory* prefix)
            (layout-toolchain! ui stage pkgs tree (absolute prefix) version)
            (ui-status! ui "Installed" (string-append "CapyScheme " version " in " (absolute prefix)))
            0))]
      [else
        (let ([prefix (absolute (or (option-value results "prefix") (default-install-prefix)))])
          (let-values ([(root source) (install-source ui results)])
            (let-values ([(programs manifest) (release-build ui results root)])
              (let* ([name (symbol->string (manifest-name manifest))]
                     [records (read-install-records prefix)])
                ;; the executables must not replace files that are not this package's
                (for-each
                  (lambda (program)
                    (let* ([file (path-join "bin" (car program))]
                           [owner (file-owner records file)])
                      (when (and (file-exists? (path-join prefix file))
                                 (not (and owner (string=? (car owner) name)))
                                 (not (arg-results-ref results "force")))
                        (fail (string-append (path-join prefix file) " exists and "
                                (if owner (string-append "belongs to " (car owner)) "was not installed by capy")
                                "; --force replaces it")))))
                  programs)
                (let ([files (layout-package! ui programs prefix)])
                  (record-install! prefix name (manifest-version manifest) source files)
                  (ui-status! ui "Installed" (string-append name " " (manifest-version manifest)
                                               " (" (number->string (length programs))
                                               (if (= (length programs) 1) " executable" " executables") ")"))
                  (let ([bin (path-join prefix "bin")] [path (or (getenv "PATH") "")])
                    (unless (exists (lambda (d) (string=? d bin)) (split-path-list path))
                      (ui-note! ui (string-append bin " is not in PATH"))))
                  0)))))]))

  (define (split-path-list text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text)) (reverse (cons (substring text start i) out))]
        [(char=? (string-ref text i) #\:) (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (command-uninstall ui results)
    (let ([rest (arg-results-rest results)]
          [prefix (absolute (or (option-value results "prefix") (default-install-prefix)))])
      (unless (and (pair? rest) (null? (cdr rest))) (fail "usage: capy uninstall NAME"))
      (let ([files (uninstall! prefix (car rest))])
        (ui-status! ui "Removed" (string-append (car rest) " ("
                                   (number->string (length files)) (if (= (length files) 1) " file" " files") ")"))
        0)))

  (define (command-dist ui results)
    (unless (null? (arg-results-rest results)) (fail "unexpected argument" (car (arg-results-rest results))))
    (let ([format (string->symbol (arg-results-ref results "format"))]
          [version (option-value results "version")])
      (define (finish! info layout! extras base default-out)
        (let* ([out (absolute (or (option-value results "out") default-out))]
               [work (path-join base "target" "dist-work" (symbol->string format))]
               [written (case format
                          [(portable) (list (write-portable! ui info layout! extras work out (option-value results "name")))]
                          [(deb) (list (write-deb! ui info layout! work out))]
                          [(rpm) (write-rpm! ui info layout! work out)])])
          (for-each (lambda (f) (ui-status! ui "Wrote" f)) written)
          0))
      (cond
        [(option-value results "toolchain")
          (let-values ([(stage pkgs tree runtime-version) (toolchain-of results)])
            (finish! (toolchain-dist-info (or version runtime-version))
              (lambda (dest) (layout-toolchain! ui stage pkgs tree dest runtime-version))
              (list (path-join tree "LICENSE") (path-join tree "CHANGELOG.md"))
              tree (path-join tree "dist")))]
        [else
          (let ([root (package-here "capy dist")])
            (let-values ([(programs manifest) (release-build ui results root)])
              (finish! (package-dist-info manifest version)
                (lambda (dest) (layout-package! ui programs dest))
                (map (lambda (f) (path-join root f)) '("LICENSE" "README.md" "CHANGELOG.md"))
                root (path-join root "target" "dist"))))])))

  ;;; Dependencies

  (define (resolver-parser)
    (let ([parser (common-parser #f)])
      (add-resolve-flags! parser)
      parser))

  (define (selection-resolver-parser)
    (let ([parser (resolver-parser)])
      (add-selection-flags! parser)
      parser))

  (define (count-text n one many)
    (string-append (number->string n) " " (if (= n 1) one many)))

  (define (command-fetch ui results)
    (let* ([root (project-root (project-here))])
      (unless (null? (arg-results-rest results)) (fail "capy fetch takes no arguments"))
      (let ([resolution (resolve-dependencies root (resolve-options-of results) ui)])
        (ui-status! ui "Fetched"
          (count-text (length (resolution-nodes resolution)) "git package" "git packages"))
        0)))

  (define (command-tree ui results)
    (let* ([project (project-here)]
           [selected (select-members project (selected-names results) (all-members? results))])
      (unless (null? (arg-results-rest results)) (fail "capy tree takes no arguments"))
      (let ([resolution (resolve-dependencies (project-root project) (resolve-options-of results) ui)])
        (let loop ([dirs selected] [first? #t])
          (when (pair? dirs)
            (unless first? (put-string (current-output-port) "\n"))
            (print-tree (car dirs) resolution)
            (loop (cdr dirs) #f)))
        (flush-output-port (current-output-port))
        0)))

  (define (print-tree root resolution)
    (let* ([packages (resolve-packages root resolution)]
           [top (car (reverse packages))]
           [port (current-output-port)]
           [printed (list top)])
      (define (line pkg)
        (let ([manifest (package-manifest pkg)]
              [node (resolution-node-by-id resolution (package-name pkg))])
          (string-append (symbol->string (manifest-name manifest)) " v"
            (if node (node-version node) (manifest-version manifest))
            " (" (if node (symbol->string (package-name pkg)) (package-directory pkg)) ")")))
      (define (show pkg prefix)
        (let loop ([deps (package-dependencies pkg)])
          (unless (null? deps)
            (let* ([dep (cdr (car deps))]
                   [last? (null? (cdr deps))]
                   [again? (and (memq dep printed) #t)])
              (put-string port
                (string-append prefix (if last? "└── " "├── ") (line dep) (if again? " (*)" "") "\n"))
              (unless again?
                (set! printed (cons dep printed))
                (show dep (string-append prefix (if last? "    " "│   "))))
              (loop (cdr deps))))))
      (put-string port (string-append (line top) "\n"))
      (show top "")))

  (define (read-text path)
    (call-with-input-file path get-string-all))

  (define (write-text! path text)
    (write-file-atomically! path (lambda (port) (put-string port text))))

  ;; Changes the manifest in the directory DIR with TRANSFORM (text to text), then resolves the
  ;; dependencies (of its workspace, if it is in one), which writes package.lock. The manifest is
  ;; restored when that fails.
  (define (edit-manifest! ui dir offline? update? transform)
    (edit-manifests! ui dir offline? update? (list (cons dir transform))))

  ;; The same for several manifests: EDITS are (directory . transform) pairs. The resolution is
  ;; the one of ROOT.
  (define (edit-manifests! ui root offline? update? edits)
    (let ([olds (map (lambda (edit) (read-text (path-join (car edit) "package.scm"))) edits)])
      (for-each (lambda (edit old) (write-text! (path-join (car edit) "package.scm") ((cdr edit) old)))
        edits olds)
      (guard (e [#t (for-each (lambda (edit old) (write-text! (path-join (car edit) "package.scm") old)) edits olds)
                    (raise e)])
        (for-each (lambda (edit) (check-root-file (path-join (car edit) "package.scm"))) edits)
        (resolve-dependencies root (make-resolve-options offline? #f update?) ui))))

  ;; Reads the package.scm PATH: the package in it, or the workspace when it is only one.
  (define (check-root-file path)
    (let ([ws (read-workspace-file path)])
      (unless (and ws (not (workspace-root-package? ws)))
        (read-manifest path))))

  ;; The directory whose package.scm `capy add` and `capy remove` edit, and the scope of the edit.
  (define (edit-target results what)
    (let ([project (project-here)])
      (if (arg-results-ref results "workspace-dep")
        (begin
          (unless (project-workspace project) (fail (string-append what " --workspace-dep needs a workspace")))
          (values (project-root project) 'workspace))
        (values (single-member project (selected-names results) #f what) 'package))))

  (define (member-parser)
    (let ([parser (resolver-parser)])
      (add-multi-option! parser "package" (abbreviation "p") (value-help "NAME")
        (help "The member of the workspace whose package.scm is edited"))
      (add-flag! parser "workspace-dep" not-negatable
        (help "Edit the dependencies of the workspace, which members inherit with (NAME (workspace #t))"))
      parser))

  (define (edit-parser)
    (let ([parser (member-parser)])
      (add-option! parser "name" (defaults-to "") (help "The name of the dependency"))
      (add-option! parser "version" (defaults-to "") (help "The lowest version needed"))
      (add-option! parser "rev" (defaults-to "") (help "A commit, tag or branch"))
      (add-option! parser "path" (defaults-to "") (help "A package in a directory"))
      parser))

  (define (last-component url)
    (let ([id (git-repository-id url)])
      (path-file-name id)))

  (define (command-add ui results)
    (let*-values ([(rest) (arg-results-rest results)]
                  [(root scope) (edit-target results "capy add")]
                  [(option) (lambda (name) (let ([v (arg-results-ref results name)]) (and (not (string=? v "")) v)))]
                  [(offline?) (arg-results-ref results "offline")])
      (when (and (option "version") (option "rev")) (fail "--version and --rev cannot be given together"))
      (cond
        [(option "path")
          => (lambda (path)
              (unless (null? rest) (fail "capy add --path takes no URL"))
              (when (or (option "version") (option "rev")) (fail "a path dependency has no version or rev"))
              (let* ([manifest (read-manifest (path-join (path-join root path) "package.scm"))]
                     [name (or (option "name") (symbol->string (manifest-name manifest)))])
                (ui-status! ui "Adding" (string-append name " (path " path ")"))
                (edit-manifest! ui root offline? #f
                  (lambda (text)
                    (add-dependency-text text name (dependency-text name (list (list "path" path))) scope)))))]
        [else
          (unless (and (pair? rest) (null? (cdr rest)))
            (fail "usage: capy add URL [--name NAME] [--version V | --rev R], or capy add --path DIR"))
          (let* ([url (car rest)]
                 [name (or (option "name") (last-component url))]
                 [clause
                   (cond
                     [(option "rev") => (lambda (rev) (list "rev" rev))]
                     [(option "version")
                       => (lambda (version)
                           (unless (parse-version version)
                             (fail "a version is numbers separated by dots, such as 1.2 or 1.2.3, not" version))
                           (list "version" version))]
                     [else
                       (when offline? (fail "capy add needs --version or --rev with --offline"))
                       (let ([latest (latest-version url #f)])
                         (unless latest
                           (fail (string-append "the repository " url " has no tags of the form vX.Y.Z; "
                                   "give --rev to depend on a commit")))
                         (list "version" latest))])])
            (ui-status! ui "Adding"
              (string-append name " (" (git-repository-id url) " " (car clause) " " (cadr clause) ")"))
            (edit-manifest! ui root offline? #f
              (lambda (text)
                (add-dependency-text text name (dependency-text name (list (list "git" url) clause)) scope))))])
      0))

  (define (command-remove ui results)
    (let*-values ([(rest) (arg-results-rest results)]
                  [(root scope) (edit-target results "capy remove")])
      (unless (and (pair? rest) (null? (cdr rest))) (fail "usage: capy remove NAME"))
      (ui-status! ui "Removing" (car rest))
      (edit-manifest! ui root (arg-results-ref results "offline") #f
        (lambda (text) (remove-dependency-text text (car rest) scope)))
      0))

  ;; Moves the git dependencies with a version (the ones named, or all) to the latest tag of their
  ;; major version, and looks up the revisions again. In a workspace these are the dependencies
  ;; of the workspace and the own dependencies of the selected members (not the inherited ones).
  (define (command-update ui results)
    (let* ([names (arg-results-rest results)]
           [project (project-here)]
           [root (project-root project)]
           [offline? (arg-results-ref results "offline")]
           [ws (project-workspace project)]
           ;; (directory scope dependency) of every dependency that could be updated
           [candidates
             (append
               (if ws
                 (map (lambda (d) (list root 'workspace d)) (workspace-dependencies ws))
                 '())
               (apply append
                 (map (lambda (dir)
                        (let ([inherited (inherited-dependency-names (read-text (path-join dir "package.scm")))])
                          (map (lambda (d) (list dir 'package d))
                            (filter (lambda (d) (not (member (symbol->string (dependency-name d)) inherited)))
                              (manifest-dependencies (read-manifest (path-join dir "package.scm")))))))
                   (select-members project (selected-names results) (all-members? results)))))]
           [candidate-name (lambda (c) (symbol->string (dependency-name (caddr c))))])
      (for-each
        (lambda (name)
          (let ([found (filter (lambda (c) (string=? (candidate-name c) name)) candidates)])
            (when (null? found) (fail (string-append "the " (if ws "workspace" "package") " has no dependency " name)))
            (unless (exists (lambda (c) (dependency-version (caddr c))) found)
              (fail (string-append "the dependency " name " has no version to update")))))
        names)
      (let* ([targets
               (filter (lambda (c)
                        (and (dependency-version (caddr c))
                          (or (null? names) (member (candidate-name c) names))))
                 candidates)]
             [changes
               (if (or offline? (null? targets))
                 '()
                 (filter values
                   (map (lambda (c)
                         (let* ([d (caddr c)]
                                [current (parse-version (dependency-version d))]
                                [latest (latest-version (dependency-git d) (car current))])
                           (and latest (version<? current (parse-version latest))
                             (list c (version->string current) latest))))
                     targets)))]
             [dirs (fold-left (lambda (dirs change) (if (member (car (car change)) dirs) dirs (cons (car (car change)) dirs)))
                     '() changes)])
        (for-each
          (lambda (change)
            (ui-status! ui "Updating"
              (string-append (candidate-name (car change)) " v" (cadr change) " -> v" (caddr change))))
          changes)
        (edit-manifests! ui root offline? #t
          (if (null? dirs)
            (list (cons root values))
            (map (lambda (dir)
                   (cons dir
                     (lambda (text)
                       (fold-left (lambda (text change)
                                    (if (string=? (car (car change)) dir)
                                      (set-dependency-version-text text (candidate-name (car change)) (caddr change)
                                        (cadr (car change)))
                                      text))
                         text changes))))
              (reverse dirs))))
        (when (null? changes) (ui-status! ui "Updating" "nothing to update")))
      0))

  ;;; Scripts

  ;; Removes the leading `--offline` and `--locked` of ARGS: the rest, and the two flags.
  (define (split-script-flags args)
    (let loop ([args args] [offline? #f] [locked? #f])
      (cond
        [(and (pair? args) (string=? (car args) "--offline")) (loop (cdr args) #t locked?)]
        [(and (pair? args) (string=? (car args) "--locked")) (loop (cdr args) offline? #t)]
        [else (values args offline? locked?)])))

  ;; `capy [--offline] [--locked] FILE.scm ARGUMENT ...`. 
  (define (command-script mode args)
    (let-values ([(rest offline? locked?) (split-script-flags args)])
      (let ([script (and (pair? rest) (file-exists? (car rest)) (not (file-directory? (car rest))) (car rest))])
        (let ([dependencies (if script (script-dependencies script) '())])
          (if (null? dependencies)
            (passthrough rest)
            (let ([ui (make-quiet-ui mode)] [toolchain (locate-toolchain)])
              (let-values ([(result directory)
                            (prepare-script! script dependencies
                              (lambda (directory)
                                (make-build-settings "debug" 4 #f #f ui toolchain #f
                                  (make-resolve-options offline? locked? #f))))])
                (exec! (toolchain-runtime toolchain)
                  (append (list "capy") (runtime-arguments result) (list script) (cdr rest))))))))))

  ;;; The runtime

  (define (passthrough args)
    (let ([runtime (locate-runtime)])
      (exec! runtime (cons "capy" args))))

  (define (report-error mode message)
    (when message
      (let ([ui (make-ui mode)])
        (put-string (current-error-port) (ui-error-text ui message))
        (flush-output-port (current-error-port)))))


  (define (mode-of results)
    (parse-color-argument (arg-results-ref results "color")))

  (define (with-ui handler)
    (lambda (results) (handler (make-ui (mode-of results)) results)))

  ;; A command of the driver. HANDLER takes the results of the command line, and returns the exit
  ;; status. DESCRIPTION is what `capy help NAME` says above the options.
  (define (driver-command name summary description invocation grammar handler)
    (command name
      'summary: (lambda (c) summary)
      'description: description
      'invocation: (lambda (c) (string-append "capy " name invocation))
      'grammar: grammar
      'run: (lambda (c) (handler (command-results c)))))

  (define (make-driver-runner run-trailing)
    (let ([runner (make-command-runner "capy" description-text)])
      (for-each
        (lambda (c) (command-runner-add-command! runner c))
        (list
          (driver-command "new" "create a package in the directory NAME"
            "Creates a package: package.scm, a source directory and, unless --lib, a program."
            " NAME [--lib]" (new-parser)
            (with-ui (lambda (ui results) (command-new ui results #f))))
          (driver-command "init" "create a package in the current directory"
            "Creates a package in the current directory, named after it."
            " [--lib]" (new-parser)
            (with-ui (lambda (ui results) (command-new ui results #t))))
          (driver-command "build" "compile the package and its dependencies"
            "Compiles the package and its dependencies into target/. With --toolchain DIR it builds core, std and the\npackages that ship with them (the stages of ./bootstrap.sh) into DIR instead."
            " [OPTIONS]" (build-parser)
            (with-ui (lambda (ui results) (command-build ui results #f))))
          (driver-command "check" "compile into target/check, reporting every error and warning"
            "Compiles the package into target/check: for the errors and the lints, not for running."
            " [OPTIONS]" (build-parser)
            (with-ui (lambda (ui results) (command-build ui results #t))))
          (driver-command "run" "build, then run a program of the package"
            "Builds the package, then runs one of its programs. The arguments after -- are the program's."
            " [PROGRAM] [OPTIONS] [-- ARGUMENT ...]" (common-parser #t)
            (with-ui (lambda (ui results) (command-run ui results (run-trailing)))))
          (driver-command "test" "build, then run the test files of tests/"
            "Builds the package (not a package of the toolchain: its tests run with the toolchain's libraries), then runs\nevery test file of tests/: tests/**/*.scm as scripts, tests/**/*.sps as R6RS programs. Names that start with _ or .\nare helpers. A FILTER keeps the files whose path (relative to tests/) has it in it; --filter runs only the SRFI 64\ntest of that name. The first file that fails stops the run, unless --no-fail-fast. In a workspace it tests the\nmember it is run in; -p NAME and --workspace select others (every member with a tests/ directory)."
            " [FILTER ...] [OPTIONS]" (test-parser)
            (with-ui command-test))
          (driver-command "install" "build the package for release and install its executables"
            "Builds the package (the current one, the one in --path DIR, or the one at URL) for release, links its\nprograms into executables and installs them in PREFIX/bin. PREFIX is $CAPY_HOME, or ~/.capy. The files are\nrecorded in PREFIX/.capy-installs.scm: a second install replaces them, `capy uninstall` removes them.\nWith --toolchain DIR it installs the toolchain built in DIR by ./bootstrap.sh under --prefix instead."
            " [URL | --path DIR] [OPTIONS]" (install-parser)
            (with-ui command-install))
          (driver-command "uninstall" "remove a package that capy install installed"
            "Removes the files that `capy install` wrote for the package NAME."
            " NAME [OPTIONS]" (uninstall-parser)
            (with-ui command-uninstall))
          (driver-command "dist" "make a .tar.gz, deb or rpm of the package (or of the toolchain)"
            "Builds the package for release and packs its executables: --format portable (a .tar.gz that unpacks\nto NAME-VERSION/bin), deb (dpkg-deb) or rpm (rpmbuild), into target/dist. With --toolchain DIR it packs\nthe toolchain built in DIR by ./bootstrap.sh, into dist/. The metadata of a deb or rpm comes from\npackage.scm: description, license, authors, homepage, dist."
            " [OPTIONS]" (dist-parser)
            (with-ui command-dist))
          (driver-command "clean" "remove target/"
            "Removes the target/ directory of the package."
            "" (common-parser #f)
            (with-ui command-clean))
          (driver-command "repl" "start the REPL (with the libraries of the package, if in one)"
            "Starts the REPL. In a package it has the libraries of the package; anywhere else it is the\nREPL of the runtime."
            " [OPTIONS]" (common-parser #t)
            (with-ui command-repl))
          (driver-command "add" "add a dependency: URL, or --path DIR"
            "Adds a dependency to package.scm (the latest version tag of URL, unless --version or --rev says\notherwise) and locks it."
            " URL | --path DIR [OPTIONS]" (edit-parser)
            (with-ui command-add))
          (driver-command "remove" "remove a dependency"
            "Removes the dependency NAME from package.scm and updates package.lock."
            " NAME [OPTIONS]" (member-parser)
            (with-ui command-remove))
          (driver-command "update" "move git dependencies to their latest version tags"
            "Moves the git dependencies (the ones named, or all) to the latest tag of their major version."
            " [NAME ...] [OPTIONS]" (selection-resolver-parser)
            (with-ui command-update))
          (driver-command "fetch" "fetch the git dependencies and write package.lock"
            "Resolves the git dependencies, fetches them and writes package.lock."
            " [OPTIONS]" (resolver-parser)
            (with-ui command-fetch))
          (driver-command "tree" "show the packages the package depends on"
            "Prints the dependencies of the package as a tree."
            " [OPTIONS]" (selection-resolver-parser)
            (with-ui command-tree))))
      runner))

  ;; The arguments after the first `--` of `capy run ...` belong to the program, not to the driver.
  ;; Returns the arguments before it, and a thunk for the ones after.
  (define (split-run-arguments args)
    (if (and (pair? args) (string=? (car args) "run"))
      (let-values ([(before after) (split-at-dashes (cdr args))])
        (values (cons "run" before) (lambda () after)))
      (values args (lambda () '()))))

  (define (driver-command-line? args)
    (or (null? args)
      (member (car args) commands)
      (member (car args) '("--help" "-h"))))

  ;; Runs the command ARGS, the command line without the program name. Returns the exit status.
  (define (run-cli args)
    (let ([mode (guard (e [#t 'auto]) (color-mode args))])
      (guard (e [(driver-error? e) (report-error mode (driver-error-message e)) 1]
                [(and (message-condition? e) (not (violation? e)))
                  (report-error mode (condition-message e)) 1])
        (cond
          [(and (pair? args) (string=? (car args) "--version"))
            (put-string (current-output-port) (string-append "CapyScheme " (implementation-version) "\n"))
            0]
          [(driver-command-line? args)
            (let-values ([(driver-args run-trailing) (split-run-arguments args)])
              (let ([status (command-runner-run (make-driver-runner run-trailing) driver-args)])
                (cond
                  [(integer? status) status]
                  [else
                    ;; The runner printed the help and left the last line unfinished.
                    (newline (current-output-port))
                    (flush-output-port (current-output-port))
                    0])))]
          [else (command-script mode args)])))))
