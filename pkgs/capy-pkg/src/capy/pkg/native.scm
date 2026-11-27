;;; Native extension handling
(library (capy pkg native)
  (export build-natives!
    native-key)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) getenv file-exists? file-directory? directory-list create-directory*
      delete-file rename-file dll-prefix dll-suffix current-directory)
    (only (capy libmap) file-hash)
    (capy crypto sha256)
    (capy path)
    (capy process)
    (capy pkg manifest)
    (capy pkg ui)
    (capy pkg util))

  ;; The key of the extension NAME of the package whose libraries have the id ID.
  (define (native-key id name) (list '|#%native| id name))

  (define hex-digits "0123456789abcdef")

  (define (hex bytes)
    (let ([out (make-string (* 2 (bytevector-length bytes)))])
      (do ([i 0 (+ i 1)]) ((= i (bytevector-length bytes)) out)
        (let ([b (bytevector-u8-ref bytes i)])
          (string-set! out (* 2 i) (string-ref hex-digits (div b 16)))
          (string-set! out (+ 1 (* 2 i)) (string-ref hex-digits (mod b 16)))))))

  (define (string-suffix? s suffix)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (string-prefix? s prefix)
    (let ([n (string-length s)] [m (string-length prefix)])
      (and (>= n m) (string=? (substring s 0 m) prefix))))

  (define (join strings separator)
    (if (null? strings)
      ""
      (fold-left (lambda (out s) (string-append out separator s)) (car strings) (cdr strings))))

  (define (read-datum path)
    (guard (e [#t #f])
      (call-with-input-file path read)))

  (define (write-datum! path datum)
    (write-file-atomically! path
      (lambda (port) (write datum port) (newline port))))

  ;;; The files

  ;; The files below DIR, as paths that start with DIR, sorted; the directories `target` and the
  ;; ones whose name starts with a dot are left out.
  (define (files-below dir)
    (let walk ([dir dir])
      (apply append
        (map (lambda (name)
              (let ([path (string-append dir "/" name)])
                (cond
                  [(or (string=? name "target") (char=? (string-ref name 0) #\.)) '()]
                  [(file-directory? path) (walk path)]
                  [else (list path)])))
          (list-sort string<? (directory-list dir))))))

  (define (copy-file! from to)
    (let ([bytes (call-with-port (open-file-input-port from) get-bytevector-all)])
      (when (file-exists? to) (delete-file to))   ; a signed binary is not overwritten in place
      (call-with-port (open-file-output-port to (file-options no-fail))
        (lambda (port) (put-bytevector port bytes)))))

  ;; PATH as it is recorded: relative to the directory BASE when it is below it.
  (define (recorded-path path base)
    (let ([prefix (string-append base "/")])
      (if (string-prefix? path prefix) (substring path (string-length prefix) (string-length path)) path)))

  (define (absolute path) (if (path-absolute? path) path (path-join (current-directory) path)))

  (define (absolute-path recorded base) (if (path-absolute? recorded) recorded (string-append base "/" recorded)))

  ;; The `(recorded-path . hash)` pairs of the files PATHS, sorted, without repeats.
  (define (input-hashes paths base)
    (let ([table (make-hashtable string-hash string=?)])
      (for-each
        (lambda (path)
          (let ([recorded (recorded-path path base)])
            (hashtable-set! table recorded (or (file-hash path) "missing"))))
        paths)
      (let-values ([(keys values) (hashtable-entries table)])
        (list-sort (lambda (a b) (string<? (car a) (car b)))
          (map cons (vector->list keys) (vector->list values))))))

  ;; The identity of an extension built with FLAGS from INPUTS: the places outside the package
  ;; are known by their file names.
  (define (identity-of flags inputs)
    (substring
      (hex (sha256
            (string->utf8
              (apply string-append flags "\n"
                (map (lambda (input)
                      (string-append
                        (if (path-absolute? (car input)) (string-append "external:" (path-file-name (car input))) (car input))
                        "\n" (cdr input) "\n"))
                  inputs)))))
      0 32))

  ;;; Running programs

  ;; Runs PROGRAM with ARGS; returns the exit status and what it wrote (both streams). A program
  ;; that cannot be run fails the build with a message that names it.
  (define (run-program program args env cwd)
    (let ([p (guard (e [#t (fail (string-append "cannot run " program ":")
                             (if (message-condition? e) (condition-message e) "not found in the PATH"))])
               (spawn program args #:env env #:cwd cwd #:stdout 'string #:stderr 'stdout))])
      (process-wait p)
      (values (process-exit-status p) (process-stdout-string p))))

  (define (run-program! what program args env cwd)
    (let-values ([(status output) (run-program program args env cwd)])
      (unless (eqv? status 0)
        (fail (string-append what " failed (" program " " (join args " ") ")\n" output)))
      output))

  (define (lines text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text)) (reverse (if (> i start) (cons (substring text start i) out) out))]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  ;;; The kinds

  ;; What a kind of extension is built with. CONTEXT is the build.
  (define-record-type context
    (fields profile runtime compiler-id build-directory ui include-directories static?))

  ;; The libcapy of the toolchain: beside the runtime (a build tree) or in ../lib of it (an install).
  (define (libcapy-directory runtime)
    (let* ([directory (path-directory runtime)]
           [name (string-append (dll-prefix) "capy" (dll-suffix))])
      (cond
        [(file-exists? (path-join directory name)) directory]
        [(file-exists? (path-join directory (path-join ".." (path-join "lib" name))))
          (path-join directory (path-join ".." "lib"))]
        [else (fail (string-append "cannot find " name " of the toolchain (next to, or in ../lib of)") runtime)])))

  ;; The toolchain is built for use: its extensions (cparse, which `c-include` parses headers
  ;; with) are optimized like a release.
  (define (release? context) (member (context-profile context) '("release" "toolchain")))

  ;; A kind gives: the files its sources are, the text of its flags, and a procedure that builds
  ;; it into the file OUT and returns the extra files it was built from.
  ;; `(sources files)` -> the files that decide the build, from the directory of the package.
  (define (extension-sources ext directory)
    (case (extension-kind ext)
      [(cargo)
        (let ([crate (path-join directory (car (extension-files ext)))])
          (unless (file-exists? (path-join crate "Cargo.toml"))
            (fail (string-append "the extension " (symbol->string (extension-name ext))
                   " has no Cargo.toml in") crate))
          (files-below crate))]
      [(c)
        (map (lambda (file)
              (let ([path (path-join directory file)])
                (unless (file-exists? path)
                  (fail (string-append "the extension " (symbol->string (extension-name ext))
                         " has no source file") path))
                path))
          (extension-files ext))]
      [(prebuilt)
        (let ([path (path-join directory (car (extension-files ext)))])
          (unless (file-exists? path)
            (fail (string-append "the extension " (symbol->string (extension-name ext))
                   " names no library") path))
          (let ([archive (extension-archive ext)])
            (cond
              [(not archive) (list path)]
              [else
                (unless (file-exists? (path-join directory archive))
                  (fail (string-append "the extension " (symbol->string (extension-name ext))
                         " names no archive") (path-join directory archive)))
                (list path (path-join directory archive))])))]))

  (define (extension-flags ext context)
    (join
      (case (extension-kind ext)
        [(cargo) (list "cargo" (context-profile context))]
        [(c) (append (list "c" (c-compiler) (if (release? context) "release" "debug") "cflags")
              (extension-cflags ext) (list "link") (extension-link ext))]
        [else (list "prebuilt")])
      " "))

  (define (c-compiler) (or (let ([cc (getenv "CC")]) (and cc (not (string=? cc "")) cc)) "cc"))

  ;; Builds a C extension: the files are compiled one by one, with the headers they include
  ;; recorded in a dependency file, and linked. Returns the headers.
  (define (build-c! ext directory work out context)
    (let* ([cc (c-compiler)]
           [objects '()]
           [headers '()])
      (create-directory* work)
      (let loop ([files (extension-sources ext directory)] [i 0])
        (unless (null? files)
          (let ([object (path-join work (string-append (number->string i) ".o"))]
                [deps (path-join work (string-append (number->string i) ".d"))])
            (run-program! (string-append "compiling " (path-file-name (car files)))
              cc
              (append (list "-fPIC" "-c" "-MMD" "-MF" deps)
                (if (release? context) (list "-O2") (list "-g"))
                (apply append (map (lambda (dir) (list "-I" dir)) (context-include-directories context)))
                (extension-cflags ext)
                (list "-o" object (car files)))
              '() directory)
            (set! objects (append objects (list object)))
            (set! headers (append headers (dependency-file-paths deps)))
            (loop (cdr files) (+ i 1)))))
      (run-program! (string-append "linking " (symbol->string (extension-name ext)))
        cc
        (append
          (if (string-suffix? (dll-suffix) "dylib")
            (list "-dynamiclib" "-undefined" "dynamic_lookup")
            (list "-shared"))
          (list "-o" out)
          objects
          (map (lambda (lib) (if (string-prefix? lib "-") lib (string-append "-l" lib))) (extension-link ext)))
        '() directory)
      (map (lambda (path) (recorded-path path directory)) headers)))

  ;; The paths a Makefile-style dependency file lists after the colon, without the sources
  ;; themselves being singled out: backslash-newlines and `\ ` are understood.
  (define (dependency-file-paths path)
    (if (not (file-exists? path))
      '()
      (let* ([text (call-with-input-file path get-string-all)]
             [colon (let loop ([i 0])
                      (cond [(>= i (string-length text)) #f]
                        [(and (char=? (string-ref text i) #\:) (< (+ i 1) (string-length text))
                           (memv (string-ref text (+ i 1)) '(#\space #\newline #\tab)))
                          i]
                        [else (loop (+ i 1))]))])
        (if (not colon)
          '()
          (let loop ([i (+ colon 1)] [current '()] [out '()])
            (define (flush) (if (null? current) out (cons (list->string (reverse current)) out)))
            (cond
              [(>= i (string-length text)) (reverse (flush))]
              [else
                (let ([c (string-ref text i)])
                  (cond
                    [(and (char=? c #\\) (< (+ i 1) (string-length text)))
                      (let ([next (string-ref text (+ i 1))])
                        (cond
                          [(char=? next #\newline) (loop (+ i 2) current (flush))]
                          [else (loop (+ i 2) (cons next current) out)]))]
                    [(memv c '(#\space #\newline #\tab)) (loop (+ i 1) '() (flush))]
                    [else (loop (+ i 1) (cons c current) out)]))]))))))

  ;; The `.dylib` or `.so` a cargo build left in the directory DIR (not in its subdirectories).
  (define (cargo-library dir)
    (let ([found (filter (lambda (name) (and (string-suffix? name (dll-suffix)) (string-prefix? name (dll-prefix))))
                   (list-sort string<? (directory-list dir)))])
      (cond
        [(and (pair? found) (null? (cdr found))) (path-join dir (car found))]
        [(null? found) (fail "cargo built no shared library (is the crate-type of the crate cdylib?) in" dir)]
        [else (fail "cargo built several shared libraries in" dir (join found " "))])))

  ;; The directories of the packages cargo takes from paths (the crate itself, the crates of its
  ;; workspace it depends on) in the `cargo tree` text.
  (define (path-dependency-directories text)
    (let loop ([ls (lines text)] [out '()])
      (cond
        [(null? ls) (reverse out)]
        [else
          (let* ([line (car ls)]
                 [open (let find ([i 0])
                         (cond [(>= (+ i 1) (string-length line)) #f]
                           [(and (char=? (string-ref line i) #\() (char=? (string-ref line (+ i 1)) #\/)) i]
                           [else (find (+ i 1))]))]
                 [close (and open
                          (let find ([i (+ open 1)])
                            (cond [(>= i (string-length line)) #f]
                              [(char=? (string-ref line i) #\)) i]
                              [else (find (+ i 1))])))])
            (loop (cdr ls)
              (if (and open close (> close open))
                (let ([dir (substring line (+ open 1) close)])
                  (if (member dir out) out (cons dir out)))
                out)))])))

  ;; Builds a crate with cargo. Returns the extra files the library was built from: the lock file,
  ;; the manifest of the workspace and the files of the path dependencies.
  (define (build-cargo! ext directory work out context)
    (let* ([crate (path-join directory (car (extension-files ext)))]
           [manifest (path-join crate "Cargo.toml")]
           [libcapy (libcapy-directory (context-runtime context))]
           ;; cargo is run by make-like tools that pass a jobserver it cannot use
           [env (list (cons "CAPY_LIB_DIR" libcapy) (cons "MAKEFLAGS" #f) (cons "MAKELEVEL" #f)
                  (cons "MFLAGS" #f))])
      (create-directory* work)
      (run-program! (string-append "building " (symbol->string (extension-name ext)))
        "cargo"
        (append (list "build" "--manifest-path" manifest "--target-dir" work)
          (if (release? context) (list "--release") '()))
        env crate)
      (copy-file! (cargo-library (path-join work (if (release? context) "release" "debug"))) out)
      (let* ([workspace (let ([text (run-program! "locating the workspace" "cargo"
                                      (list "locate-project" "--workspace" "--message-format" "plain"
                                        "--manifest-path" manifest)
                                      env crate)])
                          (car (lines text)))]
             [workspace-directory (path-directory workspace)]
             [tree (run-program! "listing the dependencies" "cargo"
                     (list "tree" "--manifest-path" manifest "--prefix" "none" "--format" "{p}"
                       "--edges" "normal,build")
                     env crate)]
             [dependency-directories
               (filter (lambda (dir) (not (string=? dir crate))) (path-dependency-directories tree))])
        ;; the files of the workspace are recorded relative to the package, wherever it is
        (map (lambda (path)
              (if (string-prefix? path (string-append workspace-directory "/"))
                (path-relative path directory)
                path))
          (append
            (filter file-exists? (list workspace (path-join workspace-directory "Cargo.lock")))
            (apply append (map files-below (filter file-directory? dependency-directories))))))))

  ;;; The static form

  (define (hex-byte n) (string (string-ref hex-digits (div n 16)) (string-ref hex-digits (mod n 16))))

  ;; The symbol the entry point of the extension KEY has in an executable: letters and digits stay,
  ;; any other character of the package id or the name is written `_` and its code in hex.
  (define (init-symbol key)
    (define (encode text)
      (apply string-append
        (map (lambda (c)
              (if (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9))
                (string c)
                (string-append "_" (hex-byte (char->integer c)))))
          (string->list text))))
    (string-append "capy_ext_" (encode (symbol->string (cadr key))) "_" (encode (symbol->string (caddr key)))
      "_OnLoad"))

  (define (split-words text)
    (let loop ([i 0] [start #f] [out '()])
      (define (flush) (if start (cons (substring text start i) out) out))
      (cond
        [(= i (string-length text)) (reverse (flush))]
        [(memv (string-ref text i) '(#\space #\newline #\tab #\return)) (loop (+ i 1) #f (flush))]
        [else (loop (+ i 1) (or start i) out)])))

  (define (string-search needle text)
    (let ([n (string-length needle)] [m (string-length text)])
      (let loop ([i 0])
        (cond
          [(> (+ i n) m) #f]
          [(string=? (substring text i (+ i n)) needle) i]
          [else (loop (+ i 1))]))))

  ;; The system libraries rustc says a static library needs: the text after `native-static-libs:`.
  ;; `-lcapy` is what `capy-sni-sys` asks for; the runtime is in the executable.
  (define (native-static-libs output)
    (let ([marker "native-static-libs:"])
      (let loop ([ls (lines output)])
        (cond
          [(null? ls) '()]
          [(string-search marker (car ls))
            => (lambda (at)
                (filter (lambda (w) (not (string=? w "-lcapy")))
                  (split-words (substring (car ls) (+ at (string-length marker)) (string-length (car ls))))))]
          [else (loop (cdr ls))]))))

  (define (archiver) (or (let ([ar (getenv "AR")]) (and ar (not (string=? ar "")) ar)) "ar"))

  ;; The static archive a cargo build left in the directory DIR.
  (define (cargo-archive dir)
    (let ([found (filter (lambda (name) (and (string-suffix? name ".a") (string-prefix? name "lib")))
                   (list-sort string<? (directory-list dir)))])
      (cond
        [(and (pair? found) (null? (cdr found))) (path-join dir (car found))]
        [(null? found) (fail "cargo built no static library in" dir)]
        [else (fail "cargo built several static libraries in" dir (join found " "))])))

  ;; Builds the crate of EXT as a static library into ARCHIVE; returns the linker arguments.
  (define (build-cargo-static! ext directory work archive init context)
    (let* ([crate (path-join directory (car (extension-files ext)))]
           [manifest (path-join crate "Cargo.toml")]
           [target (path-join work "static")]
           [libcapy (libcapy-directory (context-runtime context))]
           ;; cargo is run by make-like tools that pass a jobserver it cannot use. The runtime does not
           ;; unwind (panic = "abort"), and the crate has to agree with it where both are in one program.
           [env (list (cons "CAPY_LIB_DIR" libcapy) (cons "CAPY_SNI_ONLOAD_SYMBOL" init)
                  (cons "CARGO_PROFILE_DEV_PANIC" "abort") (cons "CARGO_PROFILE_RELEASE_PANIC" "abort")
                  (cons "MAKEFLAGS" #f) (cons "MAKELEVEL" #f) (cons "MFLAGS" #f))]
           [output (run-program! (string-append "building " (symbol->string (extension-name ext)) " as a static library")
                    "cargo"
                    (append (list "rustc" "--lib" "--crate-type" "staticlib" "--manifest-path" manifest
                             "--target-dir" target)
                      (if (release? context) (list "--release") '())
                      (list "--" "--print" "native-static-libs"))
                    env crate)])
      (copy-file! (cargo-archive (path-join target (if (release? context) "release" "debug"))) archive)
      (native-static-libs output)))

  ;; Compiles the C sources of EXT again with the entry point under the name INIT and archives the
  ;; objects into ARCHIVE; returns the linker arguments (the libraries).
  (define (build-c-static! ext directory work archive init context)
    (let ([cc (c-compiler)] [objects '()])
      (create-directory* work)
      (let loop ([files (extension-sources ext directory)] [i 0])
        (unless (null? files)
          (let ([object (path-join work (string-append "static-" (number->string i) ".o"))])
            (run-program! (string-append "compiling " (path-file-name (car files)) " for a static executable")
              cc
              (append (list "-fPIC" "-c" (string-append "-DSNI_OnLoad=" init))
                (if (release? context) (list "-O2") (list "-g"))
                (apply append (map (lambda (dir) (list "-I" dir)) (context-include-directories context)))
                (extension-cflags ext)
                (list "-o" object (car files)))
              '() directory)
            (set! objects (append objects (list object)))
            (loop (cdr files) (+ i 1)))))
      (when (file-exists? archive) (delete-file archive))
      (run-program! (string-append "archiving " (symbol->string (extension-name ext)))
        (archiver) (append (list "rcs" archive) objects) '() directory)
      (map (lambda (lib) (if (string-prefix? lib "-") lib (string-append "-l" lib))) (extension-link ext))))

  ;; The archive of a `prebuilt` extension, copied to ARCHIVE. An extension that has no archive cannot
  ;; be linked into an executable.
  (define (copy-prebuilt-static! ext directory archive)
    (unless (extension-archive ext)
      (fail (string-append "the prebuilt extension " (symbol->string (extension-name ext))
             " has no archive, which a fully static executable is linked from. Give it as the second file: "
             "(prebuilt \"" (car (extension-files ext)) "\" \"lib" (symbol->string (extension-name ext)) ".a\")")))
    (copy-file! (path-join directory (extension-archive ext)) archive)
    '())

  ;; Makes the static form of the extension EXT (NAME.a and NAME.link in OUT-DIRECTORY) if it is not
  ;; current for IDENTITY.
  (define (ensure-static! ext directory key identity out-directory work-directory context label)
    (let* ([name (symbol->string (extension-name ext))]
           [archive-name (string-append name ".a")]
           [archive (path-join out-directory archive-name)]
           [descriptor-path (path-join out-directory (string-append name ".link"))]
           [saved (read-datum descriptor-path)])
      (unless (and saved (list? saved) (= (length saved) 6) (eq? (car saved) 'static-native)
                (equal? (list-ref saved 1) key) (equal? (list-ref saved 2) identity)
                (file-exists? archive))
        (ui-status! (context-ui context) "Building"
          (string-append "native extension " name " as a static library (" label ")"))
        (when (file-exists? descriptor-path) (delete-file descriptor-path))
        (let* ([init (if (eq? (extension-kind ext) 'prebuilt) "SNI_OnLoad" (init-symbol key))]
               [flags (case (extension-kind ext)
                       [(cargo) (build-cargo-static! ext directory work-directory archive init context)]
                       [(c) (build-c-static! ext directory work-directory archive init context)]
                       [else (copy-prebuilt-static! ext directory archive)])])
          (write-datum! descriptor-path (list 'static-native key identity init archive-name flags))))))

  ;;; Building

  ;; The entry `(key file identity)` of the extension EXT of the package in DIRECTORY, built if it
  ;; is not current. KEY-ID is the id of the libraries of the package; OUT-DIRECTORY the directory
  ;; of the shared libraries; WORK-DIRECTORY the one the build may leave its files in.
  (define (ensure-extension! ext directory0 key-id out-directory0 work-directory0 context label)
    (let* ([directory (absolute directory0)]
           [out-directory (absolute out-directory0)]
           [work-directory (absolute work-directory0)]
           [name (symbol->string (extension-name ext))]
           [file (string-append name (dll-suffix))]
           [out (path-join out-directory file)]
           [descriptor-path (path-join out-directory (string-append name ".native"))]
           [key (native-key key-id (extension-name ext))]
           [flags (extension-flags ext context)]
           [sources (extension-sources ext directory)]
           [saved (let ([d (read-datum descriptor-path)])
                    (and d (list? d) (= (length d) 8) (eq? (car d) 'native) d))]
           [saved-extras (if saved (list-ref saved 6) '())]
           [current-inputs
             (and saved (list? saved-extras) (for-all string? saved-extras)
               (input-hashes
                 (append sources (map (lambda (extra) (absolute-path extra directory)) saved-extras))
                 directory))])
      (define (entry identity)
        (when (context-static? context)
          (ensure-static! ext directory key identity out-directory work-directory context label))
        (list key out identity))
      (cond
        [(and saved (file-exists? out)
           (equal? (list-ref saved 1) key)
           (equal? (list-ref saved 4) (context-compiler-id context))
           (equal? (list-ref saved 5) flags)
           (equal? current-inputs (list-ref saved 7)))
          (entry (list-ref saved 2))]
        [else
          (ui-status! (context-ui context) "Building"
            (string-append "native extension " name " (" label ")"))
          (create-directory* out-directory)
          (when (file-exists? descriptor-path) (delete-file descriptor-path))
          (let* ([extras
                   (case (extension-kind ext)
                     [(cargo) (build-cargo! ext directory work-directory out context)]
                     [(c) (build-c! ext directory work-directory out context)]
                     [else (copy-file! (car sources) out) '()])]
                 [inputs (input-hashes
                           (append sources (map (lambda (extra) (absolute-path extra directory)) extras))
                           directory)]
                 [identity (identity-of flags inputs)])
            (write-datum! descriptor-path
              (list 'native key identity file (context-compiler-id context) flags extras inputs))
            (entry identity))])))

  ;; Builds the native extensions of PACKAGES. For each package PACKAGE:
  ;;   (package-extensions package)   its extensions
  ;;   (package-directory package)    the directory of its manifest
  ;;   (package-key-id package)       the id of its libraries
  ;;   (package-native-directory package)  where its shared libraries go
  ;;   (package-work-directory package)    where it may leave files of its build
  ;;   (package-label package)        its name in the status lines
  ;; Returns the entries `(key file identity)` of all the extensions, with absolute paths.
  (define (build-natives! packages package-extensions package-directory package-key-id
            package-native-directory package-work-directory package-label
            profile runtime compiler-id ui include-directories static?)
    (let ([context (make-context profile runtime compiler-id #f ui include-directories static?)])
      (apply append
        (map
          (lambda (package)
            (map
              (lambda (ext)
                (ensure-extension! ext (package-directory package) (package-key-id package)
                  (package-native-directory package)
                  (path-join (package-work-directory package) (symbol->string (extension-name ext)))
                  context (package-label package)))
              (package-extensions package)))
          packages)))))
