;;; -*- mode: scheme; coding: utf-8; -*-
;;; The library manager: how a library name is converted into a file and how that
;;; file is loaded on demand.
;;;
;;; A library is named by a list of symbols and non-negative integers, the
;;; library key, like `(rnrs base)` or `(srfi 1)`. The file of a library is found
;;; by the loader, which searches `%load-path` for the name of the library
;;; (`rnrs/base`, `srfi/1`, ...) with each of `%load-extensions`.
;;;
;;; - `(resolve-library key)` is the environment of the library: it is the
;;;   registered one when the library is defined, and the library is loaded from
;;;   its file otherwise. It is an error when no library is found.
;;; - `(load-library key)` loads the file of the library and says whether that
;;;   succeeded.
;;; - `(invalidate-library! key)` forgets the library, so the next resolution
;;;   loads it again.
;;;
;;; A library that is being loaded is not loaded again by a cyclic import, and
;;; one that is defined is not loaded again either, unless it is invalidated.
;;;

(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations)
  (boot raise) (boot exceptions) (boot environments) (boot linker) (boot units))
(|#%provide| all-defined)

;;; Load extensions

(define capy:r7rs-load-extensions '())
(define capy:r6rs-load-extensions '())
(define capy:compat-load-extensions '())
(define capy:all-mode-load-extensions '())

(let* ([host-arch (host-arch)]
       [host-os (host-os)]
       [host-family (host-family)]
       [host-os-scm (string-append host-os ".scm")]
       [host-family-scm (string-append host-family ".scm")]
       [arch-scm (string-append host-arch ".scm")]
       [host-os-sld (string-append host-os ".sld")]
       [host-family-sld (string-append host-family ".sld")]
       [arch-sld (string-append host-arch ".sld")]
       [host-os-sls (string-append host-os ".sls")]
       [host-family-sls (string-append host-family ".sls")]
       [arch-sls (string-append host-arch ".sls")]
       [common (list host-os-scm host-family-scm arch-scm
                "capy.scm"
                "scm"
                "sch"
                "ss")]
       [r7rs (list host-os-sld host-family-sld arch-sld
              "capy.sld"
              "sld")]
       [r6rs (list host-os-sls host-family-sls arch-sls
              "capy.sls"
              "sls"
              "sps")])
  ; Every mode searches both families, with its own family first and the common
  ; extensions last. The mode still decides the syntax (numeric components,
  ; SRFI spelling); it does not hide a dependency that a mixed-language program
  ; legitimately imports.
  (set! capy:r7rs-load-extensions
    (append r7rs r6rs common))
  (set! capy:r6rs-load-extensions
    (append r6rs r7rs common))
  (set! capy:compat-load-extensions
    (append r7rs r6rs common))
  (set! capy:all-mode-load-extensions
    (append capy:r7rs-load-extensions capy:r6rs-load-extensions)))

;; The load extensions that apply in MODE.
(define (capy:mode-load-extensions mode)
  (case mode
    [(r7rs) capy:r7rs-load-extensions]
    [(r6rs) capy:r6rs-load-extensions]
    [(compat) capy:compat-load-extensions]
    [else
      (assertion-violation 'capy:execution-mode
        "invalid execution mode"
        mode)]))

;; Is EXT one of the extensions recognized in any mode?
(define (capy:mode-load-extension? ext)
  (member ext capy:all-mode-load-extensions))

;; Rebuild %load-extensions so the MODE-specific extensions come first,
;; keeping any extra user-added extensions behind them.
(define (capy:update-load-extensions! mode)
  (let ([extra (filter (lambda (ext)
                        (not (capy:mode-load-extension? ext)))
                %load-extensions)])
    (set! %load-extensions
      (append (capy:mode-load-extensions mode) extra))))

(define capy:execution-mode
  (let ([mode 'compat])
    (lambda args
      (if (null? args)
        mode
        (let ([old mode]
              [new-mode (car args)])
          (capy:mode-load-extensions new-mode)
          (set! mode new-mode)
          (capy:update-load-extensions! new-mode)
          old)))))

(capy:update-load-extensions! (capy:execution-mode))

(define (install-r7rs!)
  (capy:execution-mode 'r7rs))

(define (install-r6rs!)
  (capy:execution-mode 'r6rs))

;; Compatibility is the default: files declare their language with a `#!`
;; directive or their extension (see `capy:file-declared-language`), and
;; undeclared files get the permissive mode, which accepts both import styles
;; and searches both families of source extensions.
(define (install-compat!)
  (capy:execution-mode 'compat))

;;; Source language of a file

;; The language a file's contents are written in, from its extension, when it
;; has one. A .scm file is written in the ambient language and gives #f.
(define (capy:string-suffix? s suffix)
  (let ([n (string-length s)]
        [m (string-length suffix)])
    (and (>= n m) (string=? (substring s (- n m) n) suffix))))

(define (capy:file-extension-language path)
  (and (string? path)
    (cond
      [(or (capy:string-suffix? path ".sls")
           (capy:string-suffix? path ".sps")) 'r6rs]
      [(capy:string-suffix? path ".sld") 'r7rs]
      [else #f])))

;; IO and `load` are defined by later files; eval.scm installs them with `$set-library-services!`.
(define $lm-open-input-file #f)
(define $lm-read-char #f)
(define $lm-close-port #f)
(define $lm-load #f)
(define ($set-library-services! open-input-file read-char close-port load)
  (set! $lm-open-input-file open-input-file)
  (set! $lm-read-char read-char)
  (set! $lm-close-port close-port)
  (set! $lm-load load))

;; The first 4096 characters of PATH, or #f when it cannot be read. The
;; directives that select a language are at the top of the file, so this is
;; enough to see them.
(define (capy:read-prefix path)
  (capy:read-file-prefix path))

(define (capy:read-file-prefix path)
  (call/cc
    (lambda (escape)
      (with-exception-handler
        (lambda (cause) (escape #f))
        (lambda ()
          (let ([port ($lm-open-input-file path)])
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (let loop ([i 0] [acc '()])
                  (cond
                    [(= i 4096) (list->string (reverse acc))]
                    [else
                      (let ([c ($lm-read-char port)])
                        (if (eof-object? c)
                          (list->string (reverse acc))
                          (loop (+ i 1) (cons c acc))))])))
              (lambda () ($lm-close-port port)))))))))

(define (capy:char-at s i)
  (and (< i (string-length s)) (string-ref s i)))

(define (capy:skip-to-line-end s i)
  (let loop ([i i])
    (cond
      [(>= i (string-length s)) i]
      [(char=? (string-ref s i) #\newline) (+ i 1)]
      [else (loop (+ i 1))])))

(define (capy:skip-block-comment s i depth)
  (let loop ([i i] [depth depth])
    (cond
      [(>= i (string-length s)) i]
      [(zero? depth) i]
      [(and (char=? (string-ref s i) #\|)
            (eqv? (capy:char-at s (+ i 1)) #\#))
        (loop (+ i 2) (- depth 1))]
      [(and (char=? (string-ref s i) #\#)
            (eqv? (capy:char-at s (+ i 1)) #\|))
        (loop (+ i 2) (+ depth 1))]
      [else (loop (+ i 1) depth)])))

(define (capy:skip-space-and-comments s i)
  (let loop ([i i])
    (cond
      [(>= i (string-length s)) i]
      [(char-whitespace? (string-ref s i)) (loop (+ i 1))]
      [(char=? (string-ref s i) #\;)
        (loop (capy:skip-to-line-end s (+ i 1)))]
      [(and (char=? (string-ref s i) #\#)
            (eqv? (capy:char-at s (+ i 1)) #\|))
        (loop (capy:skip-block-comment s (+ i 2) 1))]
      [else i])))

;; The language a leading `#!r6rs` or `#!r7rs` directive declares, or #f. A
;; `#!/...` line is skipped, as are comments, so a script with a shebang and a
;; directive is understood.
(define (capy:directive-end s start)
  (let find ([j start])
    (if (or (>= j (string-length s))
            (char-whitespace? (string-ref s j))
            (char=? (string-ref s j) #\())
      j
      (find (+ j 1)))))

(define (capy:shebang-line? s start)
  (and (< start (string-length s))
    (let ([d (string-ref s start)])
      (or (char=? d #\/) (char=? d #\space)))))

(define (capy:file-declared-language path)
  (and (string? path)
    (let ([s (capy:read-prefix path)])
      (and s
        (let loop ([i (capy:skip-space-and-comments s 0)])
          (cond
            [(>= i (string-length s)) #f]
            [(not (and (char=? (string-ref s i) #\#)
                       (eqv? (capy:char-at s (+ i 1)) #\!)))
              #f]
            [else
              (let ([start (+ i 2)])
                (if (capy:shebang-line? s start)
                  ; a shebang line, not a language directive
                  (loop (capy:skip-to-line-end s start))
                  (let* ([end (capy:directive-end s start)]
                         [word (substring s start end)])
                    (cond
                      [(string=? word "r6rs") 'r6rs]
                      [(string=? word "r7rs") 'r7rs]
                      [else (loop (capy:skip-space-and-comments s end))]))))]))))))

;; Runs THUNK with PATH's language in effect. In the compatibility mode the
;; ambient mode is already permissive, so only the reader's lexical mode is
;; narrowed; with an explicitly pinned R6RS/R7RS mode the file's declaration
;; wins and the execution mode is switched too.
(define (capy:call-with-file-language path fallback thunk)
  (let* ([declared (capy:file-declared-language path)]
         [extension (capy:file-extension-language path)]
         [language (or declared extension fallback)]
         [mode (capy:execution-mode)]
         [saved-reader (%source-reader-mode)]
         [reader-mode (cond
                        ; the directive sets the reader mode itself
                        [declared 'capy]
                        [extension extension]
                        ; a .scm file keeps the permissive reader default
                        [else 'capy])]
         [switch? (and (not (eq? mode 'compat))
                       (not (eq? language mode)))])
    (dynamic-wind
      (lambda ()
        (%source-reader-mode reader-mode)
        (when switch? (capy:execution-mode language)))
      thunk
      (lambda ()
        (%source-reader-mode saved-reader)
        (when switch? (capy:execution-mode mode))))))

;;; Load state

;; The keys of the libraries whose files are being loaded, innermost first. A library that is
;; defined is known to the registry, so this is all the state there is: it breaks cyclic imports.
(define %libraries-loading '())

(define (library-loading? key)
  (and (member key %libraries-loading) #t))

;; The default resolver of a registry, how the linker finds a library that is not a boot file:
;; the libmap says where the compiled library is when it lists it, and otherwise it is found in
;; the load path (the libraries of the user, which are compiled when they have to be).
(define (default-library-resolver key)
  (or (linker-libmap-load! key)
    ($lm-load (linker-library-source key))))

(set-registry-resolver! (root-library-registry) default-library-resolver)

;; The most recent load failure as (parts . condition), or #f
(define %last-library-load-failure (make-parameter #f))

;;; Loading

;; Loads the file of the library KEY. Returns #t when the load succeeded. A
;; failed load is remembered in `%last-library-load-failure`. A library that is being loaded
;; is not loaded again, and the result is #f.
(define (load-library key)
  (let ([parts (linker-name->path-parts key)]
        [saved %libraries-loading]
        [saved-unit #f]
        [saved-sink #f])
    (and (not (library-loading? key))
      (let ([loaded? #f])
        (dynamic-wind
          ;; The library is not part of the unit being expanded when it is needed: loaded
          ;; from its source, its forms would be expanded in that unit and take names from
          ;; its counter, so the names in a compiled file would depend on which of its
          ;; imports the compiler had compiled.
          (lambda ()
            (set! %libraries-loading (cons key saved))
            (set! saved-unit (current-unit))
            (set! saved-sink (current-unit-sink))
            (current-unit #f)
            (current-unit-sink #f))
          (lambda ()
            (call/cc (lambda (escape)
                      (with-exception-handler
                        (lambda (cause)
                          (%last-library-load-failure (cons parts cause))
                          (escape #f))
                        (lambda ()
                          (linker-require! key)
                          (set! loaded? #t))))))
          (lambda ()
            (set! %libraries-loading saved)
            (current-unit saved-unit)
            (current-unit-sink saved-sink)))
        loaded?))))

;; Whether the library KEY is defined
(define (library-defined? key)
  (env-registered-defined? key))

;; Why the last load of a library failed, as " [reason]" for an error message, or "" when
;; nothing is known. The failure is forgotten.
(define (take-library-load-failure-reason)
  (let* ([failure (%last-library-load-failure)]
         [reason (if (and failure (message-condition? (cdr failure)))
                   ($boot-format #f " [~a]" (condition-message (cdr failure)))
                   "")])
    (%last-library-load-failure #f)
    reason))

;; The environment of the library KEY. The library is loaded when it is not
;; defined yet. Raises an error when there is no such library, with the reason the
;; load failed when there is one. A library that is being loaded by a cyclic import
;; gives its environment as it is so far.
(define (resolve-library key)
  (cond
    [(library-defined? key) (ensure-env key)]
    [(library-loading? key) (ensure-env key)]
    [else
      (load-library key)
      (if (library-defined? key)
        (ensure-env key)
        (error 'resolve-library
          ($boot-format #f "no library ~a~a" (library-key->string key) (take-library-load-failure-reason))
          key))]))

;; The decl of the library KEY read from its compiled file, which is not run, or #f when the
;; library has no compiled file or the file has no decl for it. The expander uses it when the
;; library cannot be loaded (it fails to run, say), to go on with the names the library exports.
;; The compiled file is the one the libmap lists, or the one the compiler left next to the
;; source of the library (it may be the file of an earlier version of the source).
(define (library-decl-without-loading key)
  (define (decl-of path)
    (and path (file-exists? path)
      (let ([decls (fasl-library-decls path)])
        (and decls (find-library-decl decls key)))))
  (call/cc
    (lambda (return)
      (with-exception-handler
        (lambda (c) (return #f))
        (lambda ()
          (or (decl-of (linker-libmap-fasl key))
            (let ([found (try-load-thunk-in-vicinity (linker-library-source key) #t)])
              (and (pair? found)
                (let loop ([candidates (list-ref found 3)])
                  (and (pair? candidates)
                    (or (decl-of (car candidates)) (loop (cdr candidates)))))))))))))

;; A library key as people write it: the name of a library of `core` or `std`, and
;; `(json) from github.com/x/json@1` for one of any other package.
(define (library-key->string key)
  (if (linker-package-key? key)
    ($boot-format #f "~a from ~a" (linker-key-name key) (linker-key-package key))
    ($boot-format #f "~a" key)))

;; "; did you mean (import ...)?" for the library NAME, or "".
(define (library-suggestion-suffix name)
  (let ([hint (import-suggestion-string 'module name)])
    (if hint (string-append "; " hint) "")))

;; The message of an import of a library NAME that the current view does not see.
(define (library-not-visible-message view name)
  ($boot-format #f "no library ~a is visible from package ~a~a"
    name (linker-view-package view)
    (library-suggestion-suffix name)))

;; Whether the libmap lists a library of the package PACKAGE.
(define (libmap-package-built? package)
  (let loop ([keys (linker-libmap-keys)])
    (and (pair? keys)
      (or (and (linker-package-key? (car keys)) (eq? (linker-key-package (car keys)) package))
        (loop (cdr keys))))))

;; The message for a library KEY that was not found. With a view current, the name the view gives
;; the library, and the package that imports it, are part of it.
(define (library-missing-message key view)
  (let ([imported-as (if view
                       (let ([names (linker-view-names view key)])
                         (if (pair? names)
                           ($boot-format #f ", imported as ~a in package ~a" (car names)
                             (linker-view-package view))
                           ""))
                       "")])
    (if (linker-package-key? key)
      (if (libmap-package-built? (linker-key-package key))
        ($boot-format #f "no library ~a~a: the package has no such library~a"
          (library-key->string key) imported-as
          (library-suggestion-suffix (linker-key-name key)))
        ($boot-format #f "no library ~a~a: it is not in the libmap, so its package has to be built first~a"
          (library-key->string key) imported-as
          (library-suggestion-suffix (linker-key-name key))))
      ($boot-format #f "no library ~a (searched: ~a)~a"
        key (join-search-directories %load-path)
        (library-suggestion-suffix key)))))

;; The message of an import of the library KEY that has no environment. When the
;; file of the library is missing, the directories searched are listed. Otherwise
;; the file was found but failed to load, and the reason is given. The failure is
;; forgotten. KEY may be a name no library can have, one the current view does not see.
(define (library-not-found-message key)
  (let* ([failure (%last-library-load-failure)]
         [cause (and failure (cdr failure))]
         [view (current-library-view)]
         [missing? (or (not (and cause (message-condition? cause)))
                     (string=? (condition-message cause)
                       (string-append "File not found: " (linker-name->path key))))])
    (%last-library-load-failure #f)
    (cond
      [(and view (not (linker-view-key view key)))
        (library-not-visible-message view key)]
      [missing? (library-missing-message key view)]
      [else
        ($boot-format #f "no library ~a [~a]" (library-key->string key)
          (condition-message cause))])))

(define (join-search-directories dirs)
  (if (null? dirs)
    "none"
    (let loop ([dirs (cdr dirs)] [out (car dirs)])
      (if (null? dirs)
        out
        (loop (cdr dirs) (string-append out ", " (car dirs)))))))

;; Forgets the library KEY: its environment and its instance, so that the next resolution
;; loads it again. Returns whether there was something to forget.
(define (invalidate-library! key)
  (core-hash-remove! (lazy-envs) key)
  (let ([env (lookup-env key)])
    (unregister-library-instance! (current-library-registry) key)
    (when env
      (core-hash-remove! (env-registry) key))
    (and env #t)))

($set-library-manager-hooks! capy:execution-mode load-library library-not-found-message)
