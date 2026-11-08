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
;;; one that was loaded once is not loaded again either, unless it is invalidated.
;;;

(|#%require| (boot prim) (boot control) (boot environments)
  (late (boot raise) error)
  (late (boot conditions) message-condition? condition-message)
  (late (boot exceptions) with-exception-handler)
  (late (boot port-api) open-input-file read-char)
  (late (boot ports-core) close-port)
  (late (boot eval) load)
  (late (boot format) format))
(|#%provide| all-defined)

;;; The unit runner

;; Runs a compilation unit thunk (see `$boot-run-unit-thunk` in lib/boot/main.scm, which
;; installs the runner). It lives here so the boot linklets that load files can
;; refer to it.
(define $unit-runner #f)
(define ($set-unit-runner! proc) (set! $unit-runner proc))
(define ($run-unit-thunk thunk . maybe-ns)
  (apply $unit-runner thunk maybe-ns))

;;; Library names and files


(define (library-name-part->path-string part)
  (cond
    [(symbol? part) (symbol->string part)]
    [(and (exact-integer? part) (not (negative? part)))
      (number->string part)]
    [else
      (assertion-violation 'library-name-part->path-string
        "invalid library name component"
        part)]))

(define (library-name->load-parts key)
  (let* ([reverse-name (reverse key)]
         [file (library-name-part->path-string (car reverse-name))]
         [dir-name (reverse (cdr reverse-name))]
         [dir (apply string-append
               (map (lambda (elt)
                     (string-append (library-name-part->path-string elt) "/"))
                 dir-name))])
    (cons dir file)))

(define (library-name->path key)
  (let ([parts (library-name->load-parts key)])
    (string-append (car parts) (cdr parts))))

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

;; The first 4096 characters of PATH, or #f when it cannot be read. The
;; directives that select a language are at the top of the file, so this is
;; enough to see them.
(define (capy:read-prefix path)
  (call/cc
    (lambda (escape)
      (with-exception-handler
        (lambda (cause) (escape #f))
        (lambda ()
          (let ([port (open-input-file path)])
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (let loop ([i 0] [acc '()])
                  (cond
                    [(= i 4096) (list->string (reverse acc))]
                    [else
                      (let ([c (read-char port)])
                        (if (eof-object? c)
                          (list->string (reverse acc))
                          (loop (+ i 1) (cons c acc))))])))
              (lambda () (close-port port)))))))))

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

;;; SRFI spellings

(define (capy:digits? s)
  (and (> (string-length s) 0)
    (let loop ([i 0])
      (or (= i (string-length s))
          (and (char-numeric? (string-ref s i)) (loop (+ i 1)))))))

;; The key of the same SRFI written with the other spelling: (srfi 64) and
;; (srfi :64), or #f when KEY is not a SRFI key. Suffix components are kept,
;; so (srfi 64 testing) alternates with (srfi :64 testing).
(define (capy:srfi-alternate-key key)
  (and (pair? key)
       (eq? (car key) 'srfi)
       (pair? (cdr key))
    (let* ([part (cadr key)]
           [tail (cddr key)]
           [alt
             (cond
               [(and (exact-integer? part) (not (negative? part)))
                 (string->symbol (string-append ":" (number->string part)))]
               [(symbol? part)
                 (let* ([s (symbol->string part)]
                        [n (string-length s)])
                   (and (> n 1)
                        (char=? (string-ref s 0) #\:)
                        (capy:digits? (substring s 1 n))
                        (string->number (substring s 1 n))))]
               [else #f])])
      (and alt (cons 'srfi (cons alt tail))))))

;; In the compatibility mode, a SRFI is looked up under the spelling whose file
;; exists, so a library that ships only one spelling works with either. In the
;; strict modes the key is left alone (R6RS has already rewritten it).
(define (capy:compat-library-key key)
  (if (not (eq? (capy:execution-mode) 'compat))
    key
    (let ([alt (capy:srfi-alternate-key key)])
      (cond
        [(not alt) key]
        [(%search-load-path (library-name->path key)) key]
        [(%search-load-path (library-name->path alt)) alt]
        [else key]))))

;;; Load state

;; The load state of the registry REG: a vector of the (dir . file) parts of the libraries
;; that are being loaded, and of those that were loaded.
(define (registry-load-state reg)
  (or (registry-loading reg)
    (let ([state (vector '() '())])
      (set-registry-loading! reg state)
      state)))

(define (library-load-state key)
  (let ([parts (library-name->load-parts key)]
        [state (registry-load-state (current-library-registry))])
    (cond
      [(member parts (vector-ref state 0)) 'in-progress]
      [(member parts (vector-ref state 1)) 'done]
      [else #f])))

(define (set-library-load-state! reg key state)
  (let ([parts (library-name->load-parts key)]
        [loading (registry-load-state reg)])
    (vector-set! loading 0 (delete! parts (vector-ref loading 0)))
    (vector-set! loading 1 (delete! parts (vector-ref loading 1)))
    (case state
      [(in-progress) (vector-set! loading 0 (cons parts (vector-ref loading 0)))]
      [(done) (vector-set! loading 1 (cons parts (vector-ref loading 1)))])))

;; Forgets that the library KEY was loaded, so the next attempt loads it again.
(define (forget-library-load! key)
  (set-library-load-state! (current-library-registry) key #f))

(define (load-path-library-resolver key)
  (load (library-name->path key)))

(set-registry-resolver! (root-library-registry) load-path-library-resolver)

;; The most recent load failure as (parts . condition), or #f
(define %last-library-load-failure (make-parameter #f))

;;; Loading

;; Loads the file of the library KEY. Returns #t when the load succeeded. A
;; failed load is remembered in `%last-library-load-failure` and the load state of the
;; library is dropped, so that it can be tried again. A library that is being
;; loaded or was loaded is not loaded again, and the result is #f.
(define (load-library key)
  (let* ([parts (library-name->load-parts key)]
         [reg (current-library-registry)])
    (and (not (library-load-state key))
      (let ([loaded? #f])
        (dynamic-wind
          (lambda () (set-library-load-state! reg key 'in-progress))
          (lambda ()
            (call/cc (lambda (escape)
                      (with-exception-handler
                        (lambda (cause)
                          (%last-library-load-failure (cons parts cause))
                          (escape #f))
                        (lambda ()
                          (let ([resolver (registry-resolver reg)])
                            (if resolver
                              (resolver key)
                              (error 'load-library "library not available" key)))
                          ; A file that ran and registered no instance is not the library
                          (if (not (library-instance reg key))
                            (error 'load-library
                              (format #f "~a ran but registered no instance of ~a"
                                (library-name->path key) key)
                              key))
                          (set! loaded? #t))))))
          (lambda () (set-library-load-state! reg key (and loaded? 'done))))
        loaded?))))

;; Whether the library KEY is defined
(define (library-defined? key)
  (env-registered-defined? key))

;; Loads the library KEY when it is not defined yet, without building its environment.
(define (ensure-library-loaded! key)
  (if (and (not (library-defined? key))
        (not (eq? (library-load-state key) 'in-progress)))
    (load-library key)))

;; Why the last load of a library failed, as " [reason]" for an error message, or "" when
;; nothing is known. The failure is forgotten.
(define (take-library-load-failure-reason)
  (let* ([failure (%last-library-load-failure)]
         [reason (if (and failure (message-condition? (cdr failure)))
                   (format #f " [~a]" (condition-message (cdr failure)))
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
    [(eq? (library-load-state key) 'in-progress) (ensure-env key)]
    [else
      (load-library key)
      (if (library-defined? key)
        (ensure-env key)
        (error 'resolve-library
          (format #f "no library ~a~a" key (take-library-load-failure-reason))
          key))]))

;; The message of an import of the library KEY that has no environment. When the
;; file of the library is missing, the directories searched are listed. Otherwise
;; the file was found but failed to load, and the reason is given. The failure is
;; forgotten.
(define (library-not-found-message key)
  (let* ([failure (%last-library-load-failure)]
         [cause (and failure (cdr failure))]
         [missing? (or (not (and cause (message-condition? cause)))
                     (string=? (condition-message cause)
                       (string-append "File not found: " (library-name->path key))))])
    (%last-library-load-failure #f)
    (if missing?
      (format #f "no library ~a (searched: ~a)" key
        (join-search-directories %load-path))
      (format #f "no library ~a [~a]" key (condition-message cause)))))

(define (join-search-directories dirs)
  (if (null? dirs)
    "none"
    (let loop ([dirs (cdr dirs)] [out (car dirs)])
      (if (null? dirs)
        out
        (loop (cdr dirs) (string-append out ", " (car dirs)))))))

;; Forgets the library KEY: its environment, its instance and the state of its
;; load, so that the next resolution loads it again. Returns whether there was
;; something to forget.
(define (invalidate-library! key)
  (core-hash-remove! (lazy-envs) key)
  (let ([env (lookup-env key)])
    (forget-library-load! key)
    (when env
      (core-hash-remove! (env-registry) key))
    (and env #t)))
