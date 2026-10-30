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
  (set! capy:r7rs-load-extensions
    (append r7rs common))
  (set! capy:r6rs-load-extensions
    (append r6rs common))
  (set! capy:all-mode-load-extensions
    (append capy:r7rs-load-extensions capy:r6rs-load-extensions)))

;; The load extensions that apply in MODE.
(define (capy:mode-load-extensions mode)
  (case mode
    [(r7rs) capy:r7rs-load-extensions]
    [(r6rs) capy:r6rs-load-extensions]
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
  (let ([mode 'r7rs])
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
