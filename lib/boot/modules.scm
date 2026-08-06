;;; -*- mode: scheme; coding: utf-8; -*-
;;; The module system: the registry of named modules, binding lookup,
;;; use lists and public interfaces, and on-demand loading of module
;;; files.
;;;
;;; This file is loaded very early in the boot sequence, right after the
;;; control primitives, so everything here is written against raw runtime
;;; primitives (`make-module`, `module-obarray`, `core-hash-*`,
;;; `variable-*`, ...) plus a few helpers defined earlier in boot.




;; Read the value bound to NAME in MODULE, searching its use list. A
;; missing or unbound variable raises an error unless REST supplies a
;; fallback value.
(define (module-ref module name . rest)
  (let ([var (module-variable module name)])
    (if (and var (variable-bound? var))
      (variable-ref var)
      (if (null? rest)
        (assertion-violation 'module-ref "unbound variable" module name)
        (car rest)))))

;; Set the value bound to NAME in MODULE, searching its use list.
(define (module-set! module name value)
  (let ([var (module-variable module name)])
    (if var
      (variable-set! var value)
      (assertion-violation 'module-set! "unbound variable" module name))))

;; Is NAME bound to a value in MODULE or its use list?
(define (module-defined? module name)
  (let ([var (module-variable module name)])
    (and var (variable-bound? var))))

;; Define NAME in MODULE itself, keeping the existing variable when there
;; is one (so rebinding does not change the identity of the variable).
(define (module-define! module name value)
  (let ([var (module-local-variable module name)])
    (if var
      (variable-set! var value)
      (module-add! module name (make-variable value)))))

;; Apply FN to MODULE and then to each of its uses, returning the first
;; non-#f result.
(define (module-search fn m v)
  (or (fn m v)
    (let scan ([rest (module-uses m)])
      (if (null? rest)
        #f
        (or (fn (car rest) v)
          (scan (cdr rest)))))))

;; Call PROC for every binding in MODULE's own obarray.
(define (module-for-each proc module)
  (for-each (lambda (kv) (proc (car kv) (cdr kv)))
    (core-hash->list (module-obarray module))))

;; Map PROC over the bindings in MODULE's own obarray.
(define (module-map proc module)
  (map proc (core-hash->list (module-obarray module))))

;; Run THUNK as a "module excursion": the module that was current when
;; this was called is restored once THUNK returns. If a continuation
;; captured inside THUNK later re-enters its dynamic extent, the current
;; module is restored to the one THUNK had when it last exited, so nested
;; and re-entrant excursions compose.
(define (save-module-excursion thunk)
  (let ([active (current-module)]
        [saved #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-module))
        (current-module active)
        (set! active #f))
      thunk
      (lambda ()
        (set! active (current-module))
        (current-module saved)
        (set! saved #f)))))

;;; ---------------------------------------------------------------------------
;;; 2. Module trees and name paths
;;; ---------------------------------------------------------------------------

;; A module's submodules live in a hash keyed by name symbol.
(define (module-ref-submodule module name)
  (core-hash-ref (module-submodules module) name))

(define (module-define-submodule! module name submodule)
  (core-hash-put! (module-submodules module) name submodule))

;; Walk the dotted path NAMES through the submodule tree rooted at ROOT.
;; The helper used by the read-only walkers; missing intermediate modules
;; stop the walk with #f.
(define (nested-ref-module root names)
  (let descend ([cur root] [names names])
    (if (null? names)
      cur
      (let ([next (module-ref-submodule cur (car names))])
        (and next (descend next (cdr names)))))))

;; The value bound to NAMES (a symbol or a dotted path) in ROOT, or #f
;; when any part of the path is missing.
(define (nested-ref root names)
  (if (null? names)
    root
    (let descend ([cur root] [head (car names)] [tail (cdr names)])
      (if (null? tail)
        (module-ref cur head #f)
        (let ([next (module-ref-submodule cur head)])
          (and next (descend next (car tail) (cdr tail))))))))

;; Set the value bound to NAMES in ROOT. Every intermediate module must
;; already exist.
(define (nested-set! root names val)
  (let descend ([cur root] [head (car names)] [tail (cdr names)])
    (if (null? tail)
      (module-set! cur head val)
      (let ([next (module-ref-submodule cur head)])
        (if (not next)
          (assertion-violation 'nested-set! "failed to resolve module" names)
          (descend next (car tail) (cdr tail)))))))

;; Remove the binding NAMES from ROOT. Every intermediate module must
;; already exist.
(define (nested-remove! root names)
  (let descend ([cur root] [head (car names)] [tail (cdr names)])
    (if (null? tail)
      (module-remove! cur head)
      (let ([next (module-ref-submodule cur head)])
        (if (not next)
          (assertion-violation 'nested-remove! "failed to resolve module" names)
          (descend next (car tail) (cdr tail)))))))

;; Define the binding NAMES in ROOT, creating a fresh variable when the
;; name is not yet bound.  Every intermediate module must already exist.
(define (nested-define! root names val)
  (let descend ([cur root] [head (car names)] [tail (cdr names)])
    (if (null? tail)
      (module-define! cur head val)
      (let ([next (module-ref-submodule cur head)])
        (if (not next)
          (assertion-violation 'nested-define! "failed to resolve module" names)
          (descend next (car tail) (cdr tail)))))))

;; Remove the submodule NAMES from ROOT. Every intermediate module must
;; already exist.
(define (nested-remove-module! root names)
  (let descend ([cur root] [head (car names)] [tail (cdr names)])
    (if (null? tail)
      (core-hash-remove! (module-submodules cur) head)
      (let ([next (module-ref-submodule cur head)])
        (if (not next)
          (assertion-violation 'nested-remove-module! "failed to resolve module" names)
          (descend next (car tail) (cdr tail)))))))

;; Register MODULE under the path NAMES in ROOT, creating intermediate
;; `directory` modules on the way.
(define (nested-define-module! root names module)
  (if (null? names)
    (assertion-violation 'nested-define-module! "can't redefine root module" module)
    (let descend ([cur root] [head (car names)] [tail (cdr names)])
      (if (null? tail)
        (module-define-submodule! cur head module)
        (let ([next (or (module-ref-submodule cur head)
                     (let ([dir (make-module)])
                       (set-module-kind! dir 'directory)
                       (set-module-name! dir (append (module-name cur) (list head)))
                       (module-define-submodule! cur head dir)
                       dir))])
          (descend next (car tail) (cdr tail)))))))

;; The variants of the nested-* operations that work on the current module.
(define (local-ref names)
  (nested-ref (current-module) names))

(define (local-set! names val)
  (nested-set! (current-module) names val))

(define (local-define names val)
  (nested-define! (current-module) names val))

(define (local-remove names)
  (nested-remove! (current-module) names))

(define (local-ref-module names)
  (nested-ref-module (current-module) names))

(define (local-define-module names mod)
  (nested-define-module! (current-module) names mod))


;; Add INTERFACE to MODULE's use list. The interface is not added twice,
;; and adding one invalidates the import cache.
(define (module-use! module interface)
  (if (not (or (eq? module interface)
            (memq interface (module-uses module))))
    (begin
      (set-module-uses! module (cons interface (module-uses module)))
      (core-hash-clear! (module-import-obarray module)))))

;; Add every interface in INTERFACES to MODULE's use list. Interfaces
;; already used are skipped, and an interface that only re-provides
;; bindings already visible through MODULE's current uses is trimmed to
;; its genuinely new bindings (or dropped when there are none), so a
;; later use can never silently shadow an earlier one.
(define (module-use-interfaces! module interfaces)
  ;; The variable bound to SYM by the first interface in IFACES that
  ;; provides one.
  (define (find-provider ifaces sym)
    (let scan ([rest ifaces])
      (cond
        [(null? rest) #f]
        [(module-variable (car rest) sym)]
        [else (scan (cdr rest))])))

  ;; The subset of a use list that actually supplies bindings: the
  ;; implicit core module is present in every module and is not counted
  ;; as a real use.
  (define (without-core uses)
    (let scan ([rest uses] [out '()])
      (cond
        [(null? rest) (reverse out)]
        [(eq? (car rest) the-scm-module) (scan (cdr rest) out)]
        [else (scan (cdr rest) (cons (car rest) out))])))

  ;; A copy of IFACE holding only the bindings that PRIOR does not
  ;; already provide, or #f if IFACE contributes nothing new. A binding
  ;; that PRIOR already provides as the identical variable is left out
  ;; (and marks the copy as needed); a same-named but different variable
  ;; from PRIOR still wins and is kept.
  (define (trim-against iface prior)
    (let ([trimmed #f]
          [overlapped? #f]
          [fresh? #t])
      (define (ensure-trimmed!)
        (or trimmed
          (let ([copy (make-module)])
            (set-module-name! copy (module-name iface))
            (set-module-kind! copy 'custom-interface)
            (set! trimmed copy)
            copy)))
      (module-for-each
        (lambda (sym var)
          (let ([existing (find-provider prior sym)])
            (cond
              [(not existing)
                (set! fresh? #f)
                (module-add! (ensure-trimmed!) sym var)]
              [(eq? existing var)
                (set! overlapped? #t)]
              [else
                (set! fresh? #f)
                (module-add! (ensure-trimmed!) sym var)])))
        iface)
      (cond
        [fresh? #f]
        [overlapped? trimmed]
        [else iface])))

  (let* ([cur (module-uses module)]
         [cur-without-core (without-core cur)]
         [new (let collect ([in interfaces]
                            [accepted cur]
                            [accepted-explicit cur-without-core]
                            [out '()])
               (if (null? in)
                 (reverse out)
                 (let ([iface (car in)])
                   (if (or (memq iface accepted) (memq iface out))
                     (collect (cdr in) accepted accepted-explicit out)
                     (let ([trimmed (trim-against iface accepted-explicit)])
                       (if trimmed
                         (collect (cdr in)
                           (cons trimmed accepted)
                           (cons trimmed accepted-explicit)
                           (cons trimmed out))
                         (collect (cdr in) accepted accepted-explicit out)))))))])
    (set-module-uses! module (append new cur))
    (core-hash-clear! (module-import-obarray module))))

;; The name of MOD as a list of symbols, synthesizing one (and registering
;; the module under it in the registry root) when MOD was created without
;; a name.
(define (module-name mod)
  (or (raw-module-name mod)
    (let ([name (list (gensym))])
      (set-module-name! mod name)
      (nested-define-module! (resolve-module '() #f #t) name mod)
      (raw-module-name mod))))

;; Find the module at path NAME inside MODULE, creating the missing
;; modules (all `directory` kind) on the way.
(define (make-modules-in module name)
  (or (nested-ref-module module name)
    (let ([m (make-module)])
      (set-module-kind! m 'directory)
      (set-module-name! m (append (module-name module) name))
      (nested-define-module! module name m)
      m)))

;; Make MODULE fit to be a user module: it gets its own public interface
;; and sees the implicit core module.
(define (beautify-user-module! module)
  (let ([interface (module-public-interface module)])
    (if (or (not interface)
         (eq? interface module))
      (let ([interface (make-module)])
        (set-module-name! interface (module-name module))
        (set-module-kind! interface 'interface)
        (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
       (not (eq? module the-root-module)))
    (module-use! module the-scm-module)))

;; A brand-new, fully initialized user module.
(define (make-fresh-user-module)
  (let ([m (make-module)])
    (beautify-user-module! m)
    (set-module-declarative! m #f)
    m))

;; Remove the implicit core module from MODULE's use list, leaving any
;; explicitly added interfaces in place.
(define (purify-module! module)
  (let ([uses (module-uses module)])
    (if (and (pair? uses)
         (eq? (car (last-pair uses)) the-scm-module))
      (set-module-uses! module (reverse (cdr (reverse uses)))))))


;; Render one component of a module name as a path string. Symbols become
;; their names; non-negative integers are allowed (e.g. SRFI numbers) and
;; become their decimal representation.
(define (module-name-part->path-string part)
  (cond
    [(symbol? part) (symbol->string part)]
    [(and (exact-integer? part) (not (negative? part)))
      (number->string part)]
    [else
      (assertion-violation 'module-name-part->path-string
        "invalid module name component"
        part)]))

;; Resolve NAME against the registry root. AUTOLOAD? asks for on-demand
;; loading when the module is not registered yet; ENSURE? makes a
;; skeleton module rather than returning #f when nothing is found.
(define resolve-module
  (let ([root *resolve-module-root*])
    (lambda (name autoload? ensure?)
      (let ([already (nested-ref-module root name)])
        (if (and already
             (or (not autoload?) (module-public-interface already)))
          already
          (if autoload?
            (begin
              (try-module-autoload name)
              (resolve-module name #f ensure?))
            (or already
              (and ensure? (make-modules-in root name)))))))))

(define (->bool x) (not (not x)))

;; Bookkeeping for autoloading: which (directory . file) pairs have
;; already been loaded, and which are currently being loaded.
(define autoloads-in-progress '())
(define autoloads-done '((capy . capy)))

(define (autoload-done-or-in-progress? p m)
  (let ([key (cons p m)])
    (->bool (or (member key autoloads-done)
             (member key autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ([key (cons p m)])
    (set! autoloads-in-progress
      (delete! key autoloads-in-progress))
    (or (member key autoloads-done)
      (set! autoloads-done (cons key autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ([key (cons p m)])
    (set! autoloads-done
      (delete! key autoloads-done))
    (set! autoloads-in-progress (cons key autoloads-in-progress))))

;; Record the outcome of an autoload attempt, or drop the entry entirely
;; when DONE? is #f.
(define (set-autoloaded! p m done?)
  (if done?
    (autoload-done! p m)
    (let ([key (cons p m)])
      (set! autoloads-done (delete! key autoloads-done))
      (set! autoloads-in-progress (delete! key autoloads-in-progress)))))

;; Split MODULE-NAME into the directory part and the leaf, as strings:
;; (dir . file), where DIR keeps its trailing "/" and is "" for a
;; one-element name. This is the key used by the autoload bookkeeping.
(define (module-name->load-parts module-name)
  (let* ([reverse-name (reverse module-name)]
         [file (module-name-part->path-string (car reverse-name))]
         [dir-name (reverse (cdr reverse-name))]
         [dir (apply string-append
               (map (lambda (elt)
                     (string-append (module-name-part->path-string elt) "/"))
                 dir-name))])
    (cons dir file)))

;; Forget that the module NAME was autoloaded, so the next attempt loads
;; it again.
(define (clear-module-autoload! module-name)
  (let ([key (module-name->load-parts module-name)])
    (set! autoloads-done (delete! key autoloads-done))
    (set! autoloads-in-progress (delete! key autoloads-in-progress))))

;; Drop the module NAME (and its interface) from the registry, clearing
;; caches and autoload state so it can be re-created from scratch.
(define (invalidate-module! module-name)
  (let* ([root *resolve-module-root*]
         [module (nested-ref-module root module-name)])
    (when module
      (core-hash-clear! (module-import-obarray module))
      (let ([interface (module-public-interface module)])
        (when interface
          (core-hash-clear! (module-obarray interface))
          (core-hash-clear! (module-import-obarray interface)))))
    (clear-module-autoload! module-name)
    (when (and module (pair? module-name))
      (nested-remove-module! root module-name))
    (not (not module))))

;; Records the most recent autoload failure as (dir . condition), so that
;; resolve-interface can report the underlying cause alongside its own
;; error. Cleared after use.
(define %last-autoload-failure (make-parameter #f))

;; Try to load the file backing MODULE-NAME. Returns #t when the load
;; succeeded; a failed attempt is remembered via %last-autoload-failure
;; and the module's autoload state is dropped.
(define (try-module-autoload module-name)
  (let* ([parts (module-name->load-parts module-name)]
         [dir (car parts)]
         [file (cdr parts)]
         [dir-module-name (reverse (cdr (reverse module-name)))])
    ;; Make sure the directory modules leading up to this one exist, so
    ;; the loaded module can register as a submodule of them.
    (resolve-module dir-module-name #f #t)

    (and (not (autoload-done-or-in-progress? dir file))
      (let ([loaded? #f])
        (dynamic-wind
          (lambda () (autoload-in-progress! dir file))
          (lambda ()
            (save-module-excursion
              (lambda ()
                (current-module (make-fresh-user-module))
                (call/cc (lambda (escape)
                          (with-exception-handler
                            (lambda (cause)
                              (%last-autoload-failure (cons (cons dir file) cause))
                              (escape #f))
                            (lambda ()
                              (load (string-append dir file))
                              (set! loaded? #t))))))))
          (lambda () (set-autoloaded! dir file loaded?)))
        loaded?))))

;; Hook installed by `(core suggest)` to append "did you mean" hints to
;; import-related error messages.  Called as (hook kind . args) with kinds:
;;   (module name)     - unknown module name -> "did you mean (import ...)?"
;;   (binding sym mod) - binding not in module -> "sym is exported by ..."
;; Returns a suggestion string (without leading punctuation) or #f.
(define %import-suggestion-hook (make-parameter #f))

(define (import-suggestion-string kind . args)
  (let ([hook (%import-suggestion-hook)])
    (if hook (apply hook kind args) #f)))

;; Setter for the suggestion hook, callable from other libraries (the
;; %-prefixed parameter itself is awkward to reference cross-module).
(define (install-import-suggestion-hook! hook)
  (%import-suggestion-hook hook))


(define (identity x) x)

;; Return the public interface of the module NAME, optionally restricted
;; by SELECT (a list of (orig . seen) specs), HIDE (names to leave out),
;; and PREFIX (a symbol prepended to every exported name).
(define (resolve-interface name select hide prefix)
  (let* ([mod (resolve-module name #t #f)]
         [public-i (and mod (module-public-interface mod))]
         [renamer (if prefix (lambda (sym) (symbol-append prefix sym)) identity)])
    (if (not public-i)
      (let* ([hint (import-suggestion-string 'module name)]
             [failure (%last-autoload-failure)]
             [failure-text (if (and failure (message-condition? (cdr failure)))
                             (format #f " [~a]" (condition-message (cdr failure)))
                             "")])
        (%last-autoload-failure #f)
        (assertion-violation 'resolve-interface
          (format #f "no code for module ~a~a~a"
            name failure-text
            (if hint (string-append "; " hint) ""))
          name)))

    (if (and (not select) (null? hide) (eq? renamer identity))
      public-i
      (let ([custom-i (make-module)])
        ;; Copy the binding VAR (exported as SRC under the name DST) into
        ;; the custom interface, unless SRC is hidden; replacement marks
        ;; are carried over from the original public interface.
        (define (export-binding! src dst var)
          (if (not (memq src hide))
            (begin
              (let ([renamed (renamer dst)])
                (if (core-hash-ref (module-replacements public-i) src)
                  (core-hash-put! (module-replacements custom-i) renamed #t))
                (module-add! custom-i renamed var)))))
        (set-module-kind! custom-i 'custom-interface)
        (set-module-name! custom-i name)
        (for-each (lambda (binding)
                   (if (not (module-local-variable public-i binding))
                     (assertion-violation #f "no binding to hide in module" name binding)))
          hide)

        (cond
          [select
            (for-each (lambda (bspec)
                       (let* ([direct? (symbol? bspec)]
                              [orig (if direct? bspec (car bspec))]
                              [seen (if direct? bspec (cdr bspec))]
                              [var (module-local-variable public-i orig)])
                         (if (not var)
                           (let ([hint (import-suggestion-string 'binding orig name)])
                             (assertion-violation 'unbound-variable
                               (if hint
                                 (format #f "no binding to select in module ~a; ~a" name hint)
                                 "no binding to select in module")
                               orig name)))
                         (export-binding! orig seen var)))
              select)]
          [else (module-for-each (lambda (sym var)
                                  (export-binding! sym sym var))
                 public-i)])
        custom-i))))

;; Enter (creating if needed) the module NAME and return it. A pure
;; module (XPURE? non-empty) has the implicit core module removed from
;; its use list.
(define (define-module* name . xpure?)
  (let ([pure? (if (null? xpure?) #f (car xpure?))])
    (let ([module (resolve-module name #f #t)])
      (beautify-user-module! module)
      (if pure?
        (purify-module! module))
      module)))

;; Export NAMES from MODULE through its public interface. A name may be
;; given as (internal . external); REPLACE? marks the exports as
;; replacements that override bindings from used interfaces.
(define (module-export! m names . replace?)
  (let ([replace? (if (null? replace?) #f (car replace?))]
        [public-i (module-public-interface m)])
    (for-each (lambda (name)
               (let* ([internal-name (if (pair? name) (car name) name)]
                      [external-name (if (pair? name) (cdr name) name)]
                      [var (module-ensure-local-variable! m internal-name)])
                 (if replace?
                   (core-hash-put! (module-replacements public-i) external-name #t))
                 (module-add! public-i external-name var)))
      names)))

;; Export NAMES from MODULE, marking them as replacements.
(define (module-replace! m names)
  (module-export! m names #t))

;; Export every binding of MODULE (including imported ones) through its
;; public interface.
(define (module-export-all! mod)
  (define (make-export-interface!)
    (let ([iface (make-module)])
      (set-module-name! iface (module-name mod))
      (set-module-version! iface (module-version mod))
      (set-module-kind! iface 'interface)
      (set-module-public-interface! mod iface)
      iface))
  (let ([iface (or (module-public-interface mod)
                (make-export-interface!))])
    (set-module-obarray! iface (module-obarray mod))))

;; Resolve and add each interface described by MODULE-IFACE-ARGS to the
;; current module's use list.
(define (process-use-modules module-iface-args)
  (let ([interfaces (map (lambda (mif-args)
                          (or (apply resolve-interface mif-args)
                            (assertion-violation 'use "failed to resolve module" mif-args)))
                     module-iface-args)])
    (module-use-interfaces! (current-module) interfaces)))

;; Resolve the variable bound to NAME in MODULE (through its public
;; interface when PUBLIC? is #t), raising an error when the module or
;; the binding does not exist.
(define (lookup-bound module name public?)
  (let ([mod (resolve-module module #f #f)])
    (if (not mod)
      (assertion-violation 'lookup-bound "module not found" module))
    (let* ([iface (if public? (module-public-interface mod) mod)]
           [var (module-variable iface name)])
      (if (or (not var) (not (variable-bound? var)))
        (assertion-violation 'lookup-bound "unbound variable" module name))
      var)))

;; Re-export NAMES from MODULE: a name whose variable is defined locally
;; is exported directly, any other name is handed to module-export!.
(define (module-re-export! m names . replace?)
  (let ([replace? (if (null? replace?) #f (car replace?))])
    (let ([public-i (module-public-interface m)])
      (for-each
        (lambda (name)
          (let* ([internal-name (if (pair? name) (car name) name)]
                 [external-name (if (pair? name) (cdr name) name)]
                 [var (module-variable m internal-name)])
            (cond
              [(not var)
                (module-export! m (list name) replace?)]
              [(eq? var (module-local-variable m internal-name))
                (module-export! m (list name) replace?)]
              [else
                (if replace?
                  (core-hash-put! (module-replacements public-i) external-name #t))
                (module-add! public-i external-name var)])))
        names))))

;; The parameter used to print uncaught exceptions. `print-condition` is
;; defined in `boot/conditions.scm`, which loads after this file; delegate
;; to it lazily once it is available, falling back to a minimal renderer
;; during the early boot window.
(define current-exception-printer
  (make-parameter
    (lambda (exn . port)
      (define p (if (null? port) (current-error-port) (car port)))
      (if (module-search module-defined? (current-module) 'print-condition)
        (print-condition exn p)
        (begin
          (display "Unhandled exception: " p)
          (write exn p)
          (newline p))))))


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
