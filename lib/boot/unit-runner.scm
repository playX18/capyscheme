;;; -*- mode: scheme; coding: utf-8; -*-
;;; The unit runner for programs that load code at run time without `boot/main.scm`.
;;;
;;; A compilation unit is a thunk returning the values of a linklet (`|#%linklet|`), of a
;;; namespace linklet, or of several of them (`|#%linklets|`). Running it instantiates the
;;; linklets in the current library registry. `boot/main.scm` installs a runner of its own
;;; that also works before the environments exist, but `main.scm` is not part of a static
;;; executable. An open static bundle (one that can compile and load code) links this
;;; file instead; the environments always exist there, so this is the run-time half of
;;; the runner in `main.scm` and the two must agree.

(|#%require| (boot prim) (boot control) (boot environments) (boot library-manager)
  (boot expand)
  (late (boot raise) error))
(|#%provide| all-defined)

(define (unit-boot-key? key)
  (and (pair? key) (eq? (car key) 'boot)))

;; The import instances of the linklet imports IMPORTS in the registry REG, one per import
;; group. A boot key that is not instantiated yet is a late import and gets the
;; placeholder instance. Other keys are loaded on demand, since a compiled unit did not
;; expand and so did not load its imports.
(define (unit-import-instances reg imports)
  (if (null? imports)
    '()
    (let* ([key (car (car imports))]
           [inst (or (library-instance reg key)
                  (if (unit-boot-key? key)
                    (library-instance-or-placeholder reg key)
                    (begin
                      (ensure-library-loaded! key)
                      (or (library-instance reg key)
                        (begin
                          (resolve-library key)
                          (library-instance reg key))))))])
      (if inst
        (cons inst (unit-import-instances reg (cdr imports)))
        (error '$run-unit-thunk "missing library instance" key)))))

;; Runs the body of a library linklet. The library gets no expansion environment here:
;; when psyntax built one while compiling the library in this process, it is given the
;; instance variables, and otherwise it is built from the meta when something expands
;; against the library.
(define (unit-run-library-body meta exports inst body slots)
  (let ([env (env-library-instantiated! meta inst exports)])
    (if env
      (let ([saved-env (current-expansion-env)])
        (current-expansion-env env)
        (apply body slots)
        (current-expansion-env saved-env))
      (apply body slots))))

;; Runs the body of any other linklet with the expansion environment of its `meta`
;; current, after its exports are put into that environment.
(define (unit-run-body-in-env meta exports inst body slots)
  (let ([env (let ([key (if (and (pair? meta) (eq? (car meta) '|#%boot|)) (car (cdr meta)) meta)])
               (if (pair? key) (ensure-env key) (expansion-env)))]
        [saved-env (current-expansion-env)])
    (current-expansion-env env)
    (env-mirror-exports! env inst exports)
    (apply body slots)
    (current-expansion-env saved-env)))

(define (unit-instantiate-linklet name imports exports body meta)
  (let* ([reg (current-library-registry)]
         [lk (make-linklet name imports exports body meta)]
         [inst (or (take-placeholder-instance! reg name) (make-linklet-instance name #f))]
         [slots (linklet-slots reg lk (unit-import-instances reg imports) inst)])
    (if (library-meta? meta)
      (unit-run-library-body meta exports inst body slots)
      (unit-run-body-in-env meta exports inst body slots))
    (register-library-instance! reg name inst)))

;; The import instances of a namespace linklet: the namespace itself, then the groups.
(define (unit-namespace-import-instances reg imports ns)
  (let ([own (env-instance ns)])
    (for-each (lambda (sym) (instance-ensure-variable! own sym)) (cdr (car imports)))
    (cons own (unit-import-instances reg (cdr imports)))))

;; Runs a namespace linklet in the namespace NS.
(define (unit-run-namespace-linklet ns name imports exports body meta)
  (let* ([reg (current-library-registry)]
         [lk (make-linklet name imports exports body meta)]
         [insts (unit-namespace-import-instances reg imports ns)]
         [slots (linklet-slots reg lk insts (env-instance ns))])
    ;; The imports of the unit are applied to the namespace when it is first used to
    ;; expand something (`eval`, the REPL), not now.
    (env-add-pending-imports! ns (cdr (cdr meta)))
    (apply body slots)))

;; Runs the values VALS of a unit thunk in the namespace NS (or #f for the current one).
(define (unit-run-values vals ns)
  (cond
    [(and (pair? vals) (eq? (car vals) '|#%linklet|) (= (length vals) 6))
      (let ([meta (list-ref vals 5)])
        (if (and (pair? meta) (eq? (car meta) '|#%namespace|))
          (apply unit-run-namespace-linklet (or ns (current-namespace) (expansion-env)) (cdr vals))
          (apply unit-instantiate-linklet (cdr vals))))]
    [(and (pair? vals) (eq? (car vals) '|#%linklets|))
      (let loop ([thunks (cdr vals)] [result (if #f #f)])
        (if (null? thunks)
          result
          (loop (cdr thunks) (unit-run-thunk (car thunks) ns))))]
    [else (apply values vals)]))

(define (unit-run-thunk thunk . maybe-ns)
  (call-with-values thunk
    (lambda vals
      (unit-run-values vals (and (pair? maybe-ns) (car maybe-ns))))))

($set-unit-runner! unit-run-thunk)
