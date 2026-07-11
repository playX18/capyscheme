;;; Capy psyntax library manager
;;;
;;; Adapted from Loko Scheme's psyntax/library-manager.ss
;;; (c) 2006,2007 Abdulaziz Ghuloum and Kent Dybvig — MIT licence
;;; Capy-specific adaptations: use Capy record API, core-hashtable,
;;; and Capy module system for host library registration.
;;;
;;; API surface (mirrors Loko / Chez psyntax):
;;;
;;;   install-library id name ver imp* vis* inv* subst env
;;;                   visit-proc invoke-proc visit-code invoke-code
;;;                   guard-code guard-req* visible? source-file-name
;;;   find-library-by-name  name → library or #f
;;;   library-exists?       name → bool
;;;   visit-library         lib  → void
;;;   invoke-library        lib  → void
;;;   imported-label->binding  lab → binding
;;;   library-subst         lib  → subst alist
;;;   current-library-collection  parameter: () → list, (lib) → add
;;;   allow-library-redefinition  parameter: bool
;;;   library-loader        parameter: name → void
;;;   current-library-expander parameter: expander thunk
;;;   file-locator          parameter: name missing-ok? → path|#f
;;;   library-path          parameter: list of dirs
;;;   library-extensions    parameter: list of suffixes
;;;   library-name->file-name  name → rel-path
;;;   library-spec          lib  → (id name version)
;;;   library-invoke-dependencies lib → list of libs
;;;   installed-libraries   [all?] → list of names
;;;   library-available?    name → bool
;;;   uninstall-library     name [err?]
;;;   set-label-binding!    lab binding → void    (imported from expander)
;;;   label-binding         lab → binding          (imported from expander)
;;;   remove-location       lab → void             (imported from expander)
;;;
;;; R6RS phasing vocabulary:
;;;   imp* = import dependencies (visit + invoke)
;;;   vis* = visit dependencies  (invoked when visiting)
;;;   inv* = invoke dependencies (invoked when invoking)
;;;   guard-code / guard-req* = staleness guard (compile-time only)
;;;
;;; For R7RS mode (capy:execution-mode = r7rs), every import is treated
;;; as both a visit and invoke dependency.

;;; -----------------------------------------------------------------------
;;; Library record
;;; -----------------------------------------------------------------------

(define <library-rtd>
  (make-record-type-descriptor
    '<library> #f #f #f #f
    '#((mutable id)
       (mutable name)
       (mutable version)
       (mutable imp*)
       (mutable vis*)
       (mutable inv*)
       (mutable subst)
       (mutable env)
       (mutable visit-state)
       (mutable invoke-state)
       (mutable visit-code)
       (mutable invoke-code)
       (mutable guard-code)
       (mutable guard-req*)
       (mutable visible?)
       (mutable source-file-name))))

(define <library-rcd>
  (make-record-constructor-descriptor <library-rtd> #f #f))

(define make-library  (record-constructor <library-rcd>))
(define library?      (record-predicate   <library-rtd>))

(define library-id          (record-accessor <library-rtd>  0))
(define library-name        (record-accessor <library-rtd>  1))
(define library-version     (record-accessor <library-rtd>  2))
(define library-imp*        (record-accessor <library-rtd>  3))
(define library-vis*        (record-accessor <library-rtd>  4))
(define library-inv*        (record-accessor <library-rtd>  5))
(define library-subst       (record-accessor <library-rtd>  6))
(define library-env         (record-accessor <library-rtd>  7))
(define library-visit-state (record-accessor <library-rtd>  8))
(define library-invoke-state (record-accessor <library-rtd> 9))
(define library-visit-code  (record-accessor <library-rtd> 10))
(define library-invoke-code (record-accessor <library-rtd> 11))
(define library-guard-code  (record-accessor <library-rtd> 12))
(define library-guard-req*  (record-accessor <library-rtd> 13))
(define library-visible?    (record-accessor <library-rtd> 14))
(define library-source-file-name (record-accessor <library-rtd> 15))

(define set-library-id!           (record-mutator <library-rtd>  0))
(define set-library-name!         (record-mutator <library-rtd>  1))
(define set-library-version!      (record-mutator <library-rtd>  2))
(define set-library-imp*!         (record-mutator <library-rtd>  3))
(define set-library-vis*!         (record-mutator <library-rtd>  4))
(define set-library-inv*!         (record-mutator <library-rtd>  5))
(define set-library-subst!        (record-mutator <library-rtd>  6))
(define set-library-env!          (record-mutator <library-rtd>  7))
(define set-library-visit-state!  (record-mutator <library-rtd>  8))
(define set-library-invoke-state! (record-mutator <library-rtd>  9))
(define set-library-visit-code!   (record-mutator <library-rtd> 10))
(define set-library-invoke-code!  (record-mutator <library-rtd> 11))
(define set-library-guard-code!   (record-mutator <library-rtd> 12))
(define set-library-guard-req*!   (record-mutator <library-rtd> 13))
(define set-library-visible?!     (record-mutator <library-rtd> 14))
(define set-library-source-file-name! (record-mutator <library-rtd> 15))

(define (library-invoke-dependencies lib) (library-inv* lib))

;;; -----------------------------------------------------------------------
;;; Collection: ordered list of installed libraries (newest first).
;;; -----------------------------------------------------------------------

(define (make-collection)
  (let ((set '()))
    (define (set-cons x ls)
      (cond ((memq x ls) ls)
            (else (cons x ls))))
    (case-lambda
      (() set)
      ((x) (set! set (set-cons x set)))
      ((x del?)
       (if del?
           (set! set (filter (lambda (y) (not (eq? x y))) set))
           (set! set (set-cons x set)))))))

(define allow-library-redefinition (make-parameter #f))

(define current-library-collection
  (make-parameter
    (make-collection)
    (lambda (x)
      (unless (procedure? x)
        (assertion-violation 'current-library-collection "not a procedure" x))
      x)))

;;; -----------------------------------------------------------------------
;;; Label → binding table (shared with the expander)
;;; -----------------------------------------------------------------------
;;; The expander maintains a per-label binding table.  We hold references
;;; to three thin hooks that it installs during boot.

(define *set-label-binding!* #f)
(define *label-binding*      #f)
(define *remove-location*    #f)

(define (set-label-binding! lab binding)
  (if *set-label-binding!*
      (*set-label-binding!* lab binding)
      (assertion-violation 'set-label-binding!
        "label binding table not yet initialised" lab)))

(define (label-binding lab)
  (if *label-binding*
      (*label-binding* lab)
      (assertion-violation 'label-binding
        "label binding table not yet initialised" lab)))

(define (remove-location lab)
  (when *remove-location*
    (*remove-location* lab)))

;;; Called from boot to wire in the expander's binding table.
(define (install-label-binding-hooks! set! get remove)
  (set! *set-label-binding!* set!)
  (set! *label-binding*      get)
  (set! *remove-location*    remove))

;;; -----------------------------------------------------------------------
;;; File locator
;;; -----------------------------------------------------------------------

(define library-path
  (make-parameter
    (list ".")
    (lambda (x)
      (if (and (list? x) (for-all string? x))
          x
          (assertion-violation 'library-path "not a list of strings" x)))))

(define library-extensions
  (make-parameter
    '(".sls" ".ss" ".scm")
    (lambda (x)
      (if (and (list? x) (for-all string? x))
          x
          (assertion-violation 'library-extensions "not a list of strings" x)))))

(define library-directories
  (case-lambda
    (() (library-path))
    ((dirs)
     (if (string? dirs)
         (library-directories (list dirs))
         (library-path dirs)))))

(define (library-name->file-name ls)
  ;; Encode library name components to a relative path segment
  ;; following the R6RS file-name encoding: each component becomes
  ;; a slash-separated element with unsafe characters %-encoded.
  (define (encode-component sym-or-int)
    (let ((name (if (symbol? sym-or-int)
                    (symbol->string sym-or-int)
                    (number->string sym-or-int))))
      (call-with-string-output-port
        (lambda (p)
          (for-each
            (lambda (n)
              (let ((c (integer->char n)))
                (cond
                  ((or (char<=? #\a c #\z)
                       (char<=? #\A c #\Z)
                       (char<=? #\0 c #\9)
                       (memv c '(#\- #\. #\_ #\~)))
                   (write-char c p))
                  (else
                   (write-char #\% p)
                   (let-values (((h l) (fxdiv-and-mod n 16)))
                     (let ((hex-char (lambda (d)
                                       (if (fx<= d 9)
                                           (integer->char (fx+ (char->integer #\0) d))
                                           (integer->char (fx+ (char->integer #\a) (fx- d 10)))))))
                       (write-char (hex-char h) p)
                       (write-char (hex-char l) p)))))))
            (bytevector->u8-list (string->utf8 name)))))))
  (apply string-append
    (map (lambda (x) (string-append "/" (encode-component x))) ls)))

(define file-locator
  (make-parameter
    (lambda (x missing-ok?)
      (let ((str (library-name->file-name x))
            (path (library-path)))
        (let f ((ls path) (exts (library-extensions)) (failed '()))
          (cond
            ((null? ls)
             (if missing-ok?
                 #f
                 (assertion-violation 'file-locator
                   (string-append
                     "cannot find library file for "
                     (format "~s" x)
                     "; tried: "
                     (format "~s" (reverse failed)))
                   x)))
            ((null? exts)
             (f (cdr ls) (library-extensions) failed))
            (else
             (let ((candidate (string-append (car ls) str (car exts))))
               (if (file-exists? candidate)
                   candidate
                   (f ls (cdr exts) (cons candidate failed)))))))))
    (lambda (f)
      (if (procedure? f)
          f
          (assertion-violation 'file-locator "not a procedure" f)))))

;;; -----------------------------------------------------------------------
;;; Search utilities
;;; -----------------------------------------------------------------------

(define (find-library-by pred)
  (let f ((ls ((current-library-collection))))
    (cond
      ((null? ls) #f)
      ((pred (car ls)) (car ls))
      (else (f (cdr ls))))))

(define (find-library-by-spec/die spec)
  (let ((id (car spec)))
    (or (find-library-by (lambda (x) (eq? id (library-id x))))
        (assertion-violation 'find-library-by-spec/die
          "cannot find library with required spec" spec))))

;;; -----------------------------------------------------------------------
;;; External library loading (via file-locator + current-library-expander)
;;; -----------------------------------------------------------------------

(define current-library-expander
  (make-parameter
    (lambda (x file-name k)
      (assertion-violation 'library-expander "not initialized"))
    (lambda (f)
      (if (procedure? f)
          f
          (assertion-violation 'library-expander "not a procedure" f)))))

(define external-pending-libraries (make-parameter '()))

(define library-loader
  (make-parameter
    (lambda (name)
      (let ((file-name ((file-locator) name #f)))
        (cond
          ((not file-name)
           (assertion-violation #f "cannot find library" name))
          (else
           ((current-library-expander)
            (read-library-source-file file-name)
            file-name
            (lambda (found-name)
              (unless (equal? found-name name)
                (assertion-violation 'import
                  (format #f "expected ~s in ~a, found ~s" name file-name found-name)))))))))
    (lambda (f)
      (if (procedure? f)
          f
          (assertion-violation 'library-loader "not a procedure" f)))))

(define (read-library-source-file file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let lp ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x)
              (reverse acc)
              (lp (cons x acc))))))))

(define (find-external-library name)
  (when (member name (external-pending-libraries))
    (assertion-violation #f
      "circular library import detected" name))
  (parameterize ((external-pending-libraries
                  (cons name (external-pending-libraries))))
    ((library-loader) name)
    (or (find-library-by (lambda (x) (equal? (library-name x) name)))
        (assertion-violation #f
          "library loader did not install the expected library" name))))

;;; -----------------------------------------------------------------------
;;; Public query API
;;; -----------------------------------------------------------------------

(define (find-library-by-name name)
  (or (find-library-by (lambda (x) (equal? (library-name x) name)))
      (find-external-library name)))

(define (library-exists? name)
  (and (find-library-by (lambda (x) (equal? (library-name x) name)))
       #t))

(define (library-available? name)
  (or (library-exists? name)
      (and ((file-locator) name #t) #t)))

(define (library-spec lib)
  (unless (library? lib)
    (assertion-violation 'library-spec "not a library" lib))
  (list (library-id lib) (library-name lib) (library-version lib)))

(define (imported-label->binding lab)
  (label-binding lab))

;;; -----------------------------------------------------------------------
;;; install-library
;;; -----------------------------------------------------------------------

(define (install-library-record lib)
  (for-each
    (lambda (x)
      (let ((label (car x)) (binding (cdr x)))
        (let ((binding
               (case (car binding)
                 ((global)       (cons 'global       (cons lib (cdr binding))))
                 ((global-macro) (cons 'global-macro (cons lib (cdr binding))))
                 ((global-macro!)(cons 'global-macro!(cons lib (cdr binding))))
                 ((global-ctv)   (cons 'global-ctv   (cons lib (cdr binding))))
                 (else binding))))
          (set-label-binding! label binding))))
    (library-env lib))
  ((current-library-collection) lib))

(define install-library
  (case-lambda
    ((id name ver imp* vis* inv* exp-subst exp-env
      visit-proc invoke-proc visit-code invoke-code
      guard-code guard-req*
      visible? source-file-name)
     (let ((imp-lib* (map find-library-by-spec/die imp*))
           (vis-lib* (map find-library-by-spec/die vis*))
           (inv-lib* (map find-library-by-spec/die inv*))
           (guard-lib* (map find-library-by-spec/die guard-req*)))
       (unless (and (symbol? id) (list? name) (list? ver))
         (assertion-violation 'install-library
           "invalid id/name/ver" id name ver))
       (when (library-exists? name)
         (if (allow-library-redefinition)
             (uninstall-library name)
             (assertion-violation 'install-library
               "library already installed" name)))
       (let ((lib (make-library id name ver
                                imp-lib* vis-lib* inv-lib*
                                exp-subst exp-env
                                visit-proc invoke-proc
                                visit-code invoke-code
                                guard-code guard-lib*
                                visible? source-file-name)))
         (install-library-record lib))))))

;;; -----------------------------------------------------------------------
;;; uninstall-library
;;; -----------------------------------------------------------------------

(define uninstall-library
  (case-lambda
    ((name err?)
     (let ((lib (find-library-by (lambda (x) (equal? (library-name x) name)))))
       (when (and err? (not lib))
         (assertion-violation 'uninstall-library "library not installed" name))
       (when lib
         ((current-library-collection) lib #t)
         (for-each
           (lambda (x)
             (let ((label (car x)) (binding (cdr x)))
               (remove-location label)
               (when (memq (car binding)
                           '(global global-macro global-macro! global-ctv))
                 (remove-location (cdr binding)))))
           (library-env lib)))))
    ((name)
     (uninstall-library name #t))))

;;; -----------------------------------------------------------------------
;;; visit-library / invoke-library
;;; -----------------------------------------------------------------------

(define (visit-library lib)
  (let ((visit (library-visit-state lib)))
    (when (procedure? visit)
      (set-library-visit-state! lib
        (lambda () (assertion-violation 'visit-library "circular visit" lib)))
      (for-each invoke-library (library-vis* lib))
      (set-library-visit-state! lib
        (lambda () (assertion-violation 'visit-library "visit did not complete" lib)))
      (visit)
      (set-library-visit-state! lib #t))))

(define (invoke-library lib)
  (let ((invoke (library-invoke-state lib)))
    (when (procedure? invoke)
      (set-library-invoke-state! lib
        (lambda () (assertion-violation 'invoke-library "circular invoke" lib)))
      (for-each invoke-library (library-inv* lib))
      (set-library-invoke-state! lib
        (lambda () (assertion-violation 'invoke-library "invoke did not complete" lib)))
      (invoke)
      (set-library-invoke-state! lib #t))))

;;; -----------------------------------------------------------------------
;;; installed-libraries
;;; -----------------------------------------------------------------------

(define installed-libraries
  (case-lambda
    ((all?)
     (let f ((ls ((current-library-collection))))
       (cond
         ((null? ls) '())
         ((or all? (library-visible? (car ls)))
          (cons (library-name (car ls)) (f (cdr ls))))
         (else (f (cdr ls))))))
    (() (installed-libraries #f))))

;;; -----------------------------------------------------------------------
;;; Boot-library helpers: register a host (Guile module) library
;;; -----------------------------------------------------------------------
;;; During bootstrap, Capy libraries such as (core), (capy), (capy prelims)
;;; already exist as Guile modules.  `boot-install-library` registers them
;;; in the library manager so that `(import (capy))` etc. can resolve via
;;; find-library-by-name without hitting the file locator.

(define (boot-install-library name guile-module)
  "Register a pre-existing host module NAME as a library manager entry."
  (define id (generate-temporary-symbol))
  (define subst
    ;; Build a substitution alist: (exported-sym . gensym-label)
    ;; We use the module variable names directly as labels since the
    ;; binding table is already populated by the host.
    (if guile-module
        (let ((result '()))
          (module-for-each
            (lambda (sym _var)
              (set! result (cons (cons sym sym) result)))
            guile-module)
          result)
        '()))
  (define env '())  ; bindings already in host module; expander sees them via module-variable

  (unless (library-exists? name)
    (let ((lib (make-library id name '()
                              '() '() '()
                              subst env
                              #t   ; visit-state: already visited
                              #t   ; invoke-state: already invoked
                              #f   ; visit-code
                              #f   ; invoke-code
                              #f   ; guard-code
                              '()  ; guard-req*
                              #t   ; visible?
                              #f))); source-file-name
      ((current-library-collection) lib))))
