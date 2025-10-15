(library (core cond-expand)
    (export cond-expand)
    (import)

;;; {`cond-expand' for SRFI-0 support.}
;;;
;;; This syntactic form expands into different commands or
;;; definitions, depending on the features provided by the Scheme
;;; implementation.
;;;
;;; Syntax:
;;;
;;; <cond-expand>
;;;   --> (cond-expand <cond-expand-clause>+)
;;;     | (cond-expand <cond-expand-clause>* (else <command-or-definition>))
;;; <cond-expand-clause>
;;;   --> (<feature-requirement> <command-or-definition>*)
;;; <feature-requirement>
;;;   --> <feature-identifier>
;;;     | (and <feature-requirement>*)
;;;     | (or <feature-requirement>*)
;;;     | (not <feature-requirement>)
;;; <feature-identifier>
;;;   --> <a symbol which is the name or alias of a SRFI>
;;;
;;; Additionally, this implementation provides the
;;; <feature-identifier>s `guile' and `r5rs', so that programs can
;;; determine the implementation type and the supported standard.
;;;
;;; Remember to update the features list when adding more SRFIs.
;;;

(define %cond-expand-features
  ;; This should contain only features that are present in core Guile,
  ;; before loading any modules.  Modular features are handled by
  ;; placing 'cond-expand-provide' in the relevant module.
  '(capy
    r5rs
    r6rs
    r7rs
    exact-closed ieee-float full-unicode ratios ;; R7RS features.
    srfi-0   ;; cond-expand itself
    srfi-4   ;; homogeneous numeric vectors
    srfi-6   ;; string ports
    srfi-13  ;; string library
    srfi-14  ;; character sets
    srfi-16  ;; case-lambda
    srfi-23  ;; `error` procedure
    srfi-30  ;; nested multi-line comments
    srfi-39  ;; parameterize
    srfi-46  ;; basic syntax-rules extensions
    srfi-55  ;; require-extension
    srfi-61  ;; general cond clause
    srfi-62  ;; s-expression comments
    srfi-87  ;; => in case clauses
    srfi-105 ;; curly infix expressions
    ))

;; This table maps module public interfaces to the list of features.
;;
(define %cond-expand-table (make-core-hash-eq))

;; Add one or more features to the `cond-expand' feature list of the
;; module `module'.
;;
(define (cond-expand-provide module features)
  (let ((mod (module-public-interface module)))
    (and mod
         (core-hash-put! %cond-expand-table mod
                     (append (core-hash-ref %cond-expand-table mod '())
                             features)))))

(define-syntax cond-expand
  (lambda (x)
    (define (module-has-feature? mod sym)
      (or-map (lambda (mod)
                (memq sym (core-hash-ref %cond-expand-table mod '())))
              (module-uses mod)))

    (define (condition-matches? condition)
      (syntax-case condition (and or not)
        ((and c ...)
         (and-map condition-matches? #'(c ...)))
        ((or c ...)
         (or-map condition-matches? #'(c ...)))
        ((not c)
         (if (condition-matches? #'c) #f #t))
        (c
         (identifier? #'c)
         (let ((sym (syntax->datum #'c)))
           (if (memq sym %cond-expand-features)
               #t
               (module-has-feature? (current-module) sym))))))

    (define (match clauses alternate)
      (syntax-case clauses ()
        (((condition form ...) . rest)
         (if (condition-matches? #'condition)
             #'(begin form ...)
             (match #'rest alternate)))
        (() (alternate))))

    (syntax-case x (else)
      ((_ clause ... (else form ...))
       (match #'(clause ...)
         (lambda ()
           #'(begin form ...))))
      ((_ clause ...)
       (match #'(clause ...)
         (lambda ()
           (syntax-violation 'cond-expand "unfulfilled cond-expand" x)))))))
    
)