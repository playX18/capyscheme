;;;; -*-scheme-*-
;;;;
;;;; Copyright (C) 1997-1998,2000-2003,2005-2006,2008-2013,2015-2022,2024
;;;;   Free Software Foundation, Inc.
;;;;
;;;; This library is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.


;;; Originally extracted from Chez Scheme Version 5.9f
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Copyright (c) 1992-1997 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; This code is based on "Syntax Abstraction in Scheme"
;;; by R. Kent Dybvig, Robert Hieb, and Carl Bruggeman.
;;; Lisp and Symbolic Computation 5:4, 295-326, 1992.
;;; <http://www.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf>


;;; This file defines Capy's syntax expander and a set of associated
;;; syntactic forms and procedures.  For more documentation, see The
;;; Scheme Programming Language, Fourth Edition (R. Kent Dybvig, MIT
;;; Press, 2009), or the R6RS.

;;; This file is shipped along with an expanded version of itself,
;;; psyntax-exp.scm, which is loaded when psyntax.scm has not yet been
;;; compiled.  In this way, psyntax bootstraps off of an expanded
;;; version of itself.

;;; NOTES ON ORIGINAL SOURCE
;;; 
;;; This file was extracted from Guile 3.0+ and modified to suit Capy's
;;; needs. Thanks to Guile developers for their work.


;;; Implementation notes:

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax object, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax.
;;; Such objects are never copied.

;;; All identifiers that don't have macro definitions and are not bound
;;; lexically are assumed to be global variables.

;;; Top-level definitions of macro-introduced identifiers are allowed.

;;; When changing syntax representations, it is necessary to support
;;; both old and new syntax representations in id-var-name.  It
;;; should be sufficient to recognize old representations and treat
;;; them as not lexically bound.


(define syntax->datum #f)
(define datum->syntax #f)
(define identifier? #f)
(define generate-identifier #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define $sc-dispatch #f)
(define macroexpand #f)
(define :ellipsis? #f)

(define <variable-transformer>
  (let* ((rtd (make-record-type-descriptor '<variable-transformer> #f #f #f #f '#((immutable proc))))
         (rcd (make-record-constructor-descriptor rtd #f #f)))
    (make-record-type '<variable-transformer> rtd rcd)))

(define make-variable-transformer (record-constructor (record-type-rcd <variable-transformer>)))
(define variable-transformer? (record-predicate (record-type-rtd <variable-transformer>)))
(define variable-transformer-procedure (record-accessor (record-type-rtd <variable-transformer>) 0))

(define syntax-error #f)

(let ((syntax? (module-ref (current-module) 'syntax?))
      (make-syntax (module-ref (current-module) 'make-syntax))
      (syntax-expression (module-ref (current-module) 'syntax-expression))
      (syntax-wrap (module-ref (current-module) 'syntax-wrap))
      (syntax-module (module-ref (current-module) 'syntax-module))
      (syntax-sourcev (module-ref (current-module) 'syntax-sourcev)))

  (define (syntax-pair? x)
    (cond 
      [(syntax? x) (pair? (syntax-expression x))]
      [else (pair? x)]))
  
  (define (syntax-car x)
    (cond 
      [(pair? x) (car x)]
      [(syntax? x) 
        (syntax-case x () 
          [(head . tail) #'head])]
      [else (error 'car "not a pair" x)]))
  
  (define (syntax-cdr x)
    (cond 
      [(pair? x) (cdr x)]
      [(syntax? x) 
        (syntax-case x () 
          [(head . tail) #'tail])]
      [else (error 'cdr "not a pair" x)]))

  (define (modname? n)
    (cond 
      [(syntax? n) (modname? (syntax-expression n))]
      [(symbol? n) #t]
      [(number? n) (exact-nonnegative-integer? n)]
      [else #f]))

  ;; A simple pattern matcher based on Oleg Kiselyov's pmatch.
  (define-syntax-rule (simple-match e cs ...)
    (let ((v e)) (simple-match-1 v cs ...)))

  (define-syntax simple-match-1
    (syntax-rules ()
      ((_ v) (error "value failed to match" v))
      ((_ v (pat e0 e ...) cs ...)
        (let ((fk (lambda () (simple-match-1 v cs ...))))
          (simple-match-pat v pat (let () e0 e ...) (fk))))))

  (define-syntax simple-match-patv
    (syntax-rules ()
      ((_ v idx () kt kf) kt)
      ((_ v idx (x . y) kt kf)
        (simple-match-pat (vector-ref v idx) x
          (simple-match-patv v (1+ idx) y kt kf)
          kf))))

  (define-syntax simple-match-pat
    (syntax-rules (_ quote unquote ? and or not)
      ((_ v _ kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v #t kt kf) (if (eq? v #t) kt kf))
      ((_ v #f kt kf) (if (eq? v #f) kt kf))
      ((_ v (and) kt kf) kt)
      ((_ v (and x . y) kt kf)
        (simple-match-pat v x (simple-match-pat v (and . y) kt kf) kf))
      ((_ v (or) kt kf) kf)
      ((_ v (or x . y) kt kf)
        (let ((tk (lambda () kt)))
          (simple-match-pat v x (tk) (simple-match-pat v (or . y) (tk) kf))))
      ((_ v (not pat) kt kf) (simple-match-pat v pat kf kt))
      ((_ v (quote lit) kt kf)
        (if (eq? v (quote lit)) kt kf))
      ((_ v (? proc) kt kf) (simple-match-pat v (? proc _) kt kf))
      ((_ v (? proc pat) kt kf)
        (if (proc v) (simple-match-pat v pat kt kf) kf))
      ((_ v (x . y) kt kf)
        (if (pair? v)
          (let ((vx (car v)) (vy (cdr v)))
            (simple-match-pat vx x (simple-match-pat vy y kt kf) kf))
          kf))
      ((_ v #(x ...) kt kf)
        (if (and (vector? v)
             (eq? (vector-length v) (length '(x ...))))
          (simple-match-patv v 0 (x ...) kt kf)
          kf))
      ((_ v var kt kf) (let ((var v)) kt))))

  (define-syntax-rule (match e cs ...) (simple-match e cs ...))

  (define (top-level-eval exp env) (primitive-eval exp))
  (define (local-eval exp env) (primitive-eval exp))

  (define nil (list "nil"))

  (define (global-extend type sym val)
    (module-define! (current-module) sym (make-syntax-transformer sym type val)))
  (define no-source #f)
  (define (sourcev-filename s) (vector-ref s 0))
  (define (sourcev-line s) (vector-ref s 1))
  (define (sourcev-column s) (vector-ref s 2))
  (define sourcev->alist
    (lambda (sourcev)
      (letrec* ((maybe-acons (lambda (k v tail) (if v (acons k v tail) tail))))
        (and sourcev
          (maybe-acons
            'filename
            (sourcev-filename sourcev)
            (list (cons 'line (sourcev-line sourcev)) (cons 'column (sourcev-column sourcev))))))))
  (define maybe-name-value
    (lambda (name val)
      (if (proc? val)
        (let ((meta (proc-meta val)))
          (if (assq 'name meta) val (make-proc (term-src val) (proc-args val) (proc-body val) (acons 'name name meta) (proc-ids val))))
        val)))
  (define (build-primref src name) (make-primref src name))
  (define (build-void s) (make-void s))
  (define (analyze-variable mod var modref-cont bare-cont)
    (match mod
      [#f (bare-cont #f var)]
      [('public . mod) (modref-cont mod var #t)]
      [((or 'private 'hygiene) . mod)
        (if (equal? mod (module-name (current-module)))
          (bare-cont mod var)
          (modref-cont mod var #f))]
      [('primitive . _)
        (syntax-violation #f "primitive not in operator position" var)]))

  (define (build-global-definition src mod var exp)
    (make-toplevel-define src (and mod (cdr mod)) var (maybe-name-value var exp)))
  (define (build-global-reference src var mod)
    (analyze-variable
      mod
      var
      (lambda (mod var public?) (make-module-ref src mod var public?))
      (lambda (mod var) (make-toplevel-ref src mod var))))
  ; (make-toplevel-ref src mod var))
  (define (build-global-assignment src var val mod)
    (let ((exp (maybe-name-value var val)))
      (analyze-variable
        mod
        var
        (lambda (mod var public?) (make-module-set src mod var public? exp))
        (lambda (mod var) (make-toplevel-set src mod var exp)))))
  (define (build-primcall src name args)
    (make-primcall src name args))
  (define (build-simple-lambda src ids vars meta exp)
    (make-proc src vars exp meta ids))
  (define (expand-simple-lambda e r w s mod req rest meta body)
    (define ids (if rest (append req (list rest)) req))
    (define req-vars (map gen-var req))
    (define rest-var (if rest (gen-var rest) '()))
    (define vars (append req-vars (list rest-var)))
    (define labels (gen-labels ids))

    (build-simple-lambda
      s
      ids
      (if (null? req-vars) rest-var (append req-vars rest-var))
      meta
      (expand-body
        body
        (source-wrap e w s mod)
        (extend-var-env labels vars r)
        (make-binding-wrap ids labels w)
        mod)))
  (define (build-sequence src exps)
    (match exps
      ((tail) tail)
      ((head . tail)
        (make-sequence src head (build-sequence #f tail)))))
  (define (build-let src ids vars val-exps body-exp)
    (match (map maybe-name-value ids val-exps)
      (() body-exp)
      (val-exps
        (make-let src 'let ids vars val-exps body-exp))))

  (define (build-let* src ids vars val-exps body-exp)
    (let* ((v (map maybe-name-value ids val-exps)))
      (cond
        ((null? v) body-exp)
        (else
          (make-let src 'let* ids vars v body-exp)))))
  (define (build-letrec src ids vars val-exps body-exp)
    (let* ((v (map maybe-name-value ids val-exps)))
      (cond
        ((null? v) body-exp)
        (else
          (make-let src 'letrec ids vars v body-exp)))))
  (define (build-letrec* src ids vars val-exps body-exp)
    (let* ((v (map maybe-name-value ids val-exps)))
      (cond
        ((null? v) body-exp)
        (else
          (make-let src 'letrec* ids vars v body-exp)))))
  (define (build-lexical-reference src name sym)
    (make-lref src name sym))
  (define (build-lexical-assignment src name sym value)
    (make-lset src name sym value))
  (define (build-call src rator rands)
    (make-application src rator rands))
  (define (build-conditional src test-exp then-exp else-exp)
    (make-if src test-exp then-exp else-exp))
  (define (build-data src val)
    (make-constant src val))

  (define (build-named-let src ids vars val-exps body-exp)
    (match vars
      ((f . vars)
        (match ids
          ((f-name . ids)
            (let ((proc (build-simple-lambda src ids vars '() body-exp)))
              (make-let
                src
                'letrec
                (list f-name)
                (list f)
                (list (maybe-name-value f-name proc))
                (build-call
                  src
                  (build-lexical-reference src f-name f)
                  (map maybe-name-value ids val-exps)))))))))
  (define (gen-lexical id) (module-gensym (symbol->string id)))
  (define (source-annotation x) (if (syntax? x) (syntax-sourcev x) (datum-sourcev x)))

  (define-syntax make-binding
    (syntax-rules (quote)
      ((_ type value) (cons type value))
      ((_ 'type) '(type))
      ((_ type) (cons type '()))))

  (define (binding-type x) (car x))
  (define (binding-value x) (cdr x))
  (define null-env '())
  (define (extend-env labels bindings r)
    (cond
      ((null? labels) r)
      ((pair? labels)
        (let ((label (car labels)) (labels (cdr labels)))
          (let ((binding (car bindings)) (bindings (cdr bindings)))
            (extend-env labels bindings (acons label binding r)))))))
  (define (extend-var-env labels vars r)
    (cond
      ((null? labels) r)
      ((pair? labels)
        (let ((label (car labels)) (labels (cdr labels)))
          (let ((var (car vars)) (vars (cdr vars)))
            (extend-var-env labels vars (acons label (cons 'lexical var) r)))))))
  ;; we use a "macros only" environment in expansion of local macro
  ;; definitions so that their definitions can use local macros without
  ;; attempting to use other lexical identifiers.
  (define (macros-only-env r)
    (match r
      (() '())
      ((a . r)
       (match a
         ((k . ((or 'macro 'syntax-parameter 'ellipsis) . _))
          (cons a (macros-only-env r)))
         (_
          (macros-only-env r))))))

  (define (nonsymbol-id? x) (and (syntax? x) (symbol? (syntax-expression x))))
  (define (id? x)
    (cond
      ((symbol? x) #t)
      ((syntax? x) (symbol? (syntax-expression x)))
      (else #f)))
  (define (syntax-car x)
    (if (syntax? x)
      (let ((e (syntax-expression x))
            (w (syntax-wrap x))
            (m (syntax-module x))
            (s (syntax-sourcev x)))
        (make-syntax (car e) w m s))
      (car x)))
  (define (syntax-cdr x)
    (if (syntax? x)
      (let ((e (syntax-expression x))
            (w (syntax-wrap x))
            (m (syntax-module x))
            (s (syntax-sourcev x)))
        (make-syntax (cdr e) w m s))
      (cdr x)))
  (define (id-sym-name x) (if (syntax? x) (syntax-expression x) x))
  (define (id-sym-name&marks x w)
    (if (syntax? x)
      (values (syntax-expression x) (join-marks (wrap-marks w) (wrap-marks (syntax-wrap x))))
      (values x (wrap-marks w))))
  (define (make-wrap marks subst) (cons marks subst))
  (define (wrap-marks w) (car w))
  (define (wrap-subst w) (cdr w))

  (define (gen-unique . args)
    (cond
      ((null? args) (vector '(capy) (gensym "id")))
      (else (vector (module-name (car args)) (module-generate-unique-id! (car args))))))

  (define (gen-label) (gen-unique))
  (define (gen-labels ls)
    (cond
      ((null? ls) '())
      (else (cons (gen-label) (gen-labels (cdr ls))))))
  (define (make-ribcage symnames marks labels) (vector 'ribcage symnames marks labels))
  (define (ribcage-symnames r) (vector-ref r 1))
  (define (ribcage-marks r) (vector-ref r 2))
  (define (ribcage-labels r) (vector-ref r 3))
  (define (set-ribcage-symnames! r v) (vector-set! r 1 v))
  (define (set-ribcage-marks! r v) (vector-set! r 2 v))
  (define (set-ribcage-labels! r v) (vector-set! r 3 v))

  (define empty-wrap '(()))
  (define top-mark '(top))
  (define top-wrap '((top)))
  (define top-wrap-v '#((top)))
  (define the-anti-mark #f)
  (define (anti-mark w) (make-wrap (cons the-anti-mark (wrap-marks w)) (cons 'shift (wrap-subst w))))
  (define (new-mark) (gen-unique))
  (define (make-empty-ribcage) (make-ribcage '() '() '()))
  (define (extend-ribcage! ribcage id label)
    (set-ribcage-symnames! ribcage (cons (syntax-expression id) (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage (cons (wrap-marks (syntax-wrap id)) (ribcage-marks ribcage)))
    (set-ribcage-labels! ribcage (cons label (ribcage-labels ribcage))))
  (define (make-binding-wrap ids labels w)
    (cond
      ((null? ids) w)
      (else
        (make-wrap
          (wrap-marks w)
          (cons (let* ((labelvec (list->vector labels))
                       (n (vector-length labelvec))
                       (symnamevec (make-vector n))
                       (marksvec (make-vector n)))
                 (let f ((ids ids) (i 0))
                   (cond
                     ((null? ids) (make-ribcage symnamevec marksvec labelvec))
                     (else
                       (let ((id (car ids)) (ids (cdr ids)))
                         (call-with-values
                           (lambda () (id-sym-name&marks id w))
                           (lambda (symname marks)
                             (vector-set! symnamevec i symname)
                             (vector-set! marksvec i marks)
                             (f ids (+ i 1)))))))))
            (wrap-subst w))))))
  (define (smart-append m1 m2) (if (null? m2) m1 (append m1 m2)))
  (define (join-wraps w1 w2)
    (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
      (if (null? m1)
        (if (null? s1)
          w2
          (make-wrap
            (wrap-marks w2)
            (smart-append s1 (wrap-subst w2))))
        (make-wrap
          (smart-append m1 (wrap-marks w2))
          (smart-append s1 (wrap-subst w2))))))

  (define (join-marks m1 m2) (smart-append m1 m2))
  (define (same-marks? x y)
    (or (eq? x y)
      (and (not (null? x))
        (not (null? y))
        (eq? (car x) (car y))
        (same-marks? (cdr x) (cdr y)))))

  (define (id-var-name id w mod)
    ;; Returns the name/label for an identifier given its wrap and module.
    ;; Returns:  symbol, syntax object, or string label

    (define (same-marks? m1 m2)
      ;; Assuming this helper exists elsewhere
      (equal? m1 m2))

    (define (search-vector-rib sym marks rsymnames rmarks rlabels subst)
      (let ((n (vector-length rsymnames)))
        (let loop ((i 0))
          (cond
            ((= i n) (search sym subst marks))
            ((and (eq? (vector-ref rsymnames i) sym)
                (same-marks? marks (vector-ref rmarks i)))
              (let ((label (vector-ref rlabels i)))
                (if (pair? label)
                  (let ((mod* (car label))
                        (lbl (cdr label)))
                    (if (equal? mod* mod)
                      lbl
                      (loop (+ i 1))))
                  label)))
            (else (loop (+ i 1)))))))

    (define (search-list-rib sym marks rsymnames rmarks rlabels subst)
      (let loop ((rsyms rsymnames)
                 (rms rmarks)
                 (rlbls rlabels))
        (cond
          ((null? rsyms) (search sym subst marks))
          ((and (eq? sym (car rsyms))
              (same-marks? marks (car rms)))
            (let ((label (car rlbls)))
              (if (pair? label)
                (let ((mod* (car label))
                      (lbl (cdr label)))
                  (if (equal? mod* mod)
                    lbl
                    (loop (cdr rsyms) (cdr rms) (cdr rlbls))))
                label)))
          (else (loop (cdr rsyms) (cdr rms) (cdr rlbls))))))

    (define (search sym subst marks)
      (cond
        ((null? subst) #f)

        ;; Ribcage entry
        ((and (pair? subst)
            (vector? (car subst))
            (= (vector-length (car subst)) 4)
            (eq? (vector-ref (car subst) 0) 'ribcage))
          (let ((ribcage (car subst))
                (rest (cdr subst)))
            (let ((rsymnames (vector-ref ribcage 1))
                  (rmarks (vector-ref ribcage 2))
                  (rlabels (vector-ref ribcage 3)))
              (if (vector? rsymnames)
                (search-vector-rib sym marks rsymnames rmarks rlabels rest)
                (search-list-rib sym marks rsymnames rmarks rlabels rest)))))

        ;; Shift entry
        ((and (pair? subst)
            (eq? (car subst) 'shift))
          (search sym (cdr subst) (cdr marks)))

        ;; Continue searching
        (else #f)))

    (cond
      ((symbol? id)
        (or (search id (wrap-subst w) (wrap-marks w)) id))

      ((syntax? id)
        (let* ((expr (syntax-expression id))
               (w1 (syntax-wrap id))
               (mod1 (or (syntax-module id) mod))
               (marks (join-marks (wrap-marks w) (wrap-marks w1))))
          (or (search expr (wrap-subst w) marks)
            (search expr (wrap-subst w1) marks)
            expr)))

      (else
        (syntax-violation 'id-var-name "invalid id" id))))
  (define (locally-bound-identifiers w mod)
    (define (scan subst results)
      (cond
        ((null? subst) results)
        ((and (pair? subst) (eq? (car subst) 'shift))
          (scan (cdr subst) results))
        ((and (pair? subst)
            (vector? (car subst))
            (eq? (vector-ref (car subst) 0) 'ribcage))
          (let* ((ribcage (car subst))
                 (symnames (ribcage-symnames ribcage))
                 (marks (ribcage-marks ribcage))
                 (labels (ribcage-labels ribcage))
                 (subst* (cdr subst)))
            (define (scan-list-rib)
              (let lp ((symnames symnames) (marks marks) (results results))
                (cond
                  ((null? symnames) (scan subst* results))
                  (else
                    (let ((sym (car symnames))
                          (symnames (cdr symnames))
                          (m (car marks))
                          (marks (cdr marks)))
                      (lp symnames marks
                        (cons (wrap sym (anti-mark (make-wrap m subst)) mod) resulsts)))))))
            (define (scan-vector-rib)
              (let ((n (vector-length symnames)))
                (let lp ((i 0) (results results))
                  (cond
                    ((= i n) (scan subst* results))
                    (else
                      (let ((sym (vector-ref symnames i))
                            (m (vector-ref marks i)))
                        (lp (+ i 1)
                          (cons (wrap sym (anti-mark (make-wrap m subst)) mod) results))))))))
            (if (vector? symnames) (scan-vector-rib) (scan-list-rib))))))
    (scan (wrap-subst w) '()))
  (define (resolve-identifier id w r mod resolve-syntax-parameters?)
    (define (resolve-global var mod)
      (let ((v (and (not (equal? mod '(primitive)))
                (module-variable (if mod (resolve-module (cdr mod) #t #t) (current-module)) var))))
        (if (and v (variable-bound? v) (macro? (variable-ref v)))
          (let* ((m (variable-ref v))
                 (type (macro-type m))
                 (trans (macro-binding m)))
            (if (eq? type 'syntax-parameter)
              (if resolve-syntax-parameters?
                (let ((lexical (assq-ref r v)))
                  (values 'macro
                    (if lexical
                      (binding-value lexical)
                      trans)
                    mod))
                (values type v mod))
              (values type trans mod)))
          (values 'global var mod))))
    (define (resolve-lexical label mod)
      (let ((b (assq-ref r label)))
        (if b
          (let ((type (binding-type b))
                (value (binding-value b)))
            (if (eq? type 'syntax-parameter)
              (if resolve-syntax-parameters?
                (values 'macro value mod)
                (values type label mod))
              (values type value mod)))
          (values 'displaced-lexical #f #f))))
    (let ((n (id-var-name id w mod)))
      (cond
        ((syntax? n)
          (cond
            ((not (eq? n id))
              (resolve-identifier n w r mod resolve-syntax-parameters?))
            (else
              (resolve-identifier (syntax-expression n) (syntax-wrap n) r (or (syntax-module n) mod) resolve-syntax-parameters?))))
        ((symbol? n)
          (resolve-global n (or (and (syntax? id) (syntax-module id)) mod)))
        (else
          (resolve-lexical n (or (and (syntax? id) (syntax-module id)) mod))))))
  (define transformer-environment
    (make-fluid
      (lambda (k)
        (assertion-violation 'transformer-environment "called outside the dynamic extent of a syntax transformer"))))
  (define (with-transformer-environment k)
    ((fluid-ref transformer-environment) k))

  (define (free-id=? i j)
    (let* ((mi (and (syntax? i) (syntax-module i)))
           (mj (and (syntax? j) (syntax-module j)))
           (ni (id-var-name i empty-wrap mi))
           (nj (id-var-name j empty-wrap mj)))
      (define (id-module-binding id mod)
        (module-variable
          (if mod (resolve-module (cdr mod) #t #t) (current-module))
          (id-sym-name id)))

      (cond
        ((syntax? ni) (free-id=? ni j))
        ((syntax? nj) (free-id=? i nj))
        ((symbol? ni)
          (and (eq? nj (id-sym-name j))
            (let ((bi (id-module-binding i mi))
                  (bj (id-module-binding j mj)))
              (and (eq? bi bj)
                (or bi (eq? ni nj))))))
        (else

          (equal? ni nj)))))

  (define (bound-id=? i j)
    (if (and (syntax? i) (syntax? j))
      (and (eq? (syntax-expression i) (syntax-expression j))
        (same-marks? (wrap-marks (syntax-wrap i)) (wrap-marks (syntax-wrap j))))
      (eq? i j)))

  (define (valid-bound-ids? ids)
    (and (let all-ids? ((ids ids))
          (cond
            ((null? ids) #t)
            ((pair? ids)
              (let ((id (car ids)) (ids (cdr ids)))
                (and (id? id) (all-ids? ids))))
            (else #f)))))
  (define (distinct-bound-ids? ids)
    (let distinct? ((ids ids))
      (cond
        ((null? ids) #t)
        ((pair? ids)
          (let ((id (car ids)) (ids (cdr ids)))
            (and (not (bound-id-member? id ids)) (distinct? ids)))))))

  (define (bound-id-member? x ids)
    (if (null? ids)
      #f
      (or (bound-id=? x (car ids))
        (bound-id-member? x (cdr ids)))))

  (define (wrap x w defmod) (source-wrap x w #f defmod))
  (define (wrap-syntax x w defmod)
    (make-syntax (syntax-expression x) w (or (syntax-module x) defmod) (syntax-sourcev x)))
  (define (source-wrap x w s defmod)
    (cond
      ((and (null? (wrap-marks w))
          (null? (wrap-subst w))
          (not defmod)
          (not s))
        x)
      ((syntax? x) (wrap-syntax x (join-wraps w (syntax-wrap x)) defmod))
      ((null? x) x)
      (else (make-syntax x w defmod s))))
  (define (expand-sequence body r w s mod)
    (build-sequence s
      (let lp ((body body))
        (if (null? body) '()
          (let ((head (car body)) (tail (cdr body)))
            (let ((expr (expand head r w mod)))
              (cons expr (lp tail))))))))

  (define (expand-top-sequence body r w s m essew mod)
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
      (define (record-definition! id var)
        (let ((mod (cons 'hygiene (module-name (current-module)))))
          (extend-ribcage! ribcage id
            (cons (or (syntax-module id) mod)
              (wrap var top-wrap mod)))))
      (define (macro-introduced-identifier? id)
        (not (equal? (wrap-marks (syntax-wrap id)) top-mark)))
      (define (ensure-fresh-name var)
        (define (ribcage-has-var? var)
          ;; check if ribcage contains var.
          ;; Each label is (_ . wrapped) where wrapped is syntax-object
          ;; with expression being the variable.
          (let lp ((labels (ribcage-labels ribcage)))
            (and (pair? labels)
              (let ((entry (car labels)))
                (or (and (pair? entry)
                     (eq? (syntax-expression (cdr entry)) var))
                  (lp (cdr labels)))))))

        (let lp ((unique var) (n 1))
          (if (ribcage-has-var? unique)
            (let ((tail (string->symbol (number->string n))))
              (lp (symbol-append var '- tail) (+ 1 n)))
            unique)))
      (define (fresh-derived-name id orig-form)
        (ensure-fresh-name
          (symbol-append
            (syntax-expression id)
            '-
            (string->symbol
              (number->string
                (hash (syntax->datum orig-form)))))))

      (define (parse body r w s m esew mod)
        (let loop ((body body))
          (cond
            ((null? body) '())
            (else
              (let ((head (car body)) (tail (cdr body)))
                (let ((thunks (parse1 head r w s m essew mod)))
                  (append thunks (loop tail))))))))

      (define (parse1 x r w s m esew mod)
        (define (current-module-for-expansion mod)
          (cond
            ((and (pair? mod) (eq? (car mod) 'hygiene))
              (cons 'hygiene (module-name (current-module))))
            (else mod)))

        (call-with-values
          (lambda ()
            (let ((mod (current-module-for-expansion mod)))
              (syntax-type x r w (source-annotation x) ribcage mod #f)))
          (lambda (type value form e w s mod)
            (case type
              ((define-form)
                (let* ((id (wrap value w mod))
                       (var (if (macro-introduced-identifier? id)
                             (fresh-derived-name id x)
                             (syntax-expression id))))
                  (record-definition! id var)
                  (list (if (eq? m 'c&e)
                         (let ((x (build-global-definition s mod var (expand e r w mod))))
                           (top-level-eval x mod)
                           (lambda () x))
                         (call-with-values
                           (lambda () (resolve-identifier id empty-wrap r mod #t))
                           (lambda (type* value* mod*)
                             (if (eq? type* 'macro)
                               (top-level-eval
                                 (build-global-definition s mod var (build-void s))
                                 mod))
                             (lambda ()
                               (build-global-definition s mod var (expand e r w mod)))))))))
              ((begin-form)
                (syntax-case e ()
                  ((_ exp ...)
                    (parse #'(exp ...) r w s m essew mod))))
              ((eval-when-form)
                (syntax-case e () 
                  [(_ (x ...) e1 e2 ...)
                    (let* ([when-list (parse-when-list e #'(x ...))]
                           [body #'(e1 e2 ...)])
                      (define (recurse m essew)
                        (parse body r w s m essew mod))
                      (cond
                        ((eq? m 'e)
                          (if (memq 'eval when-list)
                            (recurse
                              (if (memq 'expand when-list) 'c&e 'e)
                              '(eval))
                            (begin
                              (if (memq 'expand when-list)
                                (top-level-eval
                                  (expand-top-sequence body r w s 'e '(eval) mod)
                                  mod))
                              '())))
                        ((memq 'load when-list)
                          (cond
                            ((or (memq 'compile when-list)
                                (memq 'expand when-list)
                                (and (eq? m 'c&e) (memq 'eval when-list)))
                              (recurse 'c&e '(compile load)))
                            ((memq m '(c c&e)) (recurse 'c '(load)))
                            (else '())))
                        ((or (memq 'compile when-list)
                            (memq 'expand when-list)
                            (and (eq? m 'c&e) (memq 'eval when-list)))
                          (top-level-eval
                            (expand-top-sequence body r w s 'e '(eval) mod)
                            mod)
                          '())
                        (else '())))]))

              ((define-syntax-form define-syntax-parameter-form)
                (let* ((id (wrap value w mod))
                       (var (if (macro-introduced-identifier? id)
                             (fresh-derived-name id x)
                             (syntax-expression id))))
                  (record-definition! id var)
                  (let ((key m))
                    (cond
                      ((memv key '(c))
                        (cond
                          ((memq 'compile esew)
                            (let ((e (expand-install-global mod var type (expand e r w mod) id)))
                              (top-level-eval e mod)
                              (if (memq 'load esew) (list (lambda () e)) '())))
                          ((memq 'load esew)
                            (list (lambda ()
                                   (expand-install-global mod var type (expand e r w mod) id))))
                          (else '())))
                      ((memv key '(c&e))
                        (let ((e (expand-install-global mod var type (expand e r w mod) id)))
                          (top-level-eval e mod)
                          (list (lambda () e))))
                      (else (if (memq 'eval esew)
                             (top-level-eval
                               (expand-install-global mod var type (expand e r w mod) id)
                               mod))
                        '())))))
              (else
                (list
                  (if (eq? m 'c&e)
                    (let ((x (expand-expr type value form e r w s mod)))
                      (top-level-eval x mod)
                      (lambda () x))
                    (lambda ()
                      (expand-expr type value form e r w s mod)))))))))
      (let ((res (let lp ((thunks (parse body r w s m essew mod)))
                  (if (null? thunks)
                    '()
                    (cons ((car thunks)) (lp (cdr thunks)))))))
        (if (null? res) (build-void s) (build-sequence s res)))))

  (define (parse-define-property e w s mod)
    (syntax-case e () 
      [(_ name prop expr)
        (and (id? #'name) (id? #'prop))
        (values #'name #'prop #'expr w mod)]
      [_ (syntax-violation 'define-property "bad form" (source-wrap e w s mod))]))

  (define (expand-macro p e r w s rib mod)
    (define transformer (car p))
    (define transformer-stx (cdr p))
    (define (decorate-source x)
      (source-wrap x empty-wrap s #f))
    (define (map* f x)
      (match x
        [() '()]
        [(x . x*) (cons (f x) (map* f x*))]
        [x (f x)]))

    (define (rebuild-macro-output x m)
      (cond
        ((pair? x) (decorate-source (map* (lambda (x) (rebuild-macro-output x m)) x)))
        ((syntax? x)
          (let ((w (syntax-wrap x)))
            (let ((ms (wrap-marks w)) (ss (wrap-subst w)))
              (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                (wrap-syntax x (make-wrap (cdr ms) (if rib (cons rib (cdr ss)) (cdr ss))) mod)
                (wrap-syntax
                  x
                  (make-wrap (cons m ms) (if rib (cons rib (cons 'shift ss)) (cons 'shift ss)))
                  mod)))))
        ((vector? x)
          (let* ((n (vector-length x)) (v (make-vector n)))
            (let loop ((i 0))
              (if (= i n)
                (begin (if #f #f) v)
                (begin
                  (vector-set! v i (rebuild-macro-output (vector-ref x i) m))
                  (loop (+ i 1)))))
            (decorate-source v)))
        ((symbol? x)
          (syntax-violation
            #f
            (format "encountered raw symbol '~a' in macro output" x)
            e
            x))
        (else (decorate-source x))))
    (define (apply-transformer transform e)
      (rebuild-macro-output
        (transform e)
        (new-mark)))

    (let ((old (fluid-ref transformer-environment)))
      (dynamic-wind
        (lambda () (fluid-set! transformer-environment (lambda (k) (k e r w s rib mod))))
        (lambda ()
          (cond
            ((procedure? transformer) (apply-transformer transformer (source-wrap e (anti-mark w) s mod)))
            ((variable-transformer? transformer) (apply-transformer (variable-transformer-procedure transformer) (source-wrap e (anti-mark w) s mod)))

            (else (syntax-violation #f "invalid transformer" p))))
        ;(lambda () (rebuild-macro-output (p (source-wrap e (anti-mark w) s mod)) (new-mark)))
        (lambda () (fluid-set! transformer-environment old)))))

  (define (eval-local-transformer expanded mod)
    (let ((p (local-eval expanded mod)))
      (if (not (or (procedure? p) (variable-transformer? p))) (syntax-violation #f "nonprocedure transformer" p))
      p))
  (define (expand-local-syntax rec? e r w s mod k)
    (syntax-case e () 
      [(_ ((id val) ...) body ...)
        (let ((ids #'(id ...)))
          (unless (valid-bound-ids? ids)
            (syntax-violation #f "duplicate bound keyword" e))
          (define labels (gen-labels ids))
          (define new-w (make-binding-wrap ids labels w))
          (k #'(body ...)
            (extend-env 
              labels 
              (let ([w (if rec? new-w w)] [trans-r (macros-only-env r)])
                (map (lambda (x)
                    (cons 'macro (cons (eval-local-transformer (expand x trans-r w mod) mod) (wrap x w '(hygiene capy)))))
                  #'(val ...)))
              r)
            new-w
            s mod))]
      [_ (syntax-violation #f "invalid local-syntax form" e)]))

  (define (parse-when-list e when-list)
    (let ((result (strip when-list)))
      (let lp ((l result))
        (match l
          (() result)
          ((x . l)
           (match x
             ((or 'compile 'load 'eval 'expand) (lp l))
             (_ (syntax-violation 'eval-when "invalid situation" e x))))))))

  (define (lambda-var-list vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
      (cond
        ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
        ((id? vars) (cons (wrap vars w #f) ls))
        ((null? vars) ls)
        ((syntax? vars) (lvl (syntax-expression vars) ls (join-wraps w (syntax-wrap vars))))
        (else (cons vars ls)))))
  (define (syntax-type e r w s rib mod for-car?)
    (cond
      ((symbol? e)
        (receive (type value mod*) (resolve-identifier e w r mod #t)
          (case type 
            ((macro)
              (if for-car? 
                (values type value e e w s mod)
                (syntax-type (expand-macro value e r w s rib mod) r empty-wrap s rib mod #f)))
            ((global)
              (values type value e value w s mod*))
            (else (values type value e e w s mod)))))
      ((pair? e)
        (define first (car e))
        (receive (ftype fval fform fe fw fs fmod) (syntax-type first r w s rib mod #t)
          (case ftype
            ((lexical) (values 'lexical-call fval e e w s mod))
            ((global)
              (if (equal? fmod '(primitive))
                (values 'primitive-call fval e e w s mod)
                (values 'global-call (make-syntax fval w fmod fs) e e w s mod)))
            ((macro)
              (syntax-type (expand-macro fval e r w s rib mod)
                r
                empty-wrap
                s
                rib
                mod
                for-car?))
            ((module-ref)
              (call-with-values (lambda () (fval e r w mod))
                (lambda (e r w s mod) (syntax-type e r w s rib mod for-car?))))
            ((core) (values 'core-form fval e e w s mod))
            ((local-syntax) (values 'local-syntax-form fval e e w s mod))
            ((begin) (values 'begin-form #f e e w s mod))
            ((eval-when) (values 'eval-when-form #f e e w s mod))
            ((define-property)
              (values 'define-property-form #f e e w s mod))
            ((define-syntax)
              (syntax-case e () 
                [(_ name val)
                  (id? #'name)
                  (values 'define-syntax-form #'name e #'val w s mod)]))
            ((define-syntax-parameter)
              (syntax-case e () 
                [(_ name val)
                  (id? #'name)
                  (values 'define-syntax-parameter-form #'name e #'val w s mod)]))
            ((define)
              (syntax-case e () 
                [(_ name val)
                  (id? #'name)
                  (values 'define-form #'name e #'val w s mod)]
                [(_ (name . args) e1 e2 ...)
                  (id? #'name)
                  (values 
                    'define-form
                    (wrap #'name w mod)
                    (wrap e w mod)
                    (source-wrap
                        (cons #'lambda (wrap #'(args e1 e2 ...) w mod))
                        empty-wrap s #f)
                    empty-wrap
                    s
                    mod)]
                [(_ name)
                  (id? #'name)
                  (values 
                    'define-form
                    (wrap #'name w mod)
                    (wrap e w mod)
                    (list (make-syntax 'if top-wrap '(hygiene capy)) #f #f)
                    empty-wrap
                    s
                    mod)]))
            (else (values 'call #f e e w s mod)))))
      ((syntax? e)
        (syntax-type (syntax-expression e)
          r
          (join-wraps w (syntax-wrap e))
          (or (source-annotation e) s)
          rib
          (or (syntax-module e) mod)
          for-car?))
      ((self-evaluating? e) (values 'constant #f e e w s mod))
      (else (values 'other #f e e w s mod))))

  (define (expand-install-global mod name type e id)
    (build-global-definition
      #f
      mod
      name
      (build-primcall
        #f
        'make-syntax-transformer
        (list (make-constant #f name)
          (make-constant #f
            (case type 
              ((define-syntax-parameter-form) 'syntax-parameter)
              ((define-property-form) 'property)
              (else 'macro)))
          (build-primcall #f 'cons (list e (make-constant #f id)))))))
  (define (expand e r w mod)
;    (cond 
;      [(and (syntax-pair? e)
;            (identifier? (syntax-car e)))
;        (define id (syntax-car e))
;        (receive (type* value* mod*) (resolve-identifier id w r mod #t)
;          (case type* 
;            [(macro)
;              (expand (expand-macro value* e r w #f #f mod) r empty-wrap mod)]
;            [(core core-form)
;              (value* e r w #f mod)]
;            [else 
;              (expand-implicit app-sym value* e r w mod)]))]
;      [else 
;        (receive (type value form e w s mod)
;          (expand-expr type value form e r w s mod))])
    (receive (type value form e w s mod)
      (syntax-type e r w (source-annotation e) #f mod #f)
      (expand-expr type value form e r w s mod)))
;    (call-with-values
;      (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
;      (lambda (type value form e w s mod)
;        (expand-expr type value form e r w s mod))))

  (define (make-explicit sym e r w s mod)
    (define sym-s (source-wrap sym w s mod))
    (define new-s (make-syntax (cons sym e) w mod s))
    new-s)

  (define (expand-expr type value form e r w s mod)
    (cond
      ((eq? type 'define-property-form)
        (parse-define-property e w s mod)
        (syntax-violation #f "invalid context for definition" (source-wrap e w s mod)))
      ((eq? type 'lexical)
        (build-lexical-reference s e value))
      ((or (eq? type 'core) (eq? type 'core-form))
        (value e r w s mod))
      ((eq? type 'module-ref)
        (call-with-values (lambda () (value e r w mod))
          (lambda (e r w s mod)
            (expand e r w mod))))
      ((eq? type 'lexical-call)
        (expand-call
          (let ((id (car e)))
            (build-lexical-reference (source-annotation id)
              (if (syntax? id) (syntax->datum id) id)
              value))
          e
          r
          w
          s
          mod))
      ((eq? type 'global-call)
        (expand-call
          (build-global-reference (or (source-annotation (car e)) s)
            (if (syntax? value)
              (syntax-expression value)
              value)
            (or (and (syntax? value)
                 (syntax-module value))
              mod))
          e
          r
          w
          s
          mod))
      ((eq? type 'primitive-call)
        (build-primcall s
          value
          (map (lambda (e) (expand e r w mod)) (cdr e))))
      ((eq? type 'global) (build-global-reference s value mod))
      ((eq? type 'constant) (make-constant s (strip e)))
      ((eq? type 'call) (expand-call (expand (car e) r w mod) e r w s mod))
      ((eq? type 'eval-when-form)
        (syntax-case e () 
          [(_ (x ...) e1 e2 ...)
            (let ((when-list (parse-when-list e #'(x ...))))
              (case when-list 
                ((eval) (expand-sequence #'(e1 e2 ...) r w s mod))
                (else (expand-void))))]))
      ((eq? type 'begin-form)
        (syntax-case e () 
          [(_ e1 e2 ...)
            (expand-sequence #'(e1 e2 ...) r w s mod)]
          [(_) (syntax-violation #f "sequence with no expressions" (source-wrap e w s mod))]))
      ((memq type '(define-form define-syntax-form define-syntax-parameter-form define-property-form))
        (syntax-violation #f "definition in expression context, where definitions are not allowed" (source-wrap e w s mod)))
      ((eq? type 'local-syntax-form)
        (expand-local-syntax value e r w s mod expand-sequence))
      ((eq? type 'syntax)
        (syntax-violation #f "reference to pattern variable outside syntax form" (source-wrap e w s mod)))
      ((eq? type 'displaced-lexical)
        (syntax-violation #f "reference to identifier outside its scope" (source-wrap e w s mod)))
      (else (syntax-violation #f "unexpected syntax" type (source-wrap e w s mod)))))
      
  (define (expand-call x e r w s mod)
    (syntax-case e () 
      [(e0 e1 ...)
        (build-call s x (map (lambda (e) (expand e r w mod)) #'(e1 ...)))]))

 

  (define (expand-body body outer-form r w mod)

    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
      (let parse ((body (map (lambda (x) (cons r (wrap x w mod))) body))
                  (ids '())
                  (labels '())
                  (var-ids '())
                  (vars '())
                  (vals '())
                  (bindings '())
                  (expand-tail-expr #f))
        (cond
          ((null? body)
            (if (not expand-tail-expr)
              (if (null? ids)
                (syntax-violation #f "empty body" outer-form)
                (syntax-violation #f "body should end with an expression" outer-form)))
            (if (not (valid-bound-ids? ids))
              (syntax-violation #f "invalid or duplicate identifier in definition" outer-form))
            (set-cdr! r (extend-env labels bindings (cdr r)))
            (let ((src (source-annotation outer-form)))
              (let lp ((var-ids var-ids) (vars vars) (vals vals) (tail (expand-tail-expr)))
                (cond
                  ((null? var-ids) tail)
                  ((not (car var-ids))
                    (lp (cdr var-ids) (cdr vars) (cdr vals) (make-sequence src ((car vals)) tail)))
                  (else
                    (let ((var-ids (map (lambda (id)
                                         (if id (syntax->datum id) '_))
                                    (reverse var-ids)))
                          (vars (map (lambda (var) (or var (gen-lexical '_))) (reverse vars)))
                          (vals (map (lambda (expand-expr id)
                                      (if id
                                        (expand-expr)
                                        (make-sequence
                                          src
                                          (expand-expr)
                                          (make-void #f))))
                                 (reverse vals)
                                 (reverse var-ids))))

                      (build-letrec* src var-ids vars vals tail)))))))
          (expand-tail-expr
            (parse body ids labels (cons #f var-ids) (cons #f vars) (cons expand-tail-expr vals) bindings #f))
          (else
            (let ((e (cdar body)) (er (caar body)) (body (cdr body)))
              (call-with-values
                (lambda () (syntax-type e er empty-wrap (source-annotation e) ribcage mod #f))
                (lambda (type value form e w s mod)
                  (cond
                    ((eq? type 'define-form)
                      (define id (wrap value w mod))
                      (define label (gen-label))
                      (define var (gen-var id))

                      (extend-ribcage! ribcage id label)
                      (parse body
                        (cons id ids)
                        (cons label labels)
                        (cons id var-ids)
                        (cons var vars)
                        (cons (let ((wrapped (source-wrap e w s mod)))
                               (lambda () (expand wrapped er empty-wrap mod)))
                          vals)
                        (cons (cons 'lexical var) bindings)
                        #f))
                    ((eq? type 'begin-form)
                      (syntax-case e () 
                        [(_ es ...)
                          (parse (let f ((forms #'(es ...)))
                                    (if (null? forms)
                                      body 
                                      (cons (cons er (wrap (car forms) w mod))
                                        (f (cdr forms)))))
                                 ids 
                                 labels
                                 var-ids
                                 vars
                                 vals
                                 bindings 
                                 #f)]))
                    ((eq? type 'define-syntax-form)
                      (define id (wrap value w mod))
                      (define label (gen-label))
                      (define trans-r (macros-only-env er))
                      (extend-ribcage! ribcage id label)
                      (set-cdr!
                        r
                        (extend-env
                          (list label)
                          (list (cons 'macro (cons (eval-local-transformer (expand e trans-r w mod) mod) id)))
                          (cdr r)))
                      (parse body (cons id ids) labels var-ids vars vals bindings #f))
                    ((eq? type 'define-syntax-parameter-form)
                      (define id (wrap value w mod))
                      (define label (gen-label))
                      (define trans-r (macros-only-env er))

                      (extend-ribcage! ribcage id label)
                      (set-cdr!
                        r
                        (extend-env
                          (list label)
                          (list (cons 'syntax-parameter
                                 (eval-local-transformer (expand e trans-r w mod) mod)))
                          (cdr r)))
                      (parse body (cons id ids) labels var-ids vars vals bindings #f))
                    ((eq? type 'local-syntax-form)
                      (expand-local-syntax
                        value
                        e
                        er
                        w
                        s
                        mod
                        (lambda (forms er w s mod)
                          (parse (let f ((forms forms))
                                  (if (null? forms)
                                    body
                                    (cons (cons er (wrap (car forms) w mod)) (f (cdr forms)))))
                            ids
                            labels
                            var-ids
                            vars
                            vals
                            bindings
                            #f))))
                    
                    (else
                      (let ((wrapped (source-wrap e w s mod)))
                        (parse body ids labels var-ids vars vals bindings
                          (lambda ()
                            (expand wrapped er empty-wrap mod))))))))))))))
  (define (gen-var id)
    (let ((id (if (syntax? id) (syntax-expression id) id)))
      (gen-lexical id)))
  (define (strip x)
    (cond
      ((syntax? x) (strip (syntax-expression x)))
      ((pair? x) (cons (strip (car x)) (strip (cdr x))))
      ((vector? x) (list->vector (strip (vector->list x))))
      (else x)))

  (define (ellipsis? e r mod)
    (and (nonsymbol-id? e)
      (call-with-values
        (lambda ()
          (resolve-identifier
            (make-syntax '$sc-ellipsis (syntax-wrap e) (or (syntax-module e) mod) #f)
            empty-wrap
            r
            mod
            #f))
        (lambda (type value mod)
          (if (eq? type 'ellipsis)
            (bound-id=? e value)
            (free-id=? e (make-syntax '... top-wrap '(hygiene capy))))))))

  (define (lambda-formals orig-args)
    (define (req args rreq)
      (syntax-case args ()
        [() (check (reverse rreq) #f)]
        [(a . b) (id? #'a)
          (req #'b (cons #'a rreq))]
        [r (id? #'r)
          (check (reverse rreq) #'r)]
        [else
          (syntax-violation 'lambda "invalid argument list" orig-args args)]))
    (define (check req rest)
      (cond
        [(distinct-bound-ids? (if rest (cons rest req) req))
          (values req #f rest #f)]
        [else
          (syntax-violation 'lambda "duplicate argument names" orig-args)]))
    (req orig-args '()))

  (define (expand-public-ref e r w mod)
    (syntax-case e ()
      [(_ (mod ...) id)
        (and (and-map id? #'(mod ...)) (id? #'id))
        (values
          (syntax->datum #'id)
          r
          top-wrap
          #f
          (syntax->datum
            #'(public mod ...)))]))

  (define (expand-private-ref e r w mod)
    (letrec* ((remodulate
                (lambda (x mod)
                  (cond
                    ((pair? x) (cons (remodulate (car x) mod) (remodulate (cdr x) mod)))
                    ((syntax? x)
                      (make-syntax
                        (remodulate (syntax-expression x) mod)
                        (syntax-wrap x)
                        mod
                        (syntax-sourcev x)))
                    ((vector? x)
                      (let* ((n (vector-length x)) (v (make-vector n)))
                        (let loop ((i 0))
                          (if (= i n)
                            (begin (if #f #f) v)
                            (begin (vector-set! v i (remodulate (vector-ref x i) mod)) (loop (+ 1 i)))))))
                    (else x)))))
      (let* ((tmp e)
             (tmp-1 ($sc-dispatch
                     tmp
                     (list '_ (vector 'free-id (make-syntax 'primitive top-wrap '(hygiene capy))) 'any))))
        (if (and tmp-1
             (apply (lambda (id)
                     (and (id? id)
                       (equal? (cdr (or (and (syntax? id) (syntax-module id)) mod)) '(capy))))
               tmp-1))
          (apply (lambda (id) (values (syntax->datum id) r top-wrap #f '(primitive))) tmp-1)
          (let ((tmp-1 ($sc-dispatch tmp '(_ each-any any))))
            (if (and tmp-1 (apply (lambda (mod id) (and (and-map id? mod) (id? id))) tmp-1))
              (apply (lambda (mod id)
                      (values
                        (syntax->datum id)
                        r
                        top-wrap
                        #f
                        (syntax->datum (cons (make-syntax 'private top-wrap '(hygiene capy)) mod))))
                tmp-1)
              (let ((tmp-1 ($sc-dispatch
                            tmp
                            (list '_
                              (vector 'free-id (make-syntax '@@ top-wrap '(hygiene capy)))
                              'each-any
                              'any))))
                (if (and tmp-1 (apply (lambda (mod exp) (and-map id? mod)) tmp-1))
                  (apply (lambda (mod exp)
                          (let ((mod (syntax->datum
                                      (cons (make-syntax 'private top-wrap '(hygiene capy)) mod))))
                            (values (remodulate exp mod) r w (source-annotation exp) mod)))
                    tmp-1)
                  (syntax-violation #f "source expression failed to match any pattern" tmp)))))))))




  (define expand-syntax
    (letrec* ((gen-syntax
                (lambda (src e r maps ellipsis? mod)
                  (if (id? e)
                    (call-with-values
                      (lambda () (resolve-identifier e empty-wrap r mod #f))
                      (lambda (type value mod)
                        (let ((key type))
                          (cond
                            ((memv key '(syntax))
                              (call-with-values
                                (lambda () (gen-ref src (car value) (cdr value) maps))
                                (lambda (var maps) (values (list 'ref var) maps))))
                            ((ellipsis? e r mod) (syntax-violation 'syntax "misplaced ellipsis" src))
                            (else (values (list 'quote e) maps))))))
                    (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(any any))))
                      (if (and tmp-1 (apply (lambda (dots e) (ellipsis? dots r mod)) tmp-1))
                        (apply (lambda (dots e) (gen-syntax src e r maps (lambda (e r mod) #f) mod)) tmp-1)
                        (let ((tmp-1 ($sc-dispatch tmp '(any any . any))))
                          (if (and tmp-1 (apply (lambda (x dots y) (ellipsis? dots r mod)) tmp-1))
                            (apply (lambda (x dots y)
                                    (let f ((y y)
                                            (k (lambda (maps)
                                                (call-with-values
                                                  (lambda ()
                                                    (gen-syntax src x r (cons '() maps) ellipsis? mod))
                                                  (lambda (x maps)
                                                    (if (null? (car maps))
                                                      (syntax-violation 'syntax "extra ellipsis" src)
                                                      (values (gen-map x (car maps)) (cdr maps))))))))
                                      (let* ((tmp y) (tmp ($sc-dispatch tmp '(any . any))))
                                        (if (and tmp
                                             (apply (lambda (dots y) (ellipsis? dots r mod)) tmp))
                                          (apply (lambda (dots y)
                                                  (f y
                                                    (lambda (maps)
                                                      (call-with-values
                                                        (lambda () (k (cons '() maps)))
                                                        (lambda (x maps)
                                                          (if (null? (car maps))
                                                            (syntax-violation
                                                              'syntax
                                                              "extra ellipsis"
                                                              src)
                                                            (values
                                                              (gen-mappend x (car maps))
                                                              (cdr maps))))))))
                                            tmp)
                                          (call-with-values
                                            (lambda () (gen-syntax src y r maps ellipsis? mod))
                                            (lambda (y maps)
                                              (call-with-values
                                                (lambda () (k maps))
                                                (lambda (x maps) (values (gen-append x y) maps)))))))))
                              tmp-1)
                            (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                              (if tmp-1
                                (apply (lambda (x y)
                                        (call-with-values
                                          (lambda () (gen-syntax src x r maps ellipsis? mod))
                                          (lambda (x maps)
                                            (call-with-values
                                              (lambda () (gen-syntax src y r maps ellipsis? mod))
                                              (lambda (y maps) (values (gen-cons x y) maps))))))
                                  tmp-1)
                                (let ((tmp-1 ($sc-dispatch tmp '#(vector (any . each-any)))))
                                  (if tmp-1
                                    (apply (lambda (e1 e2)
                                            (call-with-values
                                              (lambda ()
                                                (gen-syntax src (cons e1 e2) r maps ellipsis? mod))
                                              (lambda (e maps) (values (gen-vector e) maps))))
                                      tmp-1)
                                    (let ((tmp-1 (list tmp)))
                                      (if (and tmp-1
                                           (apply (lambda (x) (eq? (syntax->datum x) nil)) tmp-1))
                                        (apply (lambda (x) (values nil maps)) tmp-1)
                                        (let ((tmp ($sc-dispatch tmp '())))
                                          (if tmp
                                            (apply (lambda () (values ''() maps)) tmp)
                                            (values (list 'quote e) maps))))))))))))))))
              (gen-ref
                (lambda (src var level maps)
                  (cond
                    ((= level 0) (values var maps))
                    ((null? maps) (syntax-violation 'syntax "missing ellipsis" src))
                    (else (call-with-values
                           (lambda () (gen-ref src var (- level 1) (cdr maps)))
                           (lambda (outer-var outer-maps)
                             (let ((b (assq outer-var (car maps))))
                               (if b
                                 (values (cdr b) maps)
                                 (let ((inner-var (gen-var 'tmp)))
                                   (values
                                     inner-var
                                     (cons (cons (cons outer-var inner-var) (car maps)) outer-maps)))))))))))
              (gen-mappend (lambda (e map-env) (list 'apply '(primitive append) (gen-map e map-env))))
              (gen-map
                (lambda (e map-env)
                  (let ((formals (map cdr map-env)) (actuals (map (lambda (x) (list 'ref (car x))) map-env)))
                    (cond
                      ((eq? (car e) 'ref) (car actuals))
                      ((and-map (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals))) (cdr e))
                        (cons 'map
                          (cons (list 'primitive (car e))
                            (map (let ((r (map cons formals actuals)))
                                  (lambda (x) (cdr (assq (cadr x) r))))
                              (cdr e)))))
                      (else (cons 'map (cons (list 'lambda formals e) actuals)))))))
              (gen-cons
                (lambda (x y)
                  (let ((key (car y)))
                    (cond
                      ((memv key '(quote))
                        (cond
                          ((eq? (car x) 'quote) (list 'quote (cons (cadr x) (cadr y))))
                          ((eq? (cadr y) '()) (list 'list x))
                          (else (list 'cons x y))))
                      ((memv key '(list)) (cons 'list (cons x (cdr y))))
                      (else (list 'cons x y))))))
              (gen-append (lambda (x y) (if (equal? y ''()) x (list 'append x y))))
              (gen-vector
                (lambda (x)
                  (cond
                    ((eq? (car x) 'list) (cons 'vector (cdr x)))
                    ((eq? (car x) 'quote) (list 'quote (list->vector (cadr x))))
                    (else (list 'list->vector x)))))
              (regen (lambda (x)
                      (let ((key (car x)))
                        (cond
                          ((memv key '(ref)) (build-lexical-reference no-source (cadr x) (cadr x)))
                          ((memv key '(primitive)) (build-primref no-source (cadr x)))
                          ((memv key '(quote)) (build-data no-source (cadr x)))
                          ((memv key '(lambda))
                            (if (list? (cadr x))
                              (build-simple-lambda no-source (cadr x) (cadr x) '() (regen (caddr x)))
                              (error #f "how did we get here" x)))
                          (else (build-primcall no-source (car x) (map regen (cdr x)))))))))
      (lambda (e r w s mod)
        (let* ((e (source-wrap e w s mod)) (tmp e) (tmp ($sc-dispatch tmp '(_ any))))
          (if tmp
            (apply (lambda (x)
                    (call-with-values
                      (lambda () (gen-syntax e x r '() ellipsis? mod))
                      (lambda (e maps) (regen e))))
              tmp)
            (syntax-violation 'syntax "bad `syntax' form" e))))))
  (define expand-syntax-case
    (letrec* ((convert-pattern
                (lambda (pattern keys ellipsis?)
                  (letrec* ((cvt* (lambda (p* n ids)
                                   (let* ((tmp p*) (tmp ($sc-dispatch tmp '(any . any))))
                                     (if tmp
                                       (apply (lambda (x y)
                                               (call-with-values
                                                 (lambda () (cvt* y n ids))
                                                 (lambda (y ids)
                                                   (call-with-values
                                                     (lambda () (cvt x n ids))
                                                     (lambda (x ids) (values (cons x y) ids))))))
                                         tmp)
                                       (cvt p* n ids)))))
                            (v-reverse
                              (lambda (x)
                                (let loop ((r '()) (x x))
                                  (if (not (pair? x)) (values r x) (loop (cons (car x) r) (cdr x))))))
                            (cvt (lambda (p n ids)
                                  (if (id? p)
                                    (cond
                                      ((bound-id-member? p keys) (values (vector 'free-id p) ids))
                                      ((free-id=? p (make-syntax '_ top-wrap '(hygiene capy)))

                                        (values '_ ids))
                                      (else (values 'any (cons (cons p n) ids))))
                                    (let* ((tmp p) (tmp-1 ($sc-dispatch tmp '(any any))))
                                      (if (and tmp-1 (apply (lambda (x dots) (ellipsis? dots)) tmp-1))
                                        (apply (lambda (x dots)
                                                (call-with-values
                                                  (lambda () (cvt x (+ 1 n) ids))
                                                  (lambda (p ids)
                                                    (values
                                                      (if (eq? p 'any) 'each-any (vector 'each p))
                                                      ids))))
                                          tmp-1)
                                        (let ((tmp-1 ($sc-dispatch tmp '(any any . any))))
                                          (if (and tmp-1
                                               (apply (lambda (x dots ys) (ellipsis? dots)) tmp-1))
                                            (apply (lambda (x dots ys)
                                                    (call-with-values
                                                      (lambda () (cvt* ys n ids))
                                                      (lambda (ys ids)
                                                        (call-with-values
                                                          (lambda () (cvt x (+ n 1) ids))
                                                          (lambda (x ids)
                                                            (call-with-values
                                                              (lambda () (v-reverse ys))
                                                              (lambda (ys e)
                                                                (values (vector 'each+ x ys e) ids))))))))
                                              tmp-1)
                                            (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                                              (if tmp-1
                                                (apply (lambda (x y)
                                                        (call-with-values
                                                          (lambda () (cvt y n ids))
                                                          (lambda (y ids)
                                                            (call-with-values
                                                              (lambda () (cvt x n ids))
                                                              (lambda (x ids) (values (cons x y) ids))))))
                                                  tmp-1)
                                                (let ((tmp-1 ($sc-dispatch tmp '())))
                                                  (if tmp-1
                                                    (apply (lambda () (values '() ids)) tmp-1)
                                                    (let ((tmp-1 ($sc-dispatch
                                                                  tmp
                                                                  '#(vector each-any))))
                                                      (if tmp-1
                                                        (apply (lambda (x)
                                                                (call-with-values
                                                                  (lambda () (cvt x n ids))
                                                                  (lambda (p ids)
                                                                    (values (vector 'vector p) ids))))
                                                          tmp-1)
                                                        (let ((x tmp))
                                                          (values (vector 'atom (strip p)) ids))))))))))))))))
                    (cvt pattern 0 '()))))
              (build-dispatch-call
                (lambda (pvars exp y r mod)
                  (let ((ids (map car pvars)) (levels (map cdr pvars)))
                    (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
                      (build-primcall
                        no-source
                        'apply
                        (list (build-simple-lambda
                               no-source
                               (map syntax->datum ids)
                               new-vars
                               '()
                               (expand
                                 exp
                                 (extend-env
                                   labels
                                   (map (lambda (var level) (cons 'syntax (cons var level)))
                                     new-vars
                                     (map cdr pvars))
                                   r)
                                 (make-binding-wrap ids labels empty-wrap)
                                 mod))
                          y))))))
              (gen-clause
                (lambda (x keys clauses r pat fender exp mod)
                  (call-with-values
                    (lambda () (convert-pattern pat keys (lambda (e) (ellipsis? e r mod))))
                    (lambda (p pvars)
                      (cond
                        ((not (and-map (lambda (x) (not (ellipsis? (car x) r mod))) pvars))
                          (syntax-violation 'syntax-case "misplaced ellipsis" pat))
                        ((not (distinct-bound-ids? (map car pvars)))
                          (syntax-violation 'syntax-case "duplicate pattern variable" pat))
                        (else (let ((y (gen-var 'tmp)))
                               (build-call
                                 no-source
                                 (build-simple-lambda
                                   no-source
                                   (list 'tmp)
                                   (list y)
                                   '()
                                   (let ((y (build-lexical-reference no-source 'tmp y)))
                                     (build-conditional
                                       no-source
                                       (let* ((tmp fender) (tmp ($sc-dispatch tmp '#(atom #t))))
                                         (if tmp
                                           (apply (lambda () y) tmp)
                                           (build-conditional
                                             no-source
                                             y
                                             (build-dispatch-call pvars fender y r mod)
                                             (build-data no-source #f))))
                                       (build-dispatch-call pvars exp y r mod)
                                       (gen-syntax-case x keys clauses r mod))))
                                 (list (if (eq? p 'any)
                                        (build-primcall no-source 'list (list x))
                                        (build-primcall
                                          no-source
                                          '$sc-dispatch
                                          (list x (build-data no-source p)))))))))))))
              (gen-syntax-case
                (lambda (x keys clauses r mod)
                  (if (null? clauses)
                    (build-primcall
                      no-source
                      'syntax-violation
                      (list (build-data no-source #f)
                        (build-data no-source "source expression failed to match any pattern")
                        x))
                    (let* ((tmp-1 (car clauses)) (tmp ($sc-dispatch tmp-1 '(any any))))
                      (if tmp
                        (apply (lambda (pat exp)
                                (if (and (id? pat)
                                     (and-map
                                       (lambda (x) (not (free-id=? pat x)))
                                       (cons (make-syntax '... top-wrap '(hygiene capy)) keys)))
                                  (if (free-id=? pat (make-syntax '_ top-wrap '(hygiene capy)))
                                    (expand exp r empty-wrap mod)
                                    (let ((labels (list (gen-label))) (var (gen-var pat)))
                                      (build-call
                                        no-source
                                        (build-simple-lambda
                                          no-source
                                          (list (syntax->datum pat))
                                          (list var)
                                          '()
                                          (expand
                                            exp
                                            (extend-env labels (list (cons 'syntax (cons var 0))) r)
                                            (make-binding-wrap (list pat) labels empty-wrap)
                                            mod))
                                        (list x))))
                                  (gen-clause x keys (cdr clauses) r pat #t exp mod)))
                          tmp)
                        (let ((tmp ($sc-dispatch tmp-1 '(any any any))))
                          (if tmp
                            (apply (lambda (pat fender exp)
                                    (gen-clause x keys (cdr clauses) r pat fender exp mod))
                              tmp)
                            (syntax-violation 'syntax-case "invalid clause" (car clauses))))))))))
      (lambda (e r w s mod)
        (let* ((e (source-wrap e w s mod)) (tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ any each-any . each-any))))
          (if tmp
            (apply (lambda (val key m)
                    (if (and-map (lambda (x) (and (id? x) (not (ellipsis? x r mod)))) key)
                      (let ((x (gen-var 'tmp)))
                        (build-call
                          s
                          (build-simple-lambda
                            no-source
                            (list 'tmp)
                            (list x)
                            '()
                            (gen-syntax-case (build-lexical-reference no-source 'tmp x) key m r mod))
                          (list (expand val r empty-wrap mod))))
                      (syntax-violation 'syntax-case "invalid literals list" e)))
              tmp)
            (syntax-violation #f "source expression failed to match any pattern" tmp-1))))))

  (define expand-with-ellipsis
    (lambda (e r w s mod)
      (syntax-case e ()
        [(_ dots e1 e2 ...)
          (let ((id (if (symbol? #'dots)
                     '$sc-ellipsis
                     (make-syntax
                       '$sc-ellipsis
                       (syntax-wrap #'dots)
                       (syntax-module #'dots)
                       (syntax-sourcev #'dots)))))
            (let ((ids (list id))
                  (labels (list (gen-label)))
                  (bindings (list (cons 'ellipsis (source-wrap #'dots w s mod)))))
              (let ((nw (make-binding-wrap ids labels w)) (nr (extend-env labels bindings r)))
                (expand-body #'(e1 e2 ...) (source-wrap e nw s mod) nr nw mod))))])))

  (set! syntax->datum (lambda (x) (strip x)))
  (set! $sc-dispatch (lambda (e p)
                      ;; $sc-dispatch expects an expression and a pattern.  If the expression
                      ;; matches the pattern a list of the matching expressions for each
                      ;; "any" is returned.  Otherwise, #f is returned.

                      ;; The expression is matched with the pattern as follows:

                      ;; pattern:                           matches:
                      ;;   ()                                 empty list
                      ;;   any                                anything
                      ;;   (<pattern>1 . <pattern>2)          (<pattern>1 . <pattern>2)
                      ;;   each-any                           (any*)
                      ;;   #(free-id <key>)                   <key> with free-identifier=?
                      ;;   #(each <pattern>)                  (<pattern>*)
                      ;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
                      ;;   #(vector <pattern>)                (list->vector <pattern>)
                      ;;   #(atom <object>)                   <object> with "equal?"

                      ;; Vector cops out to pair under assumption that vectors are rare.  If
                      ;; not, should convert to:
                      ;;   #(vector <pattern>*)               #(<pattern>*)

                      (define (match-each e p w mod)
                        (cond
                          ((pair? e)
                            (let ((first (match (car e) p w '() mod)))
                              (and first
                                (let ((rest (match-each (cdr e) p w mod)))
                                  (and rest (cons first rest))))))
                          ((null? e) '())
                          ((syntax? e)
                            (match-each (syntax-expression e)
                              p
                              (join-wraps w (syntax-wrap e))
                              (or (syntax-module e) mod)))
                          (else #f)))

                      (define (match-each+ e x-pat y-pat z-pat w r mod)
                        (let f ((e e) (w w))
                          (cond
                            ((pair? e)
                              (call-with-values (lambda () (f (cdr e) w))
                                (lambda (xr* y-pat r)
                                  (if r
                                    (if (null? y-pat)
                                      (let ((xr (match (car e) x-pat w '() mod)))
                                        (if xr
                                          (values (cons xr xr*) y-pat r)
                                          (values #f #f #f)))
                                      (values
                                        '()
                                        (cdr y-pat)
                                        (match (car e) (car y-pat) w r mod)))
                                    (values #f #f #f)))))
                            ((syntax? e)
                              (f (syntax-expression e)
                                (join-wraps w (syntax-wrap e))))
                            (else
                              (values '() y-pat (match e z-pat w r mod))))))

                      (define (match-each-any e w mod)
                        (cond
                          ((pair? e)
                            (let ((l (match-each-any (cdr e) w mod)))
                              (and l (cons (wrap (car e) w mod) l))))
                          ((null? e) '())
                          ((syntax? e)
                            (match-each-any (syntax-expression e)
                              (join-wraps w (syntax-wrap e))
                              mod))
                          (else #f)))

                      (define (match-empty p r)
                        (cond
                          ((null? p) r)
                          ((eq? p '_) r)
                          ((eq? p 'any) (cons '() r))
                          ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
                          ((eq? p 'each-any) (cons '() r))
                          (else
                            (case (vector-ref p 0)
                              ((each) (match-empty (vector-ref p 1) r))
                              ((each+) (match-empty (vector-ref p 1)
                                        (match-empty
                                          (reverse (vector-ref p 2))
                                          (match-empty (vector-ref p 3) r))))
                              ((free-id atom) r)
                              ((vector) (match-empty (vector-ref p 1) r))))))

                      (define (combine r* r)
                        (if (null? (car r*))
                          r
                          (cons (map car r*) (combine (map cdr r*) r))))

                      (define (match* e p w r mod)
                        (cond
                          ((null? p) (and (null? e) r))
                          ((pair? p)
                            (and (pair? e) (match (car e) (car p) w
                                            (match (cdr e) (cdr p) w r mod)
                                            mod)))
                          ((eq? p 'each-any)
                            (let ((l (match-each-any e w mod))) (and l (cons l r))))
                          (else
                            (case (vector-ref p 0)
                              ((each)
                                (if (null? e)
                                  (match-empty (vector-ref p 1) r)
                                  (let ((l (match-each e (vector-ref p 1) w mod)))
                                    (and l
                                      (let collect ((l l))
                                        (if (null? (car l))
                                          r
                                          (cons (map car l) (collect (map cdr l)))))))))
                              ((each+)
                                (call-with-values
                                  (lambda ()
                                    (match-each+ e (vector-ref p 1) (vector-ref p 2) (vector-ref p 3) w r mod))
                                  (lambda (xr* y-pat r)
                                    (and r
                                      (null? y-pat)
                                      (if (null? xr*)
                                        (match-empty (vector-ref p 1) r)
                                        (combine xr* r))))))
                              ((free-id) (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r))
                              ((atom) (and (equal? (vector-ref p 1) (strip e)) r))
                              ((vector)
                                (and (vector? e)
                                  (match (vector->list e) (vector-ref p 1) w r mod)))))))

                      (define (match e p w r mod)
                        (cond
                          ((not r) #f)
                          ((eq? p '_) r)
                          ((eq? p 'any) (cons (wrap e w mod) r))
                          ((syntax? e)
                            (match*
                              (syntax-expression e)
                              p
                              (join-wraps w (syntax-wrap e))
                              r
                              (or (syntax-module e) mod)))
                          (else (match* e p w r mod))))

                      (cond
                        ((eq? p 'any) (list e))
                        ((eq? p '_) '())
                        ((syntax? e)
                          (match* (syntax-expression e)
                            p
                            (syntax-wrap e)
                            '()
                            (syntax-module e)))
                        (else (match* e p empty-wrap '() #f)))))

  (set! identifier? (lambda (x) (nonsymbol-id? x)))
  (set! datum->syntax (lambda (id datum . opt-source)
                       (define source (if (null? opt-source) #f (car opt-source)))
                       (define (props->sourcev alist)
                         (and (pair? alist)
                           (vector (assq-ref alist 'filename)
                             (assq-ref alist 'line)
                             (assq-ref alist 'column))))

                       (define (wrap e)
                         (make-syntax
                           datum
                           (if id (syntax-wrap id) empty-wrap)
                           (if id (syntax-module id) #f)
                           (cond
                             ((and (not source) id (syntax-sourcev id)) (syntax-sourcev id))
                             ((not source) (props->sourcev (source-properties datum)))
                             ((and (alist? source)) (props->sourcev source))
                             ((and (vector? source) (= (vector-length source) 3)) source)
                             (else (if (not (vector? (syntax-sourcev source)))
                                    (assertion-violation 'datum->syntax "invalid source vector" for datum))
                               (syntax-sourcev source)))))
                       (cond
                         ((syntax? datum) (wrap datum))
                         (else (wrap datum)))))

  (set! free-identifier=? (lambda (x y)
                           (if (not (nonsymbol-id? x))
                             (assertion-violation 'free-identifier=? "Expected syntax identifier" x))
                           (if (not (nonsymbol-id? y))
                             (assertion-violation 'free-identifier=? "Expected syntax identifier" y))
                           (free-id=? x y)))
  (set! bound-identifier=? (lambda (x y)
                            (if (not (nonsymbol-id? x))
                              (assertion-violation 'bound-identifier=? "Expected syntax identifier" x))
                            (if (not (nonsymbol-id? y))
                              (assertion-violation 'bound-identifier=? "Expected syntax identifier" y))
                            (bound-id=? x y)))

  (set! generate-identifier
    (lambda args
      (define sym (if (null? args) 'tmp (car args)))
      (unless (symbol? sym)
        (error 'generate-identifier "Expected symbol" sym))
      (wrap (gen-var sym) top-wrap (cons 'hygiene (module-name (current-module))))))

  (set! generate-temporaries
    (lambda (ls)
      (let ((x ls)) (if (not (list? x)) (syntax-violation 'generate-temporaries "invalid argument" x)))
      (let ((mod (cons 'hygiene (module-name (current-module)))))
        (map (lambda (x) (wrap (gen-var 't) top-wrap mod)) ls))))
  (set! :ellipsis? ellipsis?)

  (set! macroexpand (lambda (x . rest)
                     "Expands expression `x` in the context of module `m` or (current-module) if `m` is not given."
                     (define (unstrip x)
                       (define (annotate result)
                         (let ((props (source-properties x)))
                           (if (pair? props)
                             (datum->syntax #f result props)
                             (datum->syntax #f result))))

                       (cond
                         ((pair? x) (annotate (cons (unstrip (car x)) (unstrip (cdr x)))))
                         ((vector? x)
                           (annotate (list->vector (map unstrip (vector->list x)))))
                         ((syntax? x) x)
                         (else (annotate x))))

                     (let ((m (if (null? rest) 'e (car rest)))
                           (essew (if (= (length rest) 2) (car (cdr rest)) '(eval))))
                       (*log-time*
                         (lambda ()
                           (expand-top-sequence (list (unstrip x)) null-env top-wrap #f m essew
                             (cons 'hygiene (module-name (current-module)))))
                         log:trace
                         "psyntax"
                         #f
                         "macroexpand"))))

  (global-extend 'define 'define '())
  (global-extend 'begin 'begin '())
  (global-extend 'eval-when 'eval-when '())

  (global-extend 'core 'with-continuation-mark
    (lambda (e r w s mod)
      (syntax-case e ()
        [(_ key value e1 e2 ...)
          (make-wcm
            s
            (expand #'key r w mod)
            (expand #'value r w mod)
            (expand-body #'(e1 e2 ...) (source-wrap e w s mod) r w mod))])))

  (global-extend 'core 'if
    (lambda (e r w s mod)
      (syntax-case e ()
        [(_ test then)
          (build-conditional
            s
            (expand #'test r w mod)
            (expand #'then r w mod)
            (build-void no-source))]
        [(_ test then else)
          (build-conditional
            s
            (expand #'test r w mod)
            (expand #'then r w mod)
            (expand #'else r w mod))]
        [_ (syntax-violation 'if "bad if form" (source-wrap e w s mod))])))

  (global-extend 'core 'quote
    (lambda (e r w s mod)
      (syntax-case e ()
        [(_ datum) (build-data s (strip #'datum))]
        [_ (syntax-violation 'quote "bad quote form" (source-wrap e w s mod))])))

  (global-extend 'core 'quote-syntax
    (lambda (e r w s mod)
      (syntax-case (source-wrap e w s mod) ()
        [(_ datum) (build-data s #'datum)]
        [_ (syntax-violation 'quote-syntax "bad quote form" (source-wrap e w s mod))])))

  (global-extend 'core 'lambda
    (lambda (e r w s mod)
      (syntax-case e ()
        [(_ args e1 e2 ...)
          (receive (req opt rest kw) (lambda-formals #'args)
            (let lp ([body #'(e1 e2 ...)] [meta '()])
              (syntax-case body ()
                [(docstring e1 e2 ...)
                  (string? (syntax->datum #'docstring))
                  (lp #'(e1 e2 ...)
                    (append meta
                      `((documentation . ,(syntax->datum #'docstring)))))]
                [(#((k . v) ...) e1 e2 ...)
                  (lp #'(e1 e2 ...)
                    (append meta (syntax->datum #'((k . v) ...))))]
                [_ (expand-simple-lambda e r w s mod req rest meta body)])))])))

  (global-extend 'core 'with-ellipsis expand-with-ellipsis)

  (global-extend 'core 'let
    (let ()
      (define (expand-let e r w s mod constructor ids vals exps)
        (if (not (valid-bound-ids? ids))
          (syntax-violation 'let "duplicate bound variable" e)
          (let ((labels (gen-labels ids))
                (new-vars (map gen-var ids)))
            (let ((nw (make-binding-wrap ids labels w))
                  (nr (extend-var-env labels new-vars r)))
              (constructor s
                (map syntax->datum ids)
                new-vars
                (map (lambda (x) (expand x r w mod)) vals)
                (expand-body exps (source-wrap e nw s mod)
                  nr
                  nw
                  mod))))))
      (lambda (e r w s mod)
        (syntax-case e () 
          [(_ ((id val) ...) e1 e2 ...)
            (and-map id? #'(id ...))
            (expand-let 
              e r w s mod
              build-let
              #'(id ...)
              #'(val ...)
              #'(e1 e2 ...))]
          [(_ f ((id val) ...) e1 e2 ...)
            (and (id? #'f) (and-map id? #'(id ...)))
            (expand-let 
              e r w s mod 
              build-named-let 
              #'(f id ...)
              #'(val ...)
              #'(e1 e2 ...))]
          [_ (syntax-violation 'let "bad let" (source-wrap e w s mod))]))))
  (global-extend 'core 'letrec
    (lambda (e r w s mod)
      (syntax-case e () 
        [(_ ((id val) ...) e1 e2 ...)
          (and-map id? #'(id ...))
          (if (not (valid-bound-ids? #'(id ...)))
            (syntax-violation 'letrec "duplicate bound variable" e)
            (let* ([labels (gen-labels #'(id ...))]
                   [new-vars (map gen-var #'(id ...))]
                   [w (make-binding-wrap #'(id ...) labels w)]
                   [r (extend-var-env labels new-vars r)])
              (build-letrec 
                s 
                (syntax->datum #'(id ...))
                new-vars 
                (map (lambda (x) (expand x r w mod)) #'(val ...))
                (expand-body #'(e1 e2 ...)
                             (source-wrap e w s mod)
                             r w mod))))])))
      
   (global-extend 'core 'letrec*
    (lambda (e r w s mod)
      (syntax-case e () 
        [(_ ((id val) ...) e1 e2 ...)
          (and-map id? #'(id ...))
          (if (not (valid-bound-ids? #'(id ...)))
            (syntax-violation 'letrec* "duplicate bound variable" e)
            (let* ([labels (gen-labels #'(id ...))]
                   [new-vars (map gen-var #'(id ...))]
                   [w (make-binding-wrap #'(id ...) labels w)]
                   [r (extend-var-env labels new-vars r)])
              (build-letrec*
                s 
                (syntax->datum #'(id ...))
                new-vars 
                (map (lambda (x) (expand x r w mod)) #'(val ...))
                (expand-body #'(e1 e2 ...)
                             (source-wrap e w s mod)
                             r w mod))))])))
      
  (global-extend 'core 'set!
    (lambda (e r w s mod)
      (syntax-case e () 
        [(_ id val)
          (id? #'id)
          (receive (type value id-mod) (resolve-identifier #'id w r mod #t)
            (case type 
              [(lexical)
                (build-lexical-assignment s (syntax->datum #'id) value (expand #'val r w mod))]
              [(global)
                (build-global-assignment s value (expand #'val r w mod) id-mod)]
              [(macro)
                (if (variable-transformer? (car value))
                  (expand (expand-macro value e r w s #f mod) r empty-wrap mod)
                  (syntax-violation
                    'set!
                    "not a variable transformer"
                    (wrap e w mod)
                    (wrap #'id w id-mod)))]
              [(displaced-lexical)
                (syntax-violation 'set! "identifier out of context" (wrap #'id w mod))]
              [else (syntax-violation 'set! "bad set!" (source-wrap e w s mod))]))]
        [(_ (head tail ...) val)
          (receive (type value ee* ee ww ss modmod) (syntax-type #'head r empty-wrap no-source #f mod #t)
            (case type 
              [(module-ref)
                (let ([val (expand #'val r w mod)])
                  (receive (e r w s* mod) (value #'(head tail ...) r w mod)
                    (syntax-case e () 
                      [e (id? #'e)
                        (build-global-assignment s (syntax->datum #'e) val mod)])))]
              [else 
                (build-call 
                  s 
                  (expand #'(setter head) r w mod)
                  (map (lambda (e) (expand e r w mod)) #'(tail ... val)))]))])))

  (global-extend 'local-syntax 'letrec-syntax #t)
  (global-extend 'local-syntax 'let-syntax #f)
  (global-extend 'core 'syntax-case expand-syntax-case)
  (global-extend 'core 'syntax expand-syntax)
  (global-extend 'define-syntax 'define-syntax '())
  (global-extend 'define-syntax-parameter 'define-syntax-parameter '())
  (global-extend 'define-property 'define-property '())
  (global-extend 'module-ref '@ expand-public-ref)
  (global-extend 'module-ref '@@ expand-private-ref)

  (set! syntax-error
    (let ((make-syntax make-syntax))
      (make-syntax-transformer
        'syntax-error
        'macro
        (cons (lambda (x)
               (let ((tmp-1 x))
                 (let ((tmp ($sc-dispatch tmp-1 '(_ (any . any) any . each-any))))
                   (if (if tmp (apply (lambda (keyword operands message arg) (string? (syntax->datum message))) tmp) #f)
                     (apply (lambda (keyword operands message arg)
                             (syntax-violation
                               (syntax->datum keyword)
                               (string-join
                                 (cons (syntax->datum message) (map (lambda (x) (object->string (syntax->datum x))) arg)))
                               (if (syntax->datum keyword) (cons keyword operands) #f)))
                       tmp)
                     (let ((tmp ($sc-dispatch tmp-1 '(_ any . each-any))))
                       (if (if tmp (apply (lambda (message arg) (string? (syntax->datum message))) tmp) #f)
                         (apply (lambda (message arg)
                                 (cons (make-syntax
                                        'syntax-error
                                        (list top-mark
                                          (vector
                                            'ribcage
                                            '#(syntax-error)
                                            top-wrap-v
                                            (vector
                                              (cons '(hygiene capy)
                                                (make-syntax 'syntax-error top-wrap '(hygiene capy))))))
                                        '(hygiene capy))
                                   (cons '(#f) (cons message arg))))
                           tmp)
                         (syntax-violation #f "source expression failed to match any pattern" tmp-1)))))))
          (make-syntax #f top-wrap '(hygiene capy)))))))

(define with-syntax
  (let ((make-syntax make-syntax))
    (make-syntax-transformer
      'with-syntax
      'macro
      (cons (lambda (x)
             (let ((tmp x))
               (let ((tmp-1 ($sc-dispatch tmp '(_ () any . each-any))))
                 (if tmp-1
                   (apply (lambda (e1 e2) (cons (make-syntax 'let '((top)) '(hygiene capy)) (cons '() (cons e1 e2))))
                     tmp-1)
                   (let ((tmp-1 ($sc-dispatch tmp '(_ ((any any)) any . each-any))))
                     (if tmp-1
                       (apply (lambda (out in e1 e2)
                               (list (make-syntax 'syntax-case '((top)) '(hygiene capy))
                                 in
                                 '()
                                 (list out
                                   (cons (make-syntax 'let '((top)) '(hygiene capy)) (cons '() (cons e1 e2))))))
                         tmp-1)
                       (let ((tmp-1 ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
                         (if tmp-1
                           (apply (lambda (out in e1 e2)
                                   (list (make-syntax 'syntax-case '((top)) '(hygiene capy))
                                     (cons (make-syntax 'list '((top)) '(hygiene capy)) in)
                                     '()
                                     (list out
                                       (cons (make-syntax 'let '((top)) '(hygiene capy))
                                         (cons '() (cons e1 e2))))))
                             tmp-1)
                           (syntax-violation #f "source expression failed to match any pattern" tmp)))))))))
        (make-syntax #f '((top)) '(hygiene capy))))))



(define syntax-error
  (let ((make-syntax make-syntax))
    (define (string-join args)
      (define result (open-output-string))
      (define len (length args))
      (let loop ((i 0) (args args))
        (if (null? args)
          (get-output-string result)
          (begin
            (when (> i 0)
              (write-string " " result))
            (write-string (car args) result)
            (loop (+ i 1) (cdr args))))))

    (make-syntax-transformer
      'syntax-error
      'macro
      (cons (lambda (x)
             (let ((tmp-1 x))
               (let ((tmp ($sc-dispatch tmp-1 '(_ (any . any) any . each-any))))
                 (if (if tmp (apply (lambda (keyword operands message arg) (string? (syntax->datum message))) tmp) #f)
                   (apply (lambda (keyword operands message arg)
                           (syntax-violation
                             (syntax->datum keyword)
                             (string-join
                               (cons (syntax->datum message) (map (lambda (x) (format "~a" (syntax->datum x))) arg)))
                             (if (syntax->datum keyword) (cons keyword operands) #f)))
                     tmp)
                   (let ((tmp ($sc-dispatch tmp-1 '(_ any . each-any))))
                     (if (if tmp (apply (lambda (message arg) (string? (syntax->datum message))) tmp) #f)
                       (apply (lambda (message arg)
                               (cons (make-syntax
                                      'syntax-error
                                      (list '(top)
                                        (vector
                                          'ribcage
                                          '#(syntax-error)
                                          '#((top))
                                          (vector
                                            (cons '(hygiene capy)
                                              (make-syntax 'syntax-error '((top)) '(hygiene capy))))))
                                      '(hygiene capy))
                                 (cons '(#f) (cons message arg))))
                         tmp)
                       (syntax-violation #f "source expression failed to match any pattern" tmp-1)))))))
        (make-syntax #f '((top)) '(hygiene capy))))))

(define let*
  (let ((make-syntax make-syntax))
    (make-syntax-transformer
      'let*
      'macro
      (cons (lambda (x)
             (let ((tmp-1 x))
               (let ((tmp ($sc-dispatch tmp-1 '(any #(each (any any)) any . each-any))))
                 (if (if tmp (apply (lambda (let* x v e1 e2) (and-map identifier? x)) tmp) #f)
                   (apply (lambda (let* x v e1 e2)
                           (let f ((bindings (map list x v)))
                             (if (null? bindings)
                               (cons (make-syntax 'let '((top)) '(hygiene capy)) (cons '() (cons e1 e2)))
                               (let ((tmp-1 (list (f (cdr bindings)) (car bindings))))
                                 (let ((tmp ($sc-dispatch tmp-1 '(any any))))
                                   (if tmp
                                     (apply (lambda (body binding)
                                             (list (make-syntax 'let '((top)) '(hygiene capy)) (list binding) body))
                                       tmp)
                                     (syntax-violation #f "source expression failed to match any pattern" tmp-1)))))))
                     tmp)
                   (syntax-violation #f "source expression failed to match any pattern" tmp-1)))))
        (make-syntax #f '((top)) '(hygiene capy))))))
