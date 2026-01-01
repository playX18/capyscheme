;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995-2014, 2016-2025  Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

;;; Module system routines. Originally extracted from Guile boot-9.scm, modified
;;l to better suit Capy's needs.




(define (module-search fn m v)
  (or (fn m v)
    (let loop ([pos (module-uses m)])
      (if (null? pos)
        #f
        (or (fn (car pos) v)
          (loop (cdr pos)))))))





(define (module-for-each proc module)
  (for-each (lambda (kv) (proc (car kv) (cdr kv))) (core-hash->list (module-obarray module))))


(define (module-map proc module)
  (map proc (core-hash->list (module-obarray module))))

(define (module-ref-submodule module name)
  (core-hash-ref (module-submodules module) name))

(define (module-define-submodule! module name submodule)
  (core-hash-put! (module-submodules module) name submodule))

(define (save-module-excursion thunk)

  (let ([inner-module (current-module)]
        [outer-module #f])
    (dynamic-wind
      (lambda ()
        (set! outer-module (current-module))
        (current-module inner-module)

        (set! inner-module #f))
      thunk
      (lambda ()
        (set! inner-module (current-module))

        (current-module outer-module)
        (set! outer-module #f)))))

(define (module-ref module name . rest)
  (let ([var (module-variable module name)])
    (if (and var (variable-bound? var))
      (variable-ref var)
      (if (null? rest)
        (assertion-violation 'module-ref "unbound variable" module name)
        (car rest)))))

(define (module-set! module name value)
  (let ([var (module-variable module name)])
    (if var
      (variable-set! var value)
      (assertion-violation 'module-set! "unbound variable" module name))))


(define (module-defined? module name)
  (let ([var (module-variable module name)])
    (and var (variable-bound? var))))

(define (module-use! module interface)
  (if (not (or (eq? module interface)
               (memq interface (module-uses module))))
    (begin
      (set-module-uses! module (cons interface (module-uses module)))
      (core-hash-clear! (module-import-obarray module)))))

(define (module-use-interfaces! module interfaces)
  (let* ([cur (module-uses module)]
         [new (let loop ([in interfaces] [out '()])
                (if (null? in)
                  (reverse out)
                  (loop (cdr in)
                    (let ([iface (car in)])
                      (if (or (memq iface cur) (memq iface out))
                        out
                        (cons iface out))))))])

    (set-module-uses! module (append new cur))
    (core-hash-clear! (module-import-obarray module))))

(define (module-define! module name value)
    (let ([variable (module-local-variable module name)])
        (if variable
            (begin
                (variable-set! variable value))
            (let ([variable (make-variable value)])
                (module-add! module name variable)))))


(define (nested-ref root names)
  (if (null? names)
    root
    (let loop ([cur root]
               [head (car names)]
               [tail (cdr names)])
      (if (null? tail)
        (module-ref cur head #f)
        (let ([cur (module-ref-submodule cur head)])
          (and cur
            (loop cur (car tail) (cdr tail))))))))

(define (nested-set! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-set! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (assertion-violation 'nested-set! "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-remove! root names)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-remove! cur head)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (assertion-violation 'nested-remove! "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-ref-module root names)
  (let loop ((cur root)
             (names names))
    (if (null? names)
        cur
        (let ((cur (module-ref-submodule cur (car names))))
          (and cur
               (loop cur (cdr names)))))))

(define (nested-define-module! root names module)
  (if (null? names)
      (assertion-violation 'nested-define-module! "can't redefine root module" module)
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-define-submodule! cur head module)
            (let ((cur (or (module-ref-submodule cur head)
                           (let ((m (make-module)))
                             (set-module-kind! m 'directory)
                             (set-module-name! m (append (module-name cur)
                                                         (list head)))
                             (module-define-submodule! cur head m)
                             m))))
              (loop cur (car tail) (cdr tail)))))))


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


(define (module-name mod)
  (or (raw-module-name mod)
    (let ([name (list (gensym))])
      (set-module-name! mod name)
      (nested-define-module! (resolve-module '() #f #t) name mod)
      (raw-module-name mod))))

(define (make-modules-in module name)
  (or (nested-ref-module module name)
    (let ([m (make-module)])
      (set-module-kind! m 'directory)
      (set-module-name! m (append (module-name module) name))
      (nested-define-module! module name m)
      m)))

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

(define (make-fresh-user-module)
  (let ([m (make-module)])
    (beautify-user-module! m)
    (set-module-declarative! m #f)
    m))

(define (purify-module! module)
  (let ([use-list (module-uses module)])
    (if (and (pair? use-list)
             (eq? (car (last-pair use-list)) the-scm-module))
        (set-module-uses! module (reverse (cdr (reverse use-list)))))))

(define resolve-module
  (let ([root *resolve-module-root*])
    (lambda (name autoload ensure)
      (let ([already (nested-ref-module root name)])
        (if (and already
                 (or (not autoload) (module-public-interface already)))
          already
          (if autoload
            (begin
              (try-module-autoload name)
              (resolve-module name #f ensure))
            (or already
              (and ensure
                (make-modules-in root name)))))))))


(define (->bool x) (not (not x)))

(define autoloads-in-progress '())
(define autoloads-done '((capy . capy)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
                (member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
          (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
        (set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
          (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
        (set! autoloads-done (delete! n autoloads-done))
        (set! autoloads-in-progress (delete! n autoloads-in-progress)))))

(define (try-module-autoload module-name)
  (let* ([reverse-name (reverse module-name)]
         [name (symbol->string (car reverse-name))]
         [dir-hint-module-name (reverse (cdr reverse-name))]
         [dir-hint (apply string-append
          (map (lambda (elt)
            (string-append (symbol->string elt) "/"))
            dir-hint-module-name))])
    (resolve-module dir-hint-module-name #f #t)

    (and (not (autoload-done-or-in-progress? dir-hint name))
         (let ([didit #f])
          (dynamic-wind
            (lambda () (autoload-in-progress! dir-hint name))
            (lambda ()
              (save-module-excursion
                (lambda ()
                  (current-module (make-fresh-user-module))
                  (call/cc (lambda (return)
                    (with-exception-handler
                      (lambda (_x) ((current-exception-printer) _x) (return #f))
                      (lambda ()
                        (load (string-append dir-hint name))
                        (set! didit #t)
                        )))))))
            (lambda () (set-autoloaded! dir-hint name didit)))
          didit))))

(define (identity x) x)

(define (resolve-interface name select hide prefix)
  (let* ([mod (resolve-module name #t #f)]
         [public-i (and mod (module-public-interface mod))]
         [renamer (if prefix (lambda (symbol) (symbol-append prefix symbol)) identity)])
    (if (not public-i)
      (assertion-violation 'resolve-interface "no code for module" name))

    (if (and (not select) (null? hide) (eq? renamer identity))
      public-i
      (let ([custom-i (make-module)])
        (define (maybe-export! src dst var)
          (if (not (memq src hide))
            (begin
              (let ([name (renamer dst)])
                (if (core-hash-ref (module-replacements public-i) src)
                  (core-hash-put! (module-replacements custom-i) name #t))
                (module-add! custom-i name var)))))
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
                  (assertion-violation 'unbound-variable "no binding to select in module" orig name))
                (maybe-export! orig seen var))
            ) select)]
          [else (module-for-each (lambda (sym var)
            (maybe-export! sym sym var)) public-i)])
          custom-i))))


(define (define-module* name . xpure?)
  (define pure? (if (null? xpure?) #f (car xpure?)))
  (let ([module (resolve-module name #f #t)])
    (beautify-user-module! module)
    (if pure?
      (purify-module! module))
    module))



(define (module-export! m names . replace?)
  (let ([replace? (if (null? replace?) #f (car replace?))]
        [public-i (module-public-interface m)])
    (for-each (lambda (name)
      (let* ([internal-name (if (pair? name) (car name) name)]
             [external-name (if (pair? name) (cdr name) name)]
             [var (module-ensure-local-variable! m internal-name)])
        (if replace?
          (core-hash-put! (module-replacements public-i) external-name #t))
        (module-add! public-i external-name var))) names)))

(define (module-replace! m names)
  (module-export! m names #t))

(define (module-export-all! mod)
  (define (fresh-interface!)
    (let ((iface (make-module)))
      (set-module-name! iface (module-name mod))
      (set-module-version! iface (module-version mod))
      (set-module-kind! iface 'interface)
      (set-module-public-interface! mod iface)
      iface))
  (let ((iface (or (module-public-interface mod)
                   (fresh-interface!))))
    (set-module-obarray! iface (module-obarray mod))))

(define (process-use-modules module-iface-args)
  (let ([interfaces (map (lambda (mif-args)
    (or (apply resolve-interface mif-args)
      (assertion-violation 'use "failed to resolve module" mif-args))) module-iface-args)])
    (module-use-interfaces! (current-module) interfaces)))

(define (lookup-bound module name public?)
  (let ([mod (resolve-module module #f #f)])
    (if (not mod)
      (assertion-violation 'lookup-bound "module not found" module))

    (let* ([iface (if public? (module-public-interface mod) mod)]
           [var (module-variable iface name)])
      (if (or (not var) (not (variable-bound? var)))
        (assertion-violation 'lookup-bound "unbound variable" module name))
      var)))


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
                  ;(assertion-violation 'unbound-variable "undefined variable" internal-name m)]
                [(eq? var (module-local-variable m internal-name))
                  ;(assertion-violation 'export "re-exporting local variable" internal-name)]
                  (module-export! m (list name) replace?)]
                [else
                  (if replace?
                    (core-hash-put! (module-replacements public-i) external-name #t))
                  (module-add! public-i external-name var)])))
        names))))


(define current-exception-printer
  (make-parameter
    (lambda (exn . port)
      (define p (if (null? port) (current-error-port) (car port)))
      (when (syntax-violation? exn)
        
        (format #t "form=~a, subform=~a~%" (syntax->datum (syntax-violation-form exn)) (syntax-violation-subform exn)))
      (when (message-condition? exn)
        (format #t "message=~a~%" (condition-message exn)))
      (when (irritants-condition? exn)
        (format #t "irritants=~a~%" (condition-irritants exn)))
      (format p "Unhandled exception: ~a~!: ~a~%~!" (condition-who exn)))))

(set! %load-extensions
  (append '("capy.sls" "capy.sld" "capy.scm" "sls" "sld" "sch" "sps" "ss")
          %load-extensions))

(define capy:execution-mode (make-parameter 'capy))


(let* (
      [host-arch (host-arch)]
      [host-os (host-os)]
      [host-family (host-family)]
      [host-os-sld (string-append host-os ".sld")]
      [host-family-sld (string-append host-family ".sld")]
      [arch-sld (string-append host-arch ".sld")]
      [host-os-sls (string-append host-os ".sls")]
      [host-family-sls (string-append host-family ".sls")]
      [arch-sls (string-append host-arch ".sls")])
  (set! %load-extensions
    (append (list host-os-sld host-family-sld arch-sld
                  host-os-sls host-family-sls arch-sls)
            %load-extensions)))

(define (install-r7rs!)
  (capy:execution-mode 'r7rs))

(define (install-r6rs!)
  (capy:execution-mode 'r6rs))