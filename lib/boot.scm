;; Boot library. Loads all the functionality necessary to run R6RS code
;; including macro-expander. You can produce heap image once this is done.

;; These functions are defined in `boot/eval.scm`. Due to the fact that
;; we now have letrectification overriding them also requires overriding
;; `resolve-module` so we just keep it simple and make `resolve-module`
;; do a global lookup of `load` and `load-in-vicinity`. `primitive-load`
;; is defined here because we do not depend on its overloaded version from
;; here.

;(define (load-in-vicinity filename directory)
;  (let ([thunk (load-thunk-in-vicinity filename #t directory )])
;    (thunk)))
;
;(define (load filename)
;  (let ([thunk (load-thunk-in-vicinity filename #t)])
;    (thunk)))
;
(define ($primitive-load filename)
  "Loads file by searching only load path or by its absolute path."
  (let ([thunk (load-thunk-in-vicinity filename #t)])
    (thunk)))

($primitive-load "boot/prim.scm")
($primitive-load "boot/control.scm")
($primitive-load "boot/modules.scm")
($primitive-load "boot/records.scm")
($primitive-load "boot/conditions.scm")
($primitive-load "boot/violations.scm")
($primitive-load "boot/raise.scm")
($primitive-load "boot/exceptions.scm")
($primitive-load "boot/expand.scm")
($primitive-load "boot/interpreter.scm")
($primitive-load "boot/enums.scm")
($primitive-load "boot/sys.scm")
($primitive-load "boot/osdep.scm")
($primitive-load "boot/iosys.scm")
($primitive-load "boot/iosys2.scm")
($primitive-load "boot/iosys3.scm")
($primitive-load "boot/portio.scm")
($primitive-load "boot/bytevectorio.scm")
($primitive-load "boot/fileio.scm")
($primitive-load "boot/conio.scm")
($primitive-load "boot/stringio.scm")
($primitive-load "boot/stdio.scm")
($primitive-load "boot/utf16.scm")
($primitive-load "boot/customio.scm")
($primitive-load "boot/print.scm")
($primitive-load "boot/format.scm")
($primitive-load "boot/log.scm")
(initialize-io-system)
($primitive-load "boot/match-syntax.scm")
($primitive-load "boot/psyntax-exp.scm")

($primitive-load "boot/str2num.scm")
($primitive-load "boot/num2str.scm")
($primitive-load "boot/reader.scm")
($primitive-load "boot/eval.scm")

; load file containing base macros
(define primitive-load
  (lambda (filename)
    (save-module-excursion (lambda ()
                            (cond
                              ;; when bootstrapping we rely on tree-sitter to parse Scheme code
                              ((%bootstrapping?)
                                (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) #f)])
                                  (with-exception-handler
                                    (lambda (exn)
                                      (format #t ";; (primitive) Error loading file '~a'~%" filename)
                                      (raise exn))
                                    (lambda () (thunk)))))
                              (else
                                (let ([thunk-or-path (try-load-thunk-in-vicinity filename #f)])
                                  (cond
                                    [(procedure? thunk-or-path)
                                      (with-exception-handler
                                        (lambda (exn)
                                          (format (current-error-port) ";; (primitive) Error loading file '~a'~%" filename)
                                          ((current-exception-printer) exn (current-error-port))
                                          (flush-output-port (current-error-port))
                                          (raise exn))
                                        (lambda ()
                                          (*raw-log* log:info
                                            '(capy)
                                            'primitive-load
                                            "Loading file ~a"
                                            filename)
                                          (thunk-or-path)))]
                                    [else
                                      (with-exception-handler
                                        (lambda (exn)
                                          (format (current-error-port) ";; (primitive) Error compiling file '~a'~%" filename)
                                          ((current-exception-printer) exn (current-error-port))
                                          (flush-output-port (current-error-port))
                                          (raise exn))
                                        (lambda ()
                                          (define filename (list-ref thunk-or-path 0))
                                          (define full-filename (list-ref thunk-or-path 1))
                                          (define compiled-path (list-ref thunk-or-path 2))
                                          (*raw-log* log:info
                                            '(capy)
                                            'primitive-load
                                            "Compiling file ~a"
                                            filename)
                                          ((compile-file full-filename compiled-path (current-module) #t))))]))))))))
;(let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) #f)])
;    (with-exception-handler
;        (lambda (exn)
;            (format #t ";; (primitive) Error loading file '~a'~%" filename)
;            (raise exn))
;        (lambda () (thunk))))))))

(primitive-load "boot/base.scm")
(primitive-load "boot/libraries.scm")
(primitive-load "boot/match.scm")
(primitive-load "boot/compiler.scm")
(primitive-load "boot/cli.scm")

(let ([user-module (define-module* '(capy user))])
  (current-module user-module))
