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
(define (primitive-load filename)
  "Loads file by searching only load path or by its absolute path."
  (let ([thunk (load-thunk-in-vicinity filename #f)])
    (thunk)))

(primitive-load "boot/prim.scm")
(primitive-load "boot/control.scm")
(primitive-load "boot/modules.scm")
(primitive-load "boot/records.scm")
(primitive-load "boot/exceptions.scm")
(primitive-load "boot/expand.scm")
(primitive-load "boot/interpreter.scm")
(primitive-load "boot/psyntax.scm")
(primitive-load "boot/enums.scm")
(primitive-load "boot/sys.scm")
(primitive-load "boot/osdep.scm")
(primitive-load "boot/iosys.scm")
(primitive-load "boot/portio.scm")
(primitive-load "boot/bytevectorio.scm")
(primitive-load "boot/fileio.scm")
(primitive-load "boot/conio.scm")
(primitive-load "boot/stringio.scm")
(primitive-load "boot/stdio.scm")
(primitive-load "boot/utf16.scm")
(primitive-load "boot/customio.scm")
(primitive-load "boot/print.scm")
(primitive-load "boot/format.scm")
(primitive-load "boot/log.scm")
(initialize-io-system)
(primitive-load "boot/reader.scm")
(primitive-load "boot/eval.scm")

(let ([user-module (define-module* '(capy user))])
  (current-module user-module))
