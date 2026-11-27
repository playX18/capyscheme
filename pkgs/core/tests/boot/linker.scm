(import (srfi 64) (rnrs) (rnrs eval) (prefix (srfi 1) a:) (prefix (srfi :1) b:) (prefix (srfi :1 lists) c:))

(test-group "linker.scm"
  (test-group "one instance per SRFI, whatever the spelling"
    (test-assert "(srfi 1) and (srfi :1)" (eq? a:fold b:fold))
    (test-assert "(srfi 1) and (srfi :1 lists)" (eq? a:fold c:fold)))

  (test-group "libraries export what the export spec says"
    (eval '(library (linker-test-a (1 2 3))
             (export a (rename (b bb)) m)
             (import (rnrs))
             (define a 1) (define b 2) (define c 3)
             (define-syntax m (syntax-rules () ((_) c))))
      (environment '(rnrs)))
    (test-equal "export, rename and a macro that uses a private definition"
      '(1 2 3)
      (eval '(list a bb (m)) (environment '(rnrs) '(linker-test-a))))
    (test-error "a definition that is not exported is not visible"
      (eval 'c (environment '(rnrs) '(linker-test-a))))
    (test-equal "a version reference that matches" 1
      (eval 'a (environment '(rnrs) '(linker-test-a (1 (>= 2))))))
    (test-error "a version reference that does not match"
      (environment '(rnrs) '(linker-test-a (2))))))
