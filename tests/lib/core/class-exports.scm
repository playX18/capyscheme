(import (core)
        (srfi 64))

(test-begin "core class exports")

(define-class exported-point ()
  ((x #:init-value 1)
   (items #:init-value '())))

(define exported-instance (make exported-point))

(test-assert "core exports built-in class bindings"
  (class? <class>))
(for-each
  (lambda (entry)
    (test-equal
      (string-append "core exports " (cdr entry) " class binding")
      (class-name (car entry))
      (cdr entry)))
  (list
    (cons <bottom> "bottom")
    (cons <bool> "bool")
    (cons <null> "null")
    (cons <eof> "eof")
    (cons <void> "void")
    (cons <undefined> "undefined")
    (cons <keyword> "keyword")
    (cons <tuple> "tuple")
    (cons <weak-set> "weak-set")
    (cons <weak-table> "weak-table")
    (cons <weak-mapping> "weak-mapping")
    (cons <ephemeron> "ephemeron")
    (cons <closure> "closure")
    (cons <native-procedure> "native-procedure")
    (cons <module> "module")
    (cons <environment> "environment")
    (cons <syntax> "syntax")
    (cons <port> "port")
    (cons <socket> "socket")
    (cons <thread> "thread")
    (cons <mutex> "mutex")
    (cons <condition> "condition")
    (cons <generic> "generic")
    (cons <method> "method")
    (cons <next-method> "next-method")
    (cons <slot-definition> "slot-definition")
    (cons <slot-accessor> "slot-accessor")))
(test-equal "core exports define-class and make"
  (slot-ref exported-instance 'x)
  1)
(test-equal "core exports ref and setter metadata"
  (begin
    ((setter ref) exported-instance 'x 2)
    (ref exported-instance 'x))
  2)
(test-equal "core exports slot convenience helpers"
  (begin
    (slot-push! exported-instance 'items 'value)
    (slot-pop! exported-instance 'items))
  'value)
(test-assert "core exports class option helpers"
  (memq #:slots (class-options exported-point)))

(define-generic exported-describe)
(define-method (exported-describe (obj exported-point))
  (next-method))
(define-method (exported-describe obj)
  'fallback)

(test-equal "core exports define-generic and define-method"
  (exported-describe exported-instance)
  'fallback)
(test-assert "core exports generic option helpers"
  (memq #:required-dispatch-arg-count (generic-options exported-describe)))
(test-assert "core exports method option helpers"
  (memq #:specializers (method-options (car (generic-methods exported-describe)))))

(test-equal "core exports coercion helpers"
  (x->integer "5")
  5)
(test-equal "core exports box ref helpers"
  (let ([cell (box 'inside)])
    ((setter ref) cell '* 'outside)
    (ref cell 0))
  'outside)

(test-end "core class exports")
