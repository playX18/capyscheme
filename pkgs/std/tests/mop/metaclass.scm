;; mop/metaclass: a class's class is its metaclass; user metaclasses are
;; subclasses of a metaobject class and carry extension slots.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature metaclass")

(define-class <mt-counted> (<class>) ((count #:init-value 0 #:accessor mt-count)))

(define-class mt-x () () #:metaclass <mt-counted>)

(test-group "metaclass identity"
  (test-assert "class-of a class is its metaclass"
    (eq? (class-of mt-x) <mt-counted>))
  (test-assert "a class with a user metaclass is still a class"
    (class? mt-x))
  (test-assert "the metaclass is a subclass of <class>"
    (subclass? <mt-counted> <class>))
  (test-equal "the extension slot is readable with its initial value" 0
    (mt-count mt-x))
  (test-assert "the extension slot is writable"
    (begin ((setter mt-count) mt-x 4) (= 4 (mt-count mt-x)))))

(test-group "builtin metaobject classes"
  (test-assert "<generic> is a metaobject"
    (subclass? <generic> <metaobject>)))

(test-end "mop feature metaclass")
