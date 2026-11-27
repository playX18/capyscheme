;; (closer-mop capy): the Closer-MOP names, mapped onto Capy's protocol.
(import (srfi 64)
        (capy)
        (capy mop)
        (closer-mop capy))

(test-begin "closer-mop shim")

(defclass cs-point () ((x :accessor cs-x #:init-keyword #:x) (y :initform 2)))
(defclass cs-named (cs-point) ())

(defgeneric cs-area (o))
(defmethod cs-area ((o cs-point)) (* 2 (cs-x o)))
(defmethod cs-area :before ((o cs-named)) 'before)
(defmethod cs-area ((o cs-named)) (list 'named (next-method)))

(test-group "classes"
  (test-assert "standard-class is the class metaclass" (eq? standard-class <class>))
  (test-assert "standard-object is the root object class" (eq? standard-object <object>))
  (test-assert "classp recognizes a class" (classp cs-point))
  (test-assert "subclassp is subclass?" (subclassp cs-named cs-point))
  (test-equal "class-direct-superclasses lists the supers" (list cs-point <object>)
    (class-direct-superclasses cs-named))
  (test-assert "class-finalized-p holds after definition" (class-finalized-p cs-point))
  (test-assert "ensure-finalized returns its class" (eq? (ensure-finalized cs-point) cs-point)))

(test-group "generic functions"
  (test-equal "defmethod with a qualifier and a plain method" '(named 4)
    (cs-area (make cs-named #:x 2)))
  (test-equal "generic-function-name is a symbol" 'cs-area
    (generic-function-name cs-area))
  (test-equal "generic-function-methods lists the methods" 3
    (length (generic-function-methods cs-area)))
  (test-equal "method-qualifiers names the qualifier" '(#:before)
    (method-qualifiers (let loop ([ms (generic-function-methods cs-area)])
      (if (equal? (method-qualifiers (car ms)) '(#:before)) (car ms) (loop (cdr ms)))))))

(defgeneric cs-declared (a b))
(defgeneric cs-undeclared)

(test-group "defgeneric keeps its lambda list"
  (test-equal "the declared list" '(a b) (generic-function-lambda-list cs-declared))
  (test-assert "none when it was not given" (not (generic-function-lambda-list cs-undeclared))))

(test-group "slots"
  (define p (make cs-point #:x 3))
  (test-equal "slot-value reads by name" 2 (slot-value p 'y))
  (test-equal "slot-value-using-class accepts a slot name" 3
    (slot-value-using-class cs-point p 'x))
  (test-equal "slot-value-using-class accepts a slot definition" 3
    (slot-value-using-class cs-point p (car (class-direct-slots cs-point))))
  (test-assert "slot-boundp-using-class sees a bound slot"
    (slot-boundp-using-class cs-point p 'x))
  (test-equal "required-args reads the leading names" '(a b)
    (required-args '(a b #:key c))))

(test-end "closer-mop shim")
