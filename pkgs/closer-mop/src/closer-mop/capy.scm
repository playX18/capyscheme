; Closer-MOP compatibility shim.

(library (closer-mop capy)
  (export
    ; Classes.
    standard-object funcallable-standard-object standard-class
    funcallable-standard-class standard-generic-function standard-method
    standard-reader-method standard-writer-method
    standard-direct-slot-definition standard-effective-slot-definition
    eql-specializer forward-referenced-class metaobject specializer
    slot-definition generic-function method class method-combination

    ; Class protocol.
    class-direct-superclasses class-direct-slots class-direct-subclasses
    class-precedence-list class-slots class-finalized-p class-prototype
    class-default-initargs class-direct-default-initargs
    compute-class-precedence-list compute-slots finalize-inheritance
    ensure-finalized validate-superclass ensure-class
    classp subclassp

    ; Generic functions and methods.
    generic-function-name generic-function-methods
    method-specializers method-generic-function method-function
    method-qualifiers
    add-method remove-method
    compute-applicable-methods compute-applicable-methods-using-classes
    compute-discriminating-function compute-effective-method
    compute-effective-method-function make-method-lambda
    set-funcallable-instance-function
    required-args extract-lambda-list extract-specializer-names
    generic-function-lambda-list
    eql-specializer-object intern-eql-specializer

    ; Slots.
    slot-definition-name slot-definition-allocation
    slot-definition-getter slot-definition-setter slot-definition-options
    slot-value set-slot-value!
    slot-value-using-class slot-boundp-using-class
    slot-makunbound-using-class standard-instance-access

    ; Syntax.
    defclass defgeneric defmethod)
  (import (capy) (capy mop))

  ; Closer names.
  (define standard-object <object>)
  (define funcallable-standard-object <funcallable-standard-object>)
  (define standard-class <class>)
  (define funcallable-standard-class <funcallable-standard-class>)
  (define standard-generic-function <generic>)
  (define standard-method <method>)
  (define standard-reader-method <standard-reader-method>)
  (define standard-writer-method <standard-writer-method>)
  (define standard-direct-slot-definition <standard-direct-slot-definition>)
  (define standard-effective-slot-definition <standard-effective-slot-definition>)
  (define eql-specializer <eql-specializer>)
  (define forward-referenced-class <forward-referenced-class>)
  (define metaobject <metaobject>)
  (define specializer <specializer>)
  (define slot-definition <slot-definition>)
  (define generic-function <generic>)
  (define method <method>)
  (define class <class>)
  (define method-combination <method-combination>)

  ; Class protocol.
  (define class-direct-superclasses class-direct-supers)
  (define class-finalized-p class-finalized?)
  (define classp class?)
  (define subclassp subclass?)

  (define (ensure-finalized class)
    (unless (class-finalized? class)
      (finalize-inheritance class))
    class)

  ; Generics.
  (define (generic-function-name generic)
    (let ([name (generic-name generic)])
      (if (string? name) (string->symbol name) name)))
  (define add-method add-method!)
  (define remove-method remove-method!)
  (define set-funcallable-instance-function set-funcallable-instance-function!)

  ; Slots.
  (define (slot-name-of slot)
    (if (slot-definition? slot) (slot-definition-name slot) slot))
  (define (slot-value object name) (slot-ref object name))
  (define (set-slot-value! object name value) (slot-set! object name value))
  (define (slot-value-using-class class object slot)
    (slot-ref-using-class class object (slot-name-of slot)))
  (define (slot-boundp-using-class class object slot)
    (slot-bound-using-class? class object (slot-name-of slot)))
  (define (slot-makunbound-using-class class object slot)
    (slot-unbind-using-class! class object (slot-name-of slot)))

  ; Required args.
  (define (required-args lambda-list)
    (let loop ([rest lambda-list] [acc '()])
      (cond
        [(and (pair? rest) (symbol? (car rest)))
          (loop (cdr rest) (cons (car rest) acc))]
        [else (reverse acc)])))

  ; The lambda list a generic was declared with by defgeneric, or #f.
  (define (generic-function-lambda-list generic)
    (procedure-property generic 'lambda-list))

  ; Definition macros.
  (define-syntax defclass
    (syntax-rules ()
      [(_ . rest) (define-class . rest)]))
  (define-syntax defgeneric
    (syntax-rules ()
      [(_ name (arg ...) . options)
        (begin
          (define-generic name . options)
          (set-procedure-property! name 'lambda-list '(arg ...)))]
      [(_ name . options) (define-generic name . options)]))
  (define-syntax defmethod
    (syntax-rules (:before :after :around :locked)
      [(_ name :before (arg ...) body ...)
        (define-method (name arg ...) #:before body ...)]
      [(_ name :after (arg ...) body ...)
        (define-method (name arg ...) #:after body ...)]
      [(_ name :around (arg ...) body ...)
        (define-method (name arg ...) #:around body ...)]
      [(_ name :locked (arg ...) body ...)
        (define-method (name arg ...) #:locked body ...)]
      [(_ name (arg ...) body ...)
        (define-method (name arg ...) body ...)])))
