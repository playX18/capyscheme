; MOP library exports.

(library (capy mop)
  (export
    <standard-class>
    <standard-generic-function>
    <standard-method>
    <funcallable-standard-class>
    <standard-reader-method>
    <standard-writer-method>
    <standard-direct-slot-definition>
    <standard-effective-slot-definition>
    class-precedence-list
    class-direct-supers
    class-direct-slots
    class-slots
    class-direct-subclasses
    class-direct-default-initargs
    class-default-initargs
    class-prototype
    class-finalized?
    <forward-referenced-class>
    compute-class-precedence-list
    compute-slots
    finalize-inheritance
    direct-slot-definition-class
    effective-slot-definition-class
    reader-method-class
    writer-method-class
    compute-effective-slot-definition
    allocate-instance
    initialize-instance
    shared-initialize
    reinitialize-instance
    update-instance-for-different-class
    validate-superclass
    ensure-class
    ensure-class-using-class
    generic-function-methods
    <eql-specializer>
    intern-eql-specializer
    eql-specializer-object
    method-specializers
    method-generic-function
    method-function
    slot-definition-name
    slot-definition-allocation
    slot-definition-getter
    slot-definition-setter
    slot-definition-options
    add-method!
    remove-method!
    set-funcallable-instance-function!
    compute-discriminating-function
    standard-discriminating-function
    ensure-generic-function
    add-dependent
    remove-dependent
    map-dependents
    update-dependent
    method-qualifiers
    define-method-combination
    find-method-combination
    generic-method-combination
    method-combination?
    method-combination-name
    method-combination-options
    compute-applicable-methods
    compute-applicable-methods-using-classes
    compute-effective-method
    compute-effective-method-function
    make-method-lambda
    extract-lambda-list
    extract-specializer-names
    slot-ref-using-class
    slot-set-using-class!
    slot-bound-using-class?
    slot-unbind-using-class!
    standard-instance-access
    class-seal!
    class-unseal!
    class-sealed?
    class-final?
    generic-seal!
    generic-unseal!
    generic-sealed?
    generic-precompute!
    generic-flush-cache!
    *max-emf-precompute*
    capy-mop-features)
  (import (capy))

  ; AMOP aliases.
  (define <standard-class> <class>)
  (define <standard-generic-function> <generic>)
  (define <standard-method> <method>)
  (define generic-function-methods generic-methods)
  (define method-generic-function method-generic)
  (define method-function method-body)

  ; MOP feature symbols.
  (define (capy-mop-features)
    '(mop/library mop/combination mop/long-combination-subset mop/invocation
      mop/class-protocol mop/dfun mop/custom-dfun mop/slot-protocol
      mop/sealing mop/precompute mop/metaclass mop/resolved-keys mop/cpl
      mop/default-initargs mop/forward-classes mop/eql mop/qualifiers
      mop/invalidation)))
