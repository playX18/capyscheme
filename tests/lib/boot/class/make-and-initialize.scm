(import (srfi 64)
        (capy))

(test-begin "boot class make and initialize protocol")

(test-group "make and initialize protocol"
  (define-class make-point ()
    ((x #:init-value 1)
     (initargs #:init-value '())))

  (define-method (initialize (obj make-point) initargs)
    (slot-set! obj 'initargs initargs)
    obj)

  (define made (make make-point #:x 5))
  (define defaulted (make make-point))

  (test-equal "built-in <top> binding exposes top class"
    (class-name <top>)
    "top")
  (test-equal "built-in <object> binding exposes object class"
    (class-name <object>)
    "object")
  (test-equal "built-in <class> binding exposes class class"
    (class-name <class>)
    "class")
  (test-equal "built-in <string> binding exposes string class"
    (class-name <string>)
    "string")
  (test-equal "built-in <bytevector> binding exposes bytevector class"
    (class-name <bytevector>)
    "bytevector")
  (test-equal "built-in <immutable-bytevector> binding exposes immutable bytevector class"
    (class-name <immutable-bytevector>)
    "immutable-bytevector")
  (test-equal "built-in <hash-table> binding exposes hash table class"
    (class-name <hash-table>)
    "hash-table")
  (test-equal "built-in <immutable-hash-table> binding exposes immutable hash table class"
    (class-name <immutable-hash-table>)
    "immutable-hash-table")
  (test-equal "built-in <box> binding exposes box class"
    (class-name <box>)
    "box")
  (for-each
    (lambda (entry)
      (test-equal
        (string-append "built-in binding exposes " (cdr entry) " class")
        (class-name (car entry))
        (cdr entry)))
    (list
      (cons <pair> "pair")
      (cons <variable> "variable")
      (cons <closure> "closure")
      (cons <vector> "vector")
      (cons <tuple> "tuple")
      (cons <top> "top")
      (cons <bottom> "bottom")
      (cons <type> "type")
      (cons <class> "class")
      (cons <object> "object")
      (cons <bool> "bool")
      (cons <char> "char")
      (cons <null> "null")
      (cons <eof> "eof")
      (cons <void> "void")
      (cons <unspecified> "unspecified")
      (cons <undefined> "undefined")
      (cons <fixnum> "fixnum")
      (cons <flonum> "flonum")
      (cons <bigint> "bigint")
      (cons <rational> "rational")
      (cons <complex> "complex")
      (cons <number> "number")
      (cons <symbol> "symbol")
      (cons <keyword> "keyword")
      (cons <string> "string")
      (cons <stringbuf-wide> "stringbuf-wide")
      (cons <stringbuf-narrow> "stringbuf-narrow")
      (cons <bytevector> "bytevector")
      (cons <immutable-bytevector> "immutable-bytevector")
      (cons <mapped-bytevector> "mapped-bytevector")
      (cons <hash-table> "hash-table")
      (cons <immutable-hash-table> "immutable-hash-table")
      (cons <weak-set> "weak-set")
      (cons <weak-table> "weak-table")
      (cons <weak-mapping> "weak-mapping")
      (cons <ephemeron> "ephemeron")
      (cons <box> "box")
      (cons <fluid> "fluid")
      (cons <dynamic-state> "dynamic-state")
      (cons <native-procedure> "native-procedure")
      (cons <native-continuation> "native-continuation")
      (cons <code-block> "code-block")
      (cons <relocatable-code-block> "relocatable-code-block")
      (cons <module> "module")
      (cons <environment> "environment")
      (cons <syntax> "syntax")
      (cons <syntax-transformer> "syntax-transformer")
      (cons <port> "port")
      (cons <socket> "socket")
      (cons <poller> "poller")
      (cons <ffi-pointer> "ffi-pointer")
      (cons <cif> "cif")
      (cons <thread> "thread")
      (cons <mutex> "mutex")
      (cons <condition> "condition")
      (cons <annotation> "annotation")
      (cons <continuation-marks> "continuation-marks")
      (cons <generic> "generic")
      (cons <method> "method")
      (cons <next-method> "next-method")
      (cons <slot-definition> "slot-definition")
      (cons <slot-accessor> "slot-accessor")))
  (test-equal "%builtin-class resolves registered built-in classes"
    (class-name (%builtin-class 'fixnum))
    "fixnum")
  (test-error "%builtin-class rejects unknown built-in classes"
    &assertion-violation
    (%builtin-class 'not-a-built-in-class))
  (test-assert "Scheme classes are instances of <class>"
    (is-a? make-point <class>))
  (test-equal "make is specialized on <class>"
    (method-specializers (car (generic-methods make)))
    (list <class>))
  (test-assert "make allocates Scheme instances"
    (scheme-instance? made))
  (test-assert "Scheme instances inherit from <object>"
    (is-a? made <object>))
  (test-assert "Scheme instances inherit from <top>"
    (is-a? made <top>))
  (test-equal "make applies initargs through make-instance"
    (slot-ref made 'x)
    5)
  (test-equal "make invokes initialize with original initargs"
    (slot-ref made 'initargs)
    (list #:x 5))
  (test-equal "make runs initialize for defaulted instances"
    (slot-ref defaulted 'initargs)
    '())
  (test-error "make rejects non-class receivers"
    &assertion-violation
    (make 'not-a-class)))

(test-end "boot class make and initialize protocol")
