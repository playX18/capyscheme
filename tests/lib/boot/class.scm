(import (srfi 64)
        (capy))

(test-begin "boot class")

(test-group "scheme classes and slots"
  (define point (make-class 'point '(x y)))
  (define p (make-instance point #:x 10))

  (test-assert "class object" (class? point))
  (test-equal "class name" (class-name point) "point")
  (test-assert "class-of instance" (eq? (class-of p) point))
  (test-assert "scheme instance" (scheme-instance? p))
  (test-assert "non-instance" (not (scheme-instance? point)))

  (test-assert "initialized slot is bound" (slot-bound? p 'x))
  (test-equal "initialized slot value" (slot-ref p 'x) 10)
  (test-assert "uninitialized slot is unbound" (not (slot-bound? p 'y)))
  (test-equal "missing slot fallback" (slot-ref p 'z 'missing) 'missing)
  (test-equal "instance-slot-ref reads raw initialized slots" (instance-slot-ref p 0) 10)
  (test-equal "instance-slot-ref returns fallback for raw unbound slots"
    (instance-slot-ref p 1 'raw-missing)
    'raw-missing)

  (slot-set! p 'y 20)
  (test-assert "written slot is bound" (slot-bound? p 'y))
  (test-equal "written slot value" (slot-ref p 'y) 20)
  (instance-slot-set! p 1 30)
  (test-equal "instance-slot-set! writes raw slots" (slot-ref p 'y) 30)
  (test-error "instance-slot-ref rejects unbound raw slots without fallback"
    &assertion-violation
    (instance-slot-ref (make-instance point #:x 1) 1))
  (test-error "instance-slot-ref rejects out-of-bounds indexes"
    &assertion-violation
    (instance-slot-ref p 9))
  (test-error "instance-slot-set! rejects negative indexes"
    &assertion-violation
    (instance-slot-set! p -1 'bad))
  (test-error "instance-slot-ref rejects non-instances"
    &assertion-violation
    (instance-slot-ref point 0)))

(test-group "slot convenience helpers"
  (define-class slot-helper-record ()
    ((items #:init-value '())
     empty))
  (define record (make-instance slot-helper-record))

  (test-assert "slot-exists? reports present slots"
    (slot-exists? record 'items))
  (test-assert "slot-exists? rejects missing slots"
    (not (slot-exists? record 'missing)))
  (test-assert "slot-exists-using-class? reports present slots"
    (slot-exists-using-class? slot-helper-record record 'items))
  (test-assert "slot-exists-using-class? rejects missing slots"
    (not (slot-exists-using-class? slot-helper-record record 'missing)))
  (slot-push! record 'items 'first)
  (slot-push! record 'items 'second)
  (test-equal "slot-push! conses onto slot value"
    (slot-ref record 'items)
    '(second first))
  (test-equal "slot-pop! returns the head"
    (slot-pop! record 'items)
    'second)
  (test-equal "slot-pop! stores the tail"
    (slot-ref record 'items)
    '(first))
  (test-equal "slot-pop! returns default for unbound slot"
    (slot-pop! record 'empty 'fallback)
    'fallback)
  (test-equal "slot-pop! returns default for missing slot"
    (slot-pop! record 'missing 'fallback)
    'fallback)
  (test-error "slot-pop! rejects non-pair slot without default"
    (slot-pop! record 'empty)))

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
  (test-equal "built-in <immutable-string> binding exposes immutable string class"
    (class-name <immutable-string>)
    "immutable-string")
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
      (cons <continuation-closure> "continuation-closure")
      (cons <vector> "vector")
      (cons <immutable-vector> "immutable-vector")
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
      (cons <uninterned-symbol> "uninterned-symbol")
      (cons <keyword> "keyword")
      (cons <string> "string")
      (cons <immutable-string> "immutable-string")
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

(test-group "generic coercion helpers"
  (test-assert "x->string is a generic"
    (generic? x->string))
  (test-equal "x->string preserves immutable strings"
    (x->string "name")
    "name")
  (test-equal "x->string converts numbers"
    (x->string 42)
    "42")
  (test-equal "x->string converts symbols"
    (x->string 'name)
    "name")
  (test-equal "x->string converts chars"
    (x->string #\x)
    "x")
  (test-equal "x->number preserves numbers"
    (x->number 42)
    42)
  (test-equal "x->number parses strings"
    (x->number "42")
    42)
  (test-equal "x->number falls back for non-numeric strings"
    (x->number "nope")
    0)
  (test-equal "x->number converts chars"
    (x->number #\A)
    65)
  (test-equal "x->integer preserves fixnums"
    (x->integer 42)
    42)
  (test-equal "x->integer converts chars"
    (x->integer #\A)
    65)
  (test-equal "x->integer coerces through x->number"
    (x->integer "42")
    42))

(test-group "generic ref helpers"
  (define-class ref-record ()
    ((value #:init-value 1)
     empty))
  (define record (make ref-record))
  (define items (list 'a 'b 'c))
  (define vec (vector 'x 'y))
  (define str (string #\a #\b))
  (define bytes (make-bytevector 3 0))
  (define table (make-core-hashtable 'eq?))
  (define cell (box 'initial))
  (define nested (vector (list 'left (vector 'deep 'value))))

  (test-assert "ref is a generic"
    (generic? ref))
  (test-assert "ref has a setter generic"
    (has-setter? ref))
  (test-equal "ref reads Scheme object slots by symbol"
    (ref record 'value)
    1)
  (test-equal "ref returns fallback for missing object slots"
    (ref record 'missing 'fallback)
    'fallback)
  (test-equal "ref returns fallback for unbound object slots"
    (ref record 'empty 'fallback)
    'fallback)
  ((setter ref) record 'value 2)
  (test-equal "setter ref writes Scheme object slots"
    (slot-ref record 'value)
    2)
  (test-equal "ref reads pairs by index"
    (ref items 1)
    'b)
  ((setter ref) items 1 'changed)
  (test-equal "setter ref writes pairs by index"
    items
    '(a changed c))
  (test-equal "ref reads vectors by index"
    (ref vec 1)
    'y)
  ((setter ref) vec 1 'z)
  (test-equal "setter ref writes vectors by index"
    (vector-ref vec 1)
    'z)
  (test-equal "ref reads strings by index"
    (ref str 1)
    #\b)
  ((setter ref) str 1 #\z)
  (test-equal "setter ref writes strings by index"
    (string-ref str 1)
    #\z)
  (test-equal "ref reads bytevectors by index"
    (ref bytes 1)
    0)
  ((setter ref) bytes 1 42)
  (test-equal "setter ref writes bytevectors by index"
    (bytevector-u8-ref bytes 1)
    42)
  (test-equal "ref reads hash tables by key with fallback"
    (ref table 'missing 'fallback)
    'fallback)
  ((setter ref) table 'name 'capy)
  (test-equal "setter ref writes hash tables by key"
    (core-hash-ref table 'name #f)
    'capy)
  (test-equal "ref reads hash tables by key"
    (ref table 'name)
    'capy)
  (test-equal "ref reads immutable hash tables by key"
    (ref (core-hash-copy table) 'name 'fallback)
    'capy)
  (test-assert "box creates box objects"
    (box? cell))
  (test-equal "unbox reads box values"
    (unbox cell)
    'initial)
  (set-box! cell 'direct)
  (test-equal "set-box! writes box values"
    (unbox cell)
    'direct)
  (test-equal "ref reads boxes by star field"
    (ref cell '*)
    'direct)
  (test-equal "ref reads boxes by zero field"
    (ref cell 0)
    'direct)
  ((setter ref) cell '* 'via-ref)
  (test-equal "setter ref writes boxes"
    (unbox cell)
    'via-ref)
  (test-error "ref rejects unknown box fields"
    &assertion-violation
    (ref cell 'bad))
  (test-assert "universal accessor has a setter"
    (has-setter? ~))
  (test-equal "universal accessor follows nested refs"
    (~ nested 0 1 0)
    'deep)
  ((setter ~) nested 0 1 0 'changed)
  (test-equal "universal accessor setter writes nested refs"
    (~ nested 0 1 0)
    'changed)
  (test-equal "ref* aliases universal accessor"
    (ref* nested 0 1 1)
    'value))

(test-group "describe helpers"
  (define-class describe-record ()
    ((name #:init-value 'capy)
     empty
     (%secret #:init-value 'hidden)))
  (define record (make describe-record))

  (test-assert "describe is a generic"
    (generic? describe))
  (test-assert "describe-slots is a generic"
    (generic? describe-slots))
  (test-equal "describe-details starts false"
    (describe-details)
    #f)
  (test-equal "describe-slots prints visible slots"
    (with-output-to-string
      (lambda ()
        (describe-slots record)))
    "slots:\n  name: capy\n  empty: #<unbound>\n")
  (test-assert "describe includes class name and slots"
    (let ([out (with-output-to-string
                 (lambda ()
                   (describe record)))])
      (and (string-contains out "describe-record")
           (string-contains out "slots:")
           (string-contains out "name: capy"))))
  (test-equal "describe-details setter returns old value"
    (describe-details #t)
    #f)
  (test-equal "describe-details exposes hidden slots"
    (with-output-to-string
      (lambda ()
        (describe-slots record)))
    "slots:\n  name: capy\n  empty: #<unbound>\n  %secret: hidden\n")
  (test-equal "describe-details can be restored"
    (describe-details #f)
    #t)
  (test-error "describe-details rejects non-booleans"
    &assertion-violation
    (describe-details 'yes)))

(test-group "direct supers"
  (define point (make-class 'derived-point '(x y)))
  (define colored-point (make-class 'derived-colored-point '(color) (list point)))
  (define cp (make-instance colored-point #:x 1 #:y 2 #:color 'red))

  (test-equal "inherited slot initarg" (slot-ref cp 'x) 1)
  (test-equal "direct slot initarg" (slot-ref cp 'color) 'red)
  (test-assert "inherited slot is bound" (slot-bound? cp 'y))
  (test-equal "root class is implicit direct super"
    (class-direct-supers point)
    (list <object>))
  (test-equal "class direct supers expose class objects"
    (class-direct-supers colored-point)
    (list point <object>))
  (test-assert "class-options expose supported option keys"
    (memq #:direct-supers (class-options colored-point)))
  (test-equal "class-option exposes direct supers"
    (class-option colored-point #:direct-supers)
    (list point <object>))
  (test-equal "class-option accepts Gauche colon keys"
    (class-option colored-point ':name)
    "derived-colored-point")
  (test-equal "class-option returns defaults for missing options"
    (class-option colored-point ':unknown 'fallback)
    'fallback)
  (test-equal "class precedence list exposes most-specific order"
    (map class-name (class-precedence-list colored-point))
    '("derived-colored-point" "derived-point" "object" "top"))
  (test-assert "subclass? accepts inherited classes"
    (subclass? colored-point point))
  (test-assert "subclass? accepts identical classes"
    (subclass? point point))
  (test-assert "subclass? rejects unrelated subclass direction"
    (not (subclass? point colored-point)))
  (test-assert "is-a? accepts inherited classes"
    (is-a? cp point))
  (test-assert "is-a? accepts exact classes"
    (is-a? cp colored-point))
  (test-equal "class direct slots expose slot definitions"
    (map slot-definition-name (class-direct-slots colored-point))
    '(color))
  (test-assert "class direct slots expose first-class slot definitions"
    (slot-definition? (car (class-direct-slots colored-point))))
  (test-assert "slot definitions expose owning class objects"
    (eq? (slot-definition-owner (car (class-direct-slots colored-point))) colored-point))
  (test-equal "slot definitions expose layout indexes"
    (slot-definition-index (car (class-direct-slots colored-point)))
    2)
  (test-assert "slot definitions expose initialization flags"
    (slot-definition-initializable? (car (class-direct-slots colored-point))))
  (test-assert "slot definitions expose mutability flags"
    (slot-definition-settable? (car (class-direct-slots colored-point))))
  (test-assert "slot definitions expose immutable state"
    (not (slot-definition-immutable? (car (class-direct-slots colored-point)))))
  (test-equal "class slots expose inherited then direct slot definitions"
    (map slot-definition-name (class-slots colored-point))
    '(x y color))
  (test-equal "class accessors expose inherited then direct slot accessors"
    (map slot-accessor-name (class-accessors colored-point))
    '(x y color))
  (test-assert "class accessors expose first-class accessor definitions"
    (slot-accessor? (car (class-accessors colored-point))))
  (test-assert "slot accessors expose owning class objects"
    (eq? (slot-accessor-owner (car (class-accessors colored-point))) point))
  (test-equal "slot accessors expose layout indexes"
    (slot-accessor-index (car (class-accessors colored-point)))
    0)
  (test-equal "class initargs expose inherited then direct init keywords as symbols"
    (class-initargs colored-point)
    '(x y color))
  (test-assert "class direct subclasses expose weak subclass links"
    (memq colored-point (class-direct-subclasses point))))

(test-group "slot condition generics"
  (define point (make-class 'slot-condition-point '(x)))
  (define p (make-instance point))

  (test-assert "slot-unbound is a generic"
    (generic? slot-unbound))
  (test-assert "slot-missing is a generic"
    (generic? slot-missing))
  (test-equal "slot-unbound has a default class method"
    (length (generic-methods slot-unbound))
    1)
  (test-equal "slot-missing has a default class method"
    (length (generic-methods slot-missing))
    1)
  (test-error "slot-unbound default method raises assertion violation"
    &assertion-violation
    (slot-unbound point p 'x))
  (test-error "slot-missing default method raises assertion violation"
    &assertion-violation
    (slot-missing point p 'missing))
  (define-method (slot-unbound (class <class>) obj slot)
    (list 'unbound slot))
  (define-method (slot-missing (class <class>) obj slot . value)
    (list 'missing slot value))
  (test-equal "slot-ref routes unbound slots through slot-unbound"
    (slot-ref p 'x)
    '(unbound x))
  (test-equal "slot-ref routes missing slots through slot-missing"
    (slot-ref p 'missing)
    '(missing missing ()))
  (test-equal "slot-ref missing fallback still bypasses slot-missing"
    (slot-ref p 'missing 'fallback)
    'fallback)
  (define-method (slot-unbound (class <class>) obj slot)
    (assertion-violation 'slot-unbound
      "slot is unbound"
      class
      slot))
  (define-method (slot-missing (class <class>) obj slot . value)
    (assertion-violation 'slot-missing
      "class has no slot"
      class
      slot)))

(test-group "define-class macro"
  (define-class macro-defined-point () (x y))
  (define-class macro-defined-colored-point (macro-defined-point) (color))
  (define-class macro-defined-options ()
    ((x #:init-value 7)
     (y #:init-thunk (lambda () 8))
     (gauche-initform #:initform (+ 20 2))
     (gauche-init-form #:init-form (+ 30 3))
     (locked #:init-value 9 #:immutable #t)
     (hidden #:init-value 10 #:initializable #f)
     (shared #:allocation #:class #:init-value 'initial-shared)
     (shared-empty #:allocation #:class)
     (renamed #:init-keyword #:renamed-value)))
  (define (macro-custom-slot-ref self)
    (list 'read (slot-ref self 'backing)))
  (define (macro-custom-slot-set! self value)
    (slot-set! self 'backing (list 'written value)))
  (define (macro-custom-slot-bound? self)
    (slot-bound? self 'backing))
  (define-class macro-defined-custom-slot ()
    ((visible
       #:slot-ref macro-custom-slot-ref
       #:slot-set! macro-custom-slot-set!
       #:slot-bound? macro-custom-slot-bound?)
     (backing #:initializable #f)))
  (define-class macro-defined-accessors ()
    ((x #:accessor macro-defined-accessor-x)
     (y #:getter macro-defined-accessor-y)
     (z #:setter macro-defined-accessor-z-set!)))
  (define-class macro-defined-gauche-options ()
    ((x :init-value 17)
     (origin :initform (+ 40 1))
     (label :init-form (+ 50 2))
     (shared :allocation :class :init-value 'gauche-shared)
     (y :accessor macro-defined-gauche-y)))
  (define macro-post-initialize-events '())
  (define (macro-initarg-ref initargs key)
    (let ([tail (memq key initargs)])
      (and tail (pair? (cdr tail)) (cadr tail))))
  (define (macro-slot-spec-name slot)
    (if (pair? slot) (car slot) slot))
  (define-method (class-post-initialize (class <class>) initargs)
    (set! macro-post-initialize-events
      (cons
        (list (class-name class)
              (macro-initarg-ref initargs #:name)
              (map macro-slot-spec-name (macro-initarg-ref initargs #:slots))
              (macro-initarg-ref initargs #:applicable))
        macro-post-initialize-events))
    #f)
  (define-class macro-post-initialized ()
    ((x #:init-value 21)))
  (define-class macro-post-initialized-callable ()
    (procedure)
    :applicable #t)
  (define p (make-instance macro-defined-colored-point #:x 3 #:y 4 #:color 'blue))
  (define configured (make-instance macro-defined-options #:renamed-value 11))
  (define gauche-configured (make-instance macro-defined-gauche-options #:y 18))
  (define custom-slot (make-instance macro-defined-custom-slot))
  (define accessor-instance
    (make-instance macro-defined-accessors #:x 1 #:y 2 #:z 3))
  (define custom-visible-accessor
    (class-slot-accessor macro-defined-custom-slot 'visible))
  (define macro-x-accessor
    (class-slot-accessor macro-defined-accessors 'x))
  (define options-x-accessor
    (class-slot-accessor macro-defined-options 'x))
  (define options-y-accessor
    (class-slot-accessor macro-defined-options 'y))
  (define options-hidden-accessor
    (class-slot-accessor macro-defined-options 'hidden))
  (define options-shared-accessor
    (class-slot-accessor macro-defined-options 'shared))
  (define options-renamed-accessor
    (class-slot-accessor macro-defined-options 'renamed))

  (test-assert "define-class creates a class object"
    (class? macro-defined-point))
  (test-equal "define-class names the class"
    (class-name macro-defined-point)
    "macro-defined-point")
  (test-assert "define-class creates malleable classes"
    (class-malleable? macro-defined-point))
  (test-assert "define-class classes start unsealed"
    (not (class-sealed? macro-defined-point)))
  (test-equal "class-seal! returns the sealed class"
    (class-seal! macro-defined-point)
    macro-defined-point)
  (test-assert "class-seal! marks classes sealed"
    (class-sealed? macro-defined-point))
  (test-assert "class-seal! removes class malleability"
    (not (class-malleable? macro-defined-point)))
  (test-error "sealed classes reject redefinition"
    &assertion-violation
    (redefine-class! macro-defined-point 'macro-defined-point-v2 '(x y z)))
  (test-equal "class-unseal! returns the unsealed class"
    (class-unseal! macro-defined-point)
    macro-defined-point)
  (test-assert "class-unseal! restores Scheme class malleability"
    (class-malleable? macro-defined-point))
  (test-assert "class-unseal! clears Scheme class sealed flag"
    (not (class-sealed? macro-defined-point)))
  (test-error "class-unseal! rejects built-in classes"
    &assertion-violation
    (class-unseal! <class>))
  (test-equal "define-class passes direct supers"
    (class-direct-supers macro-defined-colored-point)
    (list macro-defined-point <object>))
  (test-equal "define-class installs inherited slots"
    (map slot-definition-name (class-slots macro-defined-colored-point))
    '(x y color))
  (test-equal "define-class instances accept initargs"
    (list (slot-ref p 'x) (slot-ref p 'y) (slot-ref p 'color))
    '(3 4 blue))
  (test-equal "define-class slot init values initialize defaults"
    (slot-ref configured 'x)
    7)
  (test-equal "define-class slot init thunks initialize defaults"
    (slot-ref configured 'y)
    8)
  (test-equal "define-class supports Gauche-style #:initform"
    (slot-ref configured 'gauche-initform)
    22)
  (test-equal "define-class supports Gauche-style #:init-form"
    (slot-ref configured 'gauche-init-form)
    33)
  (test-equal "define-class supports Gauche colon init-value"
    (slot-ref gauche-configured 'x)
    17)
  (test-equal "define-class supports Gauche colon initform"
    (slot-ref gauche-configured 'origin)
    41)
  (test-equal "define-class supports Gauche colon init-form"
    (slot-ref gauche-configured 'label)
    52)
  (test-equal "define-class supports Gauche colon allocation values"
    (slot-definition-allocation
      (class-slot-definition macro-defined-gauche-options 'shared))
    #:class)
  (test-equal "define-class class slots accept Gauche colon allocation values"
    (class-slot-ref macro-defined-gauche-options 'shared)
    'gauche-shared)
  (test-equal "define-class invokes class-post-initialize hook"
    (car (cdr macro-post-initialize-events))
    '("macro-post-initialized" macro-post-initialized (x) #f))
  (test-equal "define-class applicable option reaches class-post-initialize hook"
    (car macro-post-initialize-events)
    '("macro-post-initialized-callable" macro-post-initialized-callable (procedure) #t))
  (test-equal "define-class slot init keywords can be customized"
    (slot-ref configured 'renamed)
    11)
  (test-equal "define-class hides non-initializable initargs"
    (class-initargs macro-defined-options)
    '(x y gauche-initform gauche-init-form locked renamed-value))
  (test-error "define-class hides class-allocated initargs"
    &assertion-violation
    (make-instance macro-defined-options #:shared 'bad))
  (test-error "define-class non-initializable slots reject initargs"
    &assertion-violation
    (make-instance macro-defined-options #:hidden 12))
  (test-assert "define-class immutable slot is introspectable"
    (slot-definition-immutable?
      (class-slot-definition macro-defined-options 'locked)))
  (test-error "define-class immutable slots reject mutation"
    &assertion-violation
    (slot-set! configured 'locked 12))
  (test-assert "define-class initform aliases become init thunks"
    (procedure? (slot-definition-init-thunk
                  (class-slot-definition macro-defined-options 'gauche-initform))))
  (test-assert "define-class custom slot bound? hook can see backing storage"
    (not (slot-bound? custom-slot 'visible)))
  (slot-set! custom-slot 'visible 13)
  (test-assert "define-class custom slot setter hook updates backing storage"
    (slot-bound? custom-slot 'visible))
  (test-equal "define-class custom slot reader hook returns computed values"
    (slot-ref custom-slot 'visible)
    '(read (written 13)))
  (test-assert "class-slot-definition finds named slot definitions"
    (slot-definition? (class-slot-definition macro-defined-custom-slot 'visible)))
  (test-assert "class-slot-definition returns false for missing slots"
    (not (class-slot-definition macro-defined-custom-slot 'missing)))
  (test-assert "class-slot-accessor finds named slot accessors"
    (slot-accessor? (class-slot-accessor macro-defined-custom-slot 'visible)))
  (test-assert "class-slot-accessor returns false for missing slots"
    (not (class-slot-accessor macro-defined-custom-slot 'missing)))
  (test-assert "slot-ref-using-class is a generic"
    (generic? slot-ref-using-class))
  (test-assert "slot-set-using-class! is a generic"
    (generic? slot-set-using-class!))
  (test-assert "slot-bound-using-class? is a generic"
    (generic? slot-bound-using-class?))
  (test-equal "slot-ref-using-accessor reads ordinary slots"
    (slot-ref-using-accessor accessor-instance macro-x-accessor)
    1)
  (test-equal "slot-ref-using-class reads inherited slots"
    (slot-ref-using-class macro-defined-point p 'x)
    3)
  (test-equal "slot-ref-using-class returns fallback for missing slots"
    (slot-ref-using-class macro-defined-point p 'missing 'fallback)
    'fallback)
  (test-equal "slot-set-using-class! writes inherited slots"
    (let ([instance (make-instance macro-defined-colored-point #:x 31)])
      (slot-set-using-class! macro-defined-point instance 'x 32)
      (slot-ref-using-class macro-defined-point instance 'x))
    32)
  (test-assert "slot-bound-using-class? reports inherited slots"
    (slot-bound-using-class? macro-defined-point p 'x))
  (test-error "slot-ref-using-class rejects unrelated classes"
    &assertion-violation
    (slot-ref-using-class macro-defined-accessors custom-slot 'x))
  (test-error "slot-bound-using-class? rejects missing slots"
    &assertion-violation
    (slot-bound-using-class? macro-defined-point p 'missing))
  (define-class macro-logged-object () ())
  (define-class macro-logged-point (macro-logged-object)
    ((logged #:init-keyword #:logged)))
  (define macro-logged-events '())
  (define macro-logged-instance
    (make-instance macro-logged-point #:logged 10))
  (define-method (slot-ref-using-class
                   (class <class>) (obj macro-logged-object) slot . fallback)
    (set! macro-logged-events
      (cons (list 'read (slot-definition-name slot)) macro-logged-events))
    (next-method))
  (define-method (slot-set-using-class!
                   (class <class>) (obj macro-logged-object) slot value)
    (set! macro-logged-events
      (cons (list 'write (slot-definition-name slot) value) macro-logged-events))
    (next-method))
  (define-method (slot-bound-using-class?
                   (class <class>) (obj macro-logged-object) slot)
    (set! macro-logged-events
      (cons (list 'bound (slot-definition-name slot)) macro-logged-events))
    (next-method))
  (test-equal "slot-ref dispatches through slot-ref-using-class generic"
    (slot-ref macro-logged-instance 'logged)
    10)
  (test-equal "slot-ref-using-class override receives slot definitions"
    macro-logged-events
    '((read logged)))
  (test-equal "slot-set! dispatches through slot-set-using-class! generic"
    (begin
      (slot-set! macro-logged-instance 'logged 11)
      (slot-ref macro-logged-instance 'logged))
    11)
  (test-equal "slot-set-using-class! override receives slot definitions"
    macro-logged-events
    '((read logged) (write logged 11) (read logged)))
  (test-assert "slot-bound? dispatches through slot-bound-using-class? generic"
    (slot-bound? macro-logged-instance 'logged))
  (test-equal "slot-bound-using-class? override receives slot definitions"
    macro-logged-events
    '((bound logged) (read logged) (write logged 11) (read logged)))
  (test-assert "slot-bound-using-accessor? reports ordinary slots"
    (slot-bound-using-accessor? accessor-instance macro-x-accessor))
  (test-equal "slot-initialize-using-accessor! accepts explicit initargs"
    (let ((instance (make-instance macro-defined-options)))
      (slot-initialize-using-accessor! instance options-x-accessor (list #:x 41))
      (slot-ref instance 'x))
    41)
  (test-equal "slot-initialize-using-accessor! uses init values without initargs"
    (let ((instance (make-instance macro-defined-options #:x 12)))
      (slot-initialize-using-accessor! instance options-x-accessor '())
      (slot-ref instance 'x))
    7)
  (test-equal "slot-initialize-using-accessor! uses init thunks without initargs"
    (let ((instance (make-instance macro-defined-options)))
      (instance-slot-set! instance 1 'old)
      (slot-initialize-using-accessor! instance options-y-accessor '())
      (slot-ref instance 'y))
    8)
  (test-equal "slot-initialize-using-accessor! honors custom init keywords"
    (let ((instance (make-instance macro-defined-options)))
      (slot-initialize-using-accessor! instance options-renamed-accessor
                                      (list #:renamed-value 44))
      (slot-ref instance 'renamed))
    44)
  (test-equal "slot-initialize-using-accessor! updates class-allocated slots"
    (let ((value
           (begin
             (slot-initialize-using-accessor! configured options-shared-accessor
                                             (list #:shared 'accessor-shared))
             (class-slot-ref macro-defined-options 'shared))))
      (class-slot-set! macro-defined-options 'shared 'initial-shared)
      value)
    'accessor-shared)
  (test-error "slot-initialize-using-accessor! rejects non-initializable initargs"
    &assertion-violation
    (slot-initialize-using-accessor! configured options-hidden-accessor
                                     (list #:hidden 12)))
  (test-error "slot-initialize-using-accessor! rejects malformed initargs"
    &assertion-violation
    (slot-initialize-using-accessor! configured options-x-accessor (list #:x)))
  (test-equal "slot-set-using-accessor! writes ordinary slots"
    (begin
      (slot-set-using-accessor! accessor-instance macro-x-accessor 4)
      (slot-ref-using-accessor accessor-instance macro-x-accessor))
    4)
  (test-equal "slot-set-using-accessor! dispatches Scheme writer hooks"
    (begin
      (slot-set-using-accessor! custom-slot custom-visible-accessor 14)
      (slot-ref custom-slot 'backing))
    '(written 14))
  (test-equal "slot-set-using-class! dispatches Scheme writer hooks"
    (begin
      (slot-set-using-class! macro-defined-custom-slot custom-slot 'visible 18)
      (slot-ref custom-slot 'backing))
    '(written 18))
  (test-equal "slot-ref-using-accessor dispatches Scheme reader hooks"
    (slot-ref-using-accessor custom-slot custom-visible-accessor)
    '(read (written 18)))
  (test-equal "slot-ref-using-class dispatches Scheme reader hooks"
    (slot-ref-using-class macro-defined-custom-slot custom-slot 'visible)
    '(read (written 18)))
  (test-assert "slot-bound-using-accessor? dispatches Scheme bound hooks"
    (slot-bound-using-accessor? custom-slot custom-visible-accessor))
  (test-assert "slot-bound-using-class? dispatches Scheme bound hooks"
    (slot-bound-using-class? macro-defined-custom-slot custom-slot 'visible))
  (test-equal "slot-definition-allocation reports instance allocation"
    (slot-definition-allocation (class-slot-definition macro-defined-options 'x))
    #:instance)
  (test-equal "slot-definition-allocation reports class allocation"
    (slot-definition-allocation (class-slot-definition macro-defined-options 'shared))
    #:class)
  (test-assert "class-slot-ref has a setter procedure"
    (has-setter? class-slot-ref))
  (test-equal "class-slot-ref reads class-allocated slots"
    (class-slot-ref macro-defined-options 'shared)
    'initial-shared)
  (test-assert "class-slot-bound? reports initialized class slots"
    (class-slot-bound? macro-defined-options 'shared))
  (test-assert "class-slot-bound? reports unbound class slots"
    (not (class-slot-bound? macro-defined-options 'shared-empty)))
  (test-equal "class-slot-set! updates class-allocated slots"
    (class-slot-set! macro-defined-options 'shared 'updated-shared)
    'updated-shared)
  (test-equal "slot-ref sees class-allocated slot values"
    (slot-ref configured 'shared)
    'updated-shared)
  (test-equal "class-slot-ref sees class-allocated slot updates"
    (class-slot-ref macro-defined-options 'shared)
    'updated-shared)
  (test-error "class-slot-ref rejects instance-allocated slots"
    &assertion-violation
    (class-slot-ref macro-defined-options 'x))
  (test-error "class-slot-set! rejects instance-allocated slots"
    &assertion-violation
    (class-slot-set! macro-defined-options 'x 1))
  (test-error "class-slot-bound? rejects instance-allocated slots"
    &assertion-violation
    (class-slot-bound? macro-defined-options 'x))
  (test-error "class-slot-ref rejects missing slots"
    &assertion-violation
    (class-slot-ref macro-defined-options 'missing))
  (test-assert "slot-definition-options expose supported option keys"
    (memq #:init-value
          (slot-definition-options (class-slot-definition macro-defined-options 'x))))
  (test-equal "slot-definition-option exposes init values"
    (slot-definition-option (class-slot-definition macro-defined-options 'x) #:init-value)
    7)
  (test-equal "slot-definition-option accepts Gauche colon keys"
    (slot-definition-option (class-slot-definition macro-defined-options 'x) ':init-value)
    7)
  (test-equal "slot-definition-option exposes immutable state"
    (slot-definition-option (class-slot-definition macro-defined-options 'locked) #:immutable)
    #t)
  (test-equal "slot-definition-option returns explicit defaults for missing options"
    (slot-definition-option (class-slot-definition macro-defined-options 'x) #:unknown 'fallback)
    'fallback)
  (test-equal "slot-definition-option returns defaults for unknown Gauche colon keys"
    (slot-definition-option (class-slot-definition macro-defined-options 'x) ':unknown 'fallback)
    'fallback)
  (test-assert "slot definitions expose Scheme reader hooks"
    (eq? (slot-definition-slot-ref
           (class-slot-definition macro-defined-custom-slot 'visible))
         macro-custom-slot-ref))
  (test-assert "slot definitions expose Scheme writer hooks"
    (eq? (slot-definition-slot-set!
           (class-slot-definition macro-defined-custom-slot 'visible))
         macro-custom-slot-set!))
  (test-assert "slot definitions expose Scheme bound hooks"
    (eq? (slot-definition-slot-bound?
           (class-slot-definition macro-defined-custom-slot 'visible))
         macro-custom-slot-bound?))
  (test-assert "slot-definition-option exposes Scheme reader hooks"
    (eq? (slot-definition-option
           (class-slot-definition macro-defined-custom-slot 'visible)
           #:slot-ref)
         macro-custom-slot-ref))
  (test-assert "slot-definition-option accepts Gauche colon hook keys"
    (eq? (slot-definition-option
           (class-slot-definition macro-defined-custom-slot 'visible)
           ':slot-ref)
         macro-custom-slot-ref))
  (test-assert "slot accessors expose Scheme reader hooks"
    (eq? (slot-accessor-slot-ref
           (class-slot-accessor macro-defined-custom-slot 'visible))
         macro-custom-slot-ref))
  (test-assert "slot accessors expose Scheme writer hooks"
    (eq? (slot-accessor-slot-set!
           (class-slot-accessor macro-defined-custom-slot 'visible))
         macro-custom-slot-set!))
  (test-assert "slot accessors expose Scheme bound hooks"
    (eq? (slot-accessor-slot-bound?
           (class-slot-accessor macro-defined-custom-slot 'visible))
         macro-custom-slot-bound?))
  (test-assert "slot-accessor-options expose supported option keys"
    (memq #:slot-ref
          (slot-accessor-options
            (class-slot-accessor macro-defined-custom-slot 'visible))))
  (test-assert "slot-accessor-option exposes Scheme reader hooks"
    (eq? (slot-accessor-option
           (class-slot-accessor macro-defined-custom-slot 'visible)
           #:slot-ref)
         macro-custom-slot-ref))
  (test-assert "slot-accessor-option accepts Gauche colon hook keys"
    (eq? (slot-accessor-option
           (class-slot-accessor macro-defined-custom-slot 'visible)
           ':slot-ref)
         macro-custom-slot-ref))
  (test-equal "slot-accessor-option returns defaults for missing options"
    (slot-accessor-option
      (class-slot-accessor macro-defined-custom-slot 'visible)
      #:unknown
      'fallback)
    'fallback)
  (test-assert "define-class accessor option creates getter generic"
    (generic? macro-defined-accessor-x))
  (test-assert "define-class accessor option attaches setter generic"
    (has-setter? macro-defined-accessor-x))
  (test-equal "slot-definition-accessor exposes accessor option names"
    (slot-definition-accessor
      (class-slot-definition macro-defined-accessors 'x))
    'macro-defined-accessor-x)
  (test-equal "slot-definition-getter exposes getter option names"
    (slot-definition-getter
      (class-slot-definition macro-defined-accessors 'y))
    'macro-defined-accessor-y)
  (test-equal "slot-definition-setter exposes setter option names"
    (slot-definition-setter
      (class-slot-definition macro-defined-accessors 'z))
    'macro-defined-accessor-z-set!)
  (test-equal "slot-definition-option exposes accessor option names"
    (slot-definition-option
      (class-slot-definition macro-defined-accessors 'x)
      #:accessor)
    'macro-defined-accessor-x)
  (test-equal "slot-accessor-accessor exposes accessor option names"
    (slot-accessor-accessor
      (class-slot-accessor macro-defined-accessors 'x))
    'macro-defined-accessor-x)
  (test-equal "slot-accessor-option exposes accessor option names"
    (slot-accessor-option
      (class-slot-accessor macro-defined-accessors 'x)
      ':accessor)
    'macro-defined-accessor-x)
  (test-equal "define-class accessor getter reads slots"
    (macro-defined-accessor-x accessor-instance)
    4)
  (test-equal "define-class accessor setter writes slots"
    ((setter macro-defined-accessor-x) accessor-instance 10)
    10)
  (test-equal "define-class accessor setter updates slot"
    (macro-defined-accessor-x accessor-instance)
    10)
  (test-equal "define-class Gauche colon accessor getter reads slots"
    (macro-defined-gauche-y gauche-configured)
    18)
  (test-equal "define-class Gauche colon accessor setter writes slots"
    ((setter macro-defined-gauche-y) gauche-configured 19)
    19)
  (test-equal "define-class Gauche colon accessor setter updates slot"
    (macro-defined-gauche-y gauche-configured)
    19)
  (test-equal "define-class getter option creates read-only generic"
    (macro-defined-accessor-y accessor-instance)
    2)
  (test-assert "define-class getter option does not attach setter generic"
    (not (has-setter? macro-defined-accessor-y)))
  (test-equal "define-class setter option creates standalone setter generic"
    (macro-defined-accessor-z-set! accessor-instance 30)
    30)
  (test-equal "define-class standalone setter updates slot"
    (slot-ref accessor-instance 'z)
    30)
  (test-equal "define-class redefines existing module class bindings"
    (eval
      '(begin
         (define-class macro-redefined-class () (kept removed))
         (define macro-redefined-instance
           (make macro-redefined-class #:kept 31 #:removed 32))
         (define macro-redefined-original-class macro-redefined-class)
         (define-generic macro-redefined-describe)
         (define-method (macro-redefined-describe (obj macro-redefined-class))
           (list 'kept (slot-ref obj 'kept)))
         (define before-redefinition-dispatch
           (macro-redefined-describe macro-redefined-instance))
         (define-class macro-redefined-class () (added kept))
         (list
           (not (eq? macro-redefined-class macro-redefined-original-class))
           (slot-ref macro-redefined-instance 'kept)
           (eq? (current-class-of macro-redefined-instance) macro-redefined-class)
           (slot-ref macro-redefined-instance 'removed 'missing)
           (slot-ref (make macro-redefined-class #:added 33) 'added)
           before-redefinition-dispatch
           (macro-redefined-describe macro-redefined-instance)
           (pair? (class-direct-methods macro-redefined-class))))
      (current-module))
    '(#t 31 #t missing 33 (kept 31) (kept 31) #t))
  (test-error "define-class rejects non-symbol slot names"
    &syntax
    (eval '(define-class bad-defined-class () ((bad slot)))
      (environment '(capy))))
  (test-error "define-class rejects unknown slot options"
    &assertion-violation
    (eval '(define-class bad-defined-slot-option () ((x #:unknown #t)))
      (environment '(capy))))
  (test-error "define-class rejects unsupported class options"
    &syntax
    (eval '(define-class bad-defined-class-option () () #:metaclass #f)
      (environment '(capy)))))

(test-group "class redefinition"
  (define record (make-class 'redef-record '(kept removed)))
  (define instance (make-instance record #:kept 1 #:removed 2))
  (define instance-for-touch (make-instance record #:kept 4))
  (define redefined (redefine-class! record 'redef-record-v2 '(added kept)))
  (define old-super (make-class 'redef-old-super '(old-base)))
  (define new-super (make-class 'redef-new-super '(new-base)))
  (define changing-subclass
    (make-class 'redef-changing-subclass '(child-slot) (list old-super)))
  (define changed-subclass #f)

  (test-assert "redefinition returns class object" (class? redefined))
  (test-equal "class-of resolves redefined class through table" (class-of instance) redefined)
  (test-equal "current-class-of exposes stale instance class before touch"
    (current-class-of instance)
    record)
  (test-assert "%class-redefined? reports stale instances"
    (%class-redefined? instance))
  (test-assert "%class-redefined? reports stale class descriptors"
    (%class-redefined? record))
  (test-assert "%class-redefined? rejects current class descriptors"
    (not (%class-redefined? redefined)))
  (test-assert "%class-redefined? rejects non-class values"
    (not (%class-redefined? 'not-a-class)))
  (test-equal "touch-instance! returns the touched instance"
    (touch-instance! instance-for-touch)
    instance-for-touch)
  (test-equal "touch-instance! updates stale instance class"
    (current-class-of instance-for-touch)
    redefined)
  (test-assert "%class-redefined? rejects touched instances"
    (not (%class-redefined? instance-for-touch)))
  (test-error "touch-instance! rejects non-instances"
    &assertion-violation
    (touch-instance! redefined))
  (test-equal "redefined class name" (class-name redefined) "redef-record-v2")
  (test-equal "preserved slot keeps value" (slot-ref instance 'kept) 1)
  (test-equal "current-class-of exposes touched instance class after access"
    (current-class-of instance)
    redefined)
  (test-equal "added slot initarg applies to new instances"
    (slot-ref (make-instance redefined #:added 3) 'added)
    3)
  (test-equal "removed slot is missing" (slot-ref instance 'removed 'missing) 'missing)
  (test-assert "old superclass initially tracks direct subclass"
    (memq changing-subclass (class-direct-subclasses old-super)))
  (set! changed-subclass
    (redefine-class!
      changing-subclass
      'redef-changing-subclass-v2
      '(child-slot)
      (list new-super)))
  (test-assert "redefinition removes old superclass direct subclass edge"
    (not (memq changing-subclass (class-direct-subclasses old-super))))
  (test-assert "redefinition adds new superclass direct subclass edge"
    (memq changed-subclass (class-direct-subclasses new-super)))
  (test-equal "redefinition recomputes inherited slots from new supers"
    (map slot-definition-name (class-slots changed-subclass))
    '(new-base child-slot)))

(test-group "change-class"
  (define source (make-class 'change-source '(kept removed)))
  (define target (make-class 'change-target '(added kept)))
  (define source-instance (make-instance source #:kept 10 #:removed 20))
  (define-class change-target-default ()
    ((added #:init-value 30)
     kept))
  (define defaulted-instance (make-instance source #:kept 11 #:removed 21))

  (test-equal "change-class returns the changed instance"
    (change-class source-instance target)
    source-instance)
  (test-equal "change-class updates class-of through object header"
    (class-of source-instance)
    target)
  (test-equal "change-class updates current-class-of"
    (current-class-of source-instance)
    target)
  (test-equal "change-class preserves same-name slot values"
    (slot-ref source-instance 'kept)
    10)
  (test-assert "change-class leaves added slots without defaults unbound"
    (not (slot-bound? source-instance 'added)))
  (change-class defaulted-instance change-target-default)
  (test-equal "change-class seeds added slots from target defaults"
    (slot-ref defaulted-instance 'added)
    30)
  (test-equal "change-class preserves same-name slots with defaulted targets"
    (slot-ref defaulted-instance 'kept)
    11)
  (test-equal "change-class drops removed slots"
    (slot-ref source-instance 'removed 'missing)
    'missing)
  (test-error "change-class rejects non-instances"
    &assertion-violation
    (change-class target target))
  (test-error "change-class rejects built-in target classes"
    &assertion-violation
    (change-class source-instance <class>)))

(test-group "generic dispatch"
  (define point (make-class 'generic-point '(x)))
  (define base (make-class 'generic-base '()))
  (define derived (make-class 'generic-derived '() (list base)))
  (define describe (make-generic 'describe))
  (define combine (make-generic 'combine 2))
  (define fallback (make-generic 'fallback))
  (define p (make-instance point #:x 5))
  (define base-instance (make-instance base))
  (define derived-instance (make-instance derived))

  (define describe-method
    (add-method! describe
      (list point)
      1
      (lambda (next obj)
        (list 'point (slot-ref obj 'x)))))

  (test-assert "class direct methods expose registered method metadata"
    (pair? (class-direct-methods point)))
  (test-assert "generic predicate accepts closure wrappers"
    (generic? describe))
  (test-equal "generic names are inspectable from wrappers"
    (generic-name describe)
    "describe")
  (test-equal "generic methods expose method descriptors"
    (generic-methods describe)
    (list describe-method))
  (test-equal "generic required dispatch arg count is inspectable"
    (generic-required-dispatch-arg-count combine)
    2)
  (test-assert "method predicate accepts method descriptors"
    (method? describe-method))
  (test-equal "method generic exposes its descriptor name"
    (generic-name (method-generic describe-method))
    "describe")
  (test-equal "method specializers expose class objects"
    (method-specializers describe-method)
    (list point))
  (test-equal "method required arg count is inspectable"
    (method-required-arg-count describe-method)
    1)
  (test-assert "method body is inspectable"
    (procedure? (method-body describe-method)))
  (test-assert "method-options expose supported option keys"
    (memq #:specializers (method-options describe-method)))
  (test-equal "method-option exposes specializers"
    (method-option describe-method #:specializers)
    (list point))
  (test-equal "method-option accepts Gauche colon keys"
    (method-option describe-method ':required-arg-count)
    1)
  (test-equal "method-option returns defaults for missing options"
    (method-option describe-method ':unknown 'fallback)
    'fallback)
  (test-assert "method-applicable-for-classes? accepts exact classes"
    (method-applicable-for-classes? describe-method point))
  (test-assert "method-applicable-for-classes? rejects unrelated classes"
    (not (method-applicable-for-classes? describe-method base)))
  (test-assert "method-applicable-for-classes? rejects missing required classes"
    (not (method-applicable-for-classes? describe-method)))
  (test-assert "dispatch returns method body"
    (procedure? (generic-dispatch describe p)))
  (test-equal "generic-invoke dispatches by class"
    (generic-invoke describe p)
    '(point 5))
  (test-equal "generic object is directly applicable"
    (describe p)
    '(point 5))

  (define combine-base-method
    (add-method! combine
      (list base base)
      2
      (lambda (next left right)
        '(base base))))
  (test-assert "method-applicable-for-classes? accepts subclass arguments"
    (method-applicable-for-classes? combine-base-method derived derived))
  (add-method! combine
    (list base derived)
    2
    (lambda (next left right)
      '(base derived)))
  (test-equal "generic dispatch handles multiple specializers"
    (combine base-instance derived-instance)
    '(base derived))
  (test-error "generic dispatch rejects missing required dispatch argument"
    &assertion-violation
    (combine base-instance))

  (add-method! describe
    (list point)
    1
    (lambda (next obj)
      (list 'replacement (slot-ref obj 'x))))
  (test-equal "method replacement invalidates dispatch"
    (generic-invoke describe p)
    '(replacement 5))

  (set-generic-fallback! fallback
    (lambda (obj)
      (list 'fallback obj)))
  (test-equal "fallback receives original args"
    (generic-invoke fallback 'value)
    '(fallback value)))

(test-group "next methods"
  (define point (make-class 'next-point '(x)))
  (define colored-point (make-class 'next-colored-point '(color) (list point)))
  (define describe (make-generic 'describe-next))
  (define cp (make-instance colored-point #:x 7 #:color 'blue))

  (define point-method
    (add-method! describe
      (list point)
      1
      (lambda (next obj)
        (list 'point (slot-ref obj 'x)))))
  (define colored-method
    (add-method! describe
      (list colored-point)
      1
      (lambda (next obj)
        (list 'colored (slot-ref obj 'color) (next-method-invoke next)))))
  (define next-chain (generic-next-methods describe cp))

  (test-equal "next-method advances to less specific method"
    (generic-invoke describe cp)
    '(colored blue (point 7)))
  (test-equal "compute-applicable-methods returns matching methods"
    (compute-applicable-methods describe (list cp))
    (list point-method colored-method))
  (test-equal "sort-applicable-methods orders by specificity"
    (sort-applicable-methods describe
      (compute-applicable-methods describe (list cp))
      (list cp))
    (list colored-method point-method))
  (test-equal "next-method-methods exposes sorted runtime method chains"
    (next-method-methods next-chain)
    (list colored-method point-method))
  (test-assert "next-method predicate accepts descriptors"
    (next-method? next-chain))
  (test-assert "next-method predicate rejects non-descriptors"
    (not (next-method? describe)))
  (test-equal "next-method-generic exposes owning generic"
    (generic-name (next-method-generic next-chain))
    "describe-next")
  (test-equal "next-method-args exposes dispatch args"
    (next-method-args next-chain)
    (list cp))
  (test-equal "next-method-index exposes current method index"
    (next-method-index next-chain)
    0)
  (test-assert "next-method-has-next? reports remaining methods"
    (next-method-has-next? next-chain))
  (test-assert "next-method-options expose supported option keys"
    (memq #:methods (next-method-options next-chain)))
  (test-equal "next-method-option accepts Gauche colon keys"
    (next-method-option next-chain ':index)
    0)
  (test-equal "next-method-option exposes next descriptor"
    (next-method-methods (next-method-option next-chain ':next))
    (list colored-method point-method))
  (test-equal "next-method-option returns defaults for missing options"
    (next-method-option next-chain ':unknown 'fallback)
    'fallback)
  (test-assert "method-more-specific? compares class CPL positions"
    (method-more-specific? colored-method point-method (list colored-point)))
  (test-equal "apply-generic invokes through applicable methods"
    (apply-generic describe (list cp))
    '(colored blue (point 7)))
  (test-equal "apply-methods invokes an explicit sorted method list"
    (apply-methods describe (list colored-method point-method) (list cp))
    '(colored blue (point 7))))

(test-group "generic definition macros"
  (define point (make-class 'macro-point '(x)))
  (define colored-point (make-class 'macro-colored-point '(color) (list point)))
  (define base (make-class 'macro-base '()))
  (define derived (make-class 'macro-derived '() (list base)))
  (define p (make-instance point #:x 9))
  (define cp (make-instance colored-point #:x 11 #:color 'green))
  (define base-instance (make-instance base))
  (define derived-instance (make-instance derived))

  (define-generic macro-describe)
  (define-generic macro-combine 2)
  (define-generic macro-next)
  (define-generic macro-locked)
  (define-generic macro-keyword-locked)
  (define-generic macro-split)
  (define-generic macro-keyword-split)
  (define-generic macro-options)
  (define-generic macro-ref)
  (define-generic (setter macro-ref))
  (define-generic macro-sealed)
  (define-generic macro-fallback-option
    :fallback (lambda (obj) (list 'fallback-option obj)))
  (define-generic macro-keyword-fallback-option
    #:fallback (lambda (obj) (list 'keyword-fallback-option obj)))
  (define-generic macro-sealed-option :sealed #t)
  (define-generic macro-option-dispatch 2
    :fallback (lambda (left right) (list 'fallback-pair left right)))
  (define-generic macro-option-ref)
  (define-generic (setter macro-option-ref)
    :fallback (lambda (obj value) (list 'setter-fallback obj value)))

  (define-method (macro-describe (obj point))
    (list 'point (slot-ref obj 'x)))
  (define-method (macro-describe (obj colored-point))
    (list 'colored (slot-ref obj 'color) (next-method-invoke next)))
  (define-method (macro-next (obj point))
    (list 'point (slot-ref obj 'x)))
  (define-method (macro-next (obj colored-point))
    (list 'colored (slot-ref obj 'color) (next-method)))
  (define-method (macro-combine (left base) (right base))
    '(base base))
  (define-method (macro-combine (left base) (right derived))
    '(base derived))
  (define-method (macro-describe obj)
    (list 'any obj))
  (define-method (macro-describe (obj point) prefix . rest)
    (list prefix (slot-ref obj 'x) rest))
  (define-method (macro-locked (obj point)) :locked
    (list 'locked (slot-ref obj 'x)))
  (define-method (macro-keyword-locked (obj point)) #:locked
    (list 'keyword-locked (slot-ref obj 'x)))
  (define-method macro-split :locked ((obj point))
    (list 'split (slot-ref obj 'x)))
  (define-method macro-keyword-split #:locked ((obj point))
    (list 'keyword-split (slot-ref obj 'x)))
  (define-method (macro-options (obj point) #:optional (prefix 'default) #:key (scale 1))
    (list prefix (* (slot-ref obj 'x) scale)))
  (define-method (macro-ref (obj point))
    (slot-ref obj 'x))
  (define-method ((setter macro-ref) (obj point) value)
    (slot-set! obj 'x value)
    value)
  (define-method (macro-sealed (obj point))
    (list 'sealed (slot-ref obj 'x)))

  (test-equal "define-generic creates invocable generic"
    (macro-describe p)
    '(point 9))
  (test-equal "define-method exposes next method binding"
    (macro-describe cp)
    '(colored green (point 11)))
  (test-equal "define-method exposes Gauche-style next-method procedure"
    (macro-next cp)
    '(colored green (point 11)))
  (test-equal "define-method handles multiple specializers"
    (macro-combine base-instance derived-instance)
    '(base derived))
  (test-equal "define-method handles unspecialized fallback methods"
    (macro-describe 'plain)
    '(any plain))
  (test-equal "define-method handles trailing unspecialized and rest args"
    (macro-describe p 'prefix 1 2 3)
    '(prefix 9 (1 2 3)))
  (test-equal "define-method accepts locked qualifier"
    (macro-locked p)
    '(locked 9))
  (test-assert "define-method records locked qualifier"
    (method-locked? (car (generic-methods macro-locked))))
  (test-assert "method-option exposes locked qualifier"
    (method-option (car (generic-methods macro-locked)) #:locked))
  (test-equal "define-method accepts keyword locked qualifier"
    (macro-keyword-locked p)
    '(keyword-locked 9))
  (test-assert "define-method records keyword locked qualifier"
    (method-locked? (car (generic-methods macro-keyword-locked))))
  (test-error "locked methods reject replacement"
    &assertion-violation
    (add-method! macro-locked
      (list point)
      1
      (lambda (next obj) (list 'replaced obj))))
  (test-equal "define-method accepts Gauche-style split form"
    (macro-split p)
    '(split 9))
  (test-equal "define-method accepts split keyword locked qualifier"
    (macro-keyword-split p)
    '(keyword-split 9))
  (test-assert "define-method records split keyword locked qualifier"
    (method-locked? (car (generic-methods macro-keyword-split))))
  (test-error "define-method rejects unsupported inline qualifiers"
    &syntax
    (eval '(define-method (macro-unsupported-inline (obj point)) :before obj)
      (current-module)))
  (test-error "define-method rejects unsupported split qualifiers"
    &syntax
    (eval '(define-method macro-unsupported-split :before ((obj point)) obj)
      (current-module)))
  (test-equal "define-method handles optional method arguments"
    (macro-options p)
    '(default 9))
  (test-equal "define-method handles keyword method arguments"
    (macro-options p 'scaled #:scale 3)
    '(scaled 27))
  (test-assert "define-generic installs setter generic"
    (generic? (setter macro-ref)))
  (test-assert "has-setter? reports setter generic"
    (has-setter? macro-ref))
  (test-equal "setter generic dispatches"
    ((setter macro-ref) p 42)
    42)
  (test-equal "setter generic updates slots"
    (macro-ref p)
    42)
  (test-equal "define-generic fallback option installs fallback procedure"
    (macro-fallback-option 'plain)
    '(fallback-option plain))
  (test-assert "generic-options expose supported option keys"
    (memq #:fallback (generic-options macro-fallback-option)))
  (test-assert "generic-option exposes fallback procedures"
    (procedure? (generic-option macro-fallback-option #:fallback)))
  (test-equal "generic-option exposes dispatch arg counts"
    (generic-option macro-option-dispatch ':required-dispatch-arg-count)
    2)
  (test-assert "generic-option accepts Gauche colon keys"
    (procedure? (generic-option macro-fallback-option ':fallback)))
  (test-equal "generic-option returns defaults for missing options"
    (generic-option macro-fallback-option ':unknown 'fallback)
    'fallback)
  (test-equal "define-generic keyword fallback option installs fallback procedure"
    (macro-keyword-fallback-option 'plain)
    '(keyword-fallback-option plain))
  (test-equal "define-generic fallback option honors dispatch arity"
    (macro-option-dispatch 'left 'right)
    '(fallback-pair left right))
  (test-assert "define-generic sealed option seals generic"
    (generic-sealed? macro-sealed-option))
  (test-error "define-generic sealed option rejects later methods"
    &assertion-violation
    (add-method! macro-sealed-option
      (list point)
      1
      (lambda (next obj) obj)))
  (test-equal "define-generic setter fallback option installs setter fallback"
    ((setter macro-option-ref) 'plain 99)
    '(setter-fallback plain 99))
  (test-error "define-generic rejects unsupported options"
    &syntax
    (eval '(define-generic macro-unsupported-generic-option :class <generic>)
      (current-module)))
  (test-assert "generic-sealed? starts false"
    (not (generic-sealed? macro-sealed)))
  (generic-seal! macro-sealed)
  (test-assert "generic-seal! marks generic sealed"
    (generic-sealed? macro-sealed))
  (test-error "sealed generics reject method replacement"
    &assertion-violation
    (add-method! macro-sealed
      (list point)
      1
      (lambda (next obj) (list 'replaced obj))))
  (test-equal "sealed generic keeps existing method"
    (macro-sealed p)
    '(sealed 42))
  (generic-unseal! macro-sealed)
  (test-assert "generic-unseal! clears sealed flag"
    (not (generic-sealed? macro-sealed)))
  (add-method! macro-sealed
    (list point)
    1
    (lambda (next obj) (list 'unsealed (slot-ref obj 'x))))
  (test-equal "unsealed generic accepts replacement"
    (macro-sealed p)
    '(unsealed 42))
  (test-error "define-method rejects non-prefix specializers"
    &syntax
    (eval '(define-method (macro-combine left (right derived)) '(bad))
      (environment '(capy)))))

(test-group "invocable scheme classes"
  (define callable (make-invocable-class 'callable '(procedure label)))
  (define proc
    (make-instance callable
      #:procedure (lambda (self arg)
                    (list (slot-ref self 'label) arg))
      #:label 'wrapped))
  (define instance (procedure-property proc 'instance))

  (test-assert "invocable allocation returns procedure"
    (procedure? proc))
  (test-assert "invocable class is marked applicable"
    (class-applicable? callable))
  (test-assert "invocable wrapper carries instance property"
    (scheme-instance? instance))
  (test-assert "invocable wrapper carries class property"
    (eq? (procedure-property proc 'class) callable))
  (test-equal "invocable wrapper exposes raw instance slots"
    (slot-ref instance 'label)
    'wrapped)
  (test-equal "invocable wrapper calls stored procedure with instance"
    (proc 42)
    '(wrapped 42))
  (test-equal "make returns initialized invocable wrappers"
    (let ([made
            (make callable
              #:procedure
              (lambda (self arg)
                (list 'made (slot-ref self 'label) arg))
              #:label 'via-make)])
      (list
        (procedure? made)
        (scheme-instance? (procedure-property made 'instance))
        (made 5)))
    '(#t #t (made via-make 5)))
  (test-equal "define-class applicable option creates invocable classes"
    (eval
      '(begin
         (define-class macro-callable ()
           (procedure label)
           :applicable #t)
         (define macro-proc
           (make-instance macro-callable
             #:procedure (lambda (self arg)
                           (list 'macro (slot-ref self 'label) arg))
             #:label 'first))
         (define macro-instance (procedure-property macro-proc 'instance))
         (define-class macro-callable ()
           (procedure label extra)
           :applicable #t)
         (define macro-proc-v2
           (make-instance macro-callable
             #:procedure (lambda (self) (slot-ref self 'extra))
             #:extra 'new))
         (list
           (class-applicable? macro-callable)
           (procedure? macro-proc)
           (scheme-instance? macro-instance)
           (macro-proc 7)
           (class-applicable? (procedure-property macro-proc 'class))
           (class-name (current-class-of macro-instance))
           (slot-ref (procedure-property macro-proc-v2 'instance) 'extra)))
      (current-module))
    '(#t #t #t (macro first 7) #t "macro-callable" new))
  (test-error "invocable class requires procedure slot"
    &assertion-violation
    (make-invocable-class 'bad-callable '(label)))
  (test-error "define-class applicable option requires procedure slot"
    &assertion-violation
    (eval '(define-class bad-macro-callable () (label) #:applicable #t)
      (environment '(capy))))
  (test-error "define-class applicable option must be boolean"
    &syntax
    (eval '(define-class bad-applicable-option () (procedure) #:applicable 'yes)
      (environment '(capy))))
  (test-error "invocable instance requires procedure value"
    &assertion-violation
    (make-instance callable #:procedure 'not-a-procedure #:label 'bad)))

(test-end "boot class")
