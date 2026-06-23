(import (srfi 64)
        (capy))

(test-begin "boot class define-class macro")

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

(test-end "boot class define-class macro")
