(import (srfi 64)
        (capy))

(test-begin "boot class direct supers")

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

(test-end "boot class direct supers")
