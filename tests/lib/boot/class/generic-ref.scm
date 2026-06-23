(import (srfi 64)
        (capy))

(test-begin "boot class generic ref helpers")

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

(test-end "boot class generic ref helpers")
