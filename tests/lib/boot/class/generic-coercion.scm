(import (srfi 64)
        (capy))

(test-begin "boot class generic coercion helpers")

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

(test-end "boot class generic coercion helpers")
