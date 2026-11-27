;; R6RS/R7RS numeric syntax edge cases for `string->number` (docs/REMEDIATION-PLAN.md, R6). The
;; parser is pkgs/core/src/boot/str2num.scm.
(import (rnrs)
        (srfi 64))

(test-begin "string->number syntax")

(define-syntax numbers
  (syntax-rules ()
    [(_ (text expected) ...)
     (begin (test-equal text expected (string->number text)) ...)]))

(define-syntax rejected
  (syntax-rules ()
    [(_ text ...) (begin (test-equal (string-append "rejects " text) #f (string->number text)) ...)]))

(test-group "exactness prefixes"
  (numbers ("#e1.5" 3/2) ("#e1e3" 1000) ("#e1e-2" 1/100) ("#i3/4" 0.75) ("#i1" 1.0)
           ("#e.25" 1/4) ("#x#e1" 1) ("#e#x1f" 31) ("#i#b101" 5.0))
  (rejected "#e#i1" "#i#e1" "#e" "#e#e1"))

(test-group "exact has no infinity or NaN"
  (rejected "#e+inf.0" "#e-inf.0" "#e+nan.0" "#e1+inf.0i" "#e1+nan.0i"))

(test-group "radixes"
  (numbers ("#b101" 5) ("#o17" 15) ("#xFF" 255) ("#xff/2" 255/2) ("#d10" 10) ("#x1e2" 482)
           ("#b-101" -5))
  (rejected "#b2" "#o8" "#xg" "#x1.8" "#b1.1"))

(test-group "decimal points, exponents and precision markers"
  (numbers ("1." 1.0) (".5" 0.5) ("-.5" -0.5) ("1e2" 100.0) ("1E2" 100.0) ("1e-2" 0.01)
           ("1.5e2" 150.0) ("1.5s2" 150.0) ("1.5f2" 150.0) ("1.5d2" 150.0) ("1.5l2" 150.0)
           ("1e400" +inf.0) ("1e-400" 0.0))
  (rejected "." "e1" "1e" "1e+" "1.5e" "1..5" "1.5.5"))

(test-group "mantissa width"
  ;; `|k` is accepted and does not change the value: flonums have one width.
  (numbers ("1.5|53" 1.5) ("1.5e2|24" 150.0) ("1.5|64" 1.5))
  (rejected "1.5|" "|53" "1.5|x"))

(test-group "# digit placeholders"
  (numbers ("1#.#" 10.0) ("1#/2" 5.0) ("12#.#e1" 1200.0))
  (rejected "#1" "1#1"))

(test-group "fractions"
  (numbers ("1/2" 1/2) ("-3/6" -1/2) ("+4/2" 2))
  (rejected "1/0" "1/" "/2" "1/2/3" "1/-2"))

(test-group "special flonums"
  (numbers ("+inf.0" +inf.0) ("-inf.0" -inf.0) ("-0.0" -0.0) ("+0" 0) ("00" 0) ("0e0" 0.0))
  (test-assert "+nan.0 is a NaN" (nan? (string->number "+nan.0")))
  (test-assert "-nan.0 is a NaN" (nan? (string->number "-nan.0")))
  (rejected "inf.0" "nan.0" "+inf" "+nan" "+inf.1"))

(test-group "complex rectangular"
  (numbers ("+i" +i) ("-i" -i) ("1+i" 1+i) ("1-2i" 1-2i) ("1/2+3/4i" 1/2+3/4i) ("+2i" +2i))
  (test-equal "uppercase I" 1+i (string->number "1+I"))
  (test-equal "uppercase I alone" +i (string->number "+I"))
  (test-equal "inexact imaginary" 1.0+2.5i (string->number "1+2.5i"))
  (rejected "i" "1+" "1+2" "+i+i" "1+i+i" "1i" "1+ii"))

(test-group "complex with infinity and NaN"
  (test-equal "infinite imaginary part" 1.0+inf.0i (string->number "1+inf.0i"))
  (test-equal "negative infinite imaginary part" 1.0-inf.0i (string->number "1-inf.0i"))
  (test-equal "bare infinite imaginary part" +inf.0i (string->number "+inf.0i"))
  (test-assert "NaN imaginary part" (nan? (imag-part (string->number "1+nan.0i"))))
  (test-assert "NaN alone as imaginary part" (nan? (imag-part (string->number "+nan.0i")))))

(test-group "polar"
  (test-equal "magnitude 1 angle 0" 1 (string->number "1@0"))
  (test-assert "real result for a zero angle" (real? (string->number "2@0")))
  (test-assert "angle pi/2 is imaginary"
    (let ([z (string->number "1@1.5707963267948966")])
      (and (< (abs (real-part z)) 1e-12) (< (abs (- (imag-part z) 1.0)) 1e-12))))
  (test-assert "a NaN angle gives NaN parts" (nan? (real-part (string->number "1@+nan.0"))))
  (rejected "1@" "@1" "1@@1" "1@+i"))

(test-group "radix argument"
  (test-equal "radix 2" 5 (string->number "101" 2))
  (test-equal "radix 16" 255 (string->number "ff" 16))
  (test-equal "radix 16 uppercase" 255 (string->number "FF" 16))
  (test-equal "a prefix beats the radix argument" 255 (string->number "#xff" 10))
  (test-equal "radix 8" 8 (string->number "10" 8))
  (test-equal "digits out of radix" #f (string->number "129" 8)))

(test-group "plain rejects"
  (rejected "" "+" "-" "--1" "++1" "1_0" "1 " " 1" "0x10" "1,5" "abc" "+-1"))

(test-end "string->number syntax")
