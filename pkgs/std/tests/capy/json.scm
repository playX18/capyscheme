(import (rnrs)
  (only (srfi 39) parameterize)
  (srfi 64)
  (capy json))

(test-begin "capy json")

(define (bytevector . bytes) (u8-list->bytevector bytes))

;; The &json-error of THUNK as (line column), 'other for another condition, #f for none.
(define (error-position thunk)
  (guard (e [(json-error? e) (list (json-error-line e) (json-error-column e))] [#t 'other])
    (thunk)
    #f))

(define (error-message thunk)
  (guard (e [(and (json-error? e) (message-condition? e)) (condition-message e)] [#t 'other])
    (thunk)
    #f))

(define (rejects? text)
  (guard (e [(json-error? e) #t] [#t #f])
    (string->json text)
    #f))

(define (round-trip text) (json->string (string->json text)))

(define (contains? s part)
  (let ([n (string-length s)] [m (string-length part)])
    (let loop ([i 0])
      (and (<= (+ i m) n)
        (or (string=? (substring s i (+ i m)) part) (loop (+ i 1)))))))

(test-group "reading values"
  (test-equal "object" '((a . 1) (b . "x")) (string->json "{\"a\":1,\"b\":\"x\"}"))
  (test-equal "empty object" '() (string->json "{}"))
  (test-equal "array" '#(1 2 3) (string->json "[1,2,3]"))
  (test-equal "empty array" '#() (string->json "[ ]"))
  (test-equal "nested" '((a . #(((b . #t)) null #f))) (string->json "{\"a\":[{\"b\":true},null,false]}"))
  (test-equal "scalar at top level" "hi" (string->json "\"hi\""))
  (test-equal "whitespace around" 5 (string->json " \t\r\n 5 \n"))
  (test-equal "member order is kept" '(z y x) (map car (string->json "{\"z\":1,\"y\":2,\"x\":3}")))
  (test-equal "duplicate keys are kept" '((a . 1) (a . 2)) (string->json "{\"a\":1,\"a\":2}")))

(test-group "numbers"
  (test-equal "integer is exact" 42 (string->json "42"))
  (test-assert "integer is exact (exact?)" (exact? (string->json "42")))
  (test-equal "negative" -7 (string->json "-7"))
  (test-equal "minus zero integer" 0 (string->json "-0"))
  (test-equal "bignum" 123456789012345678901234567890 (string->json "123456789012345678901234567890"))
  (test-equal "fraction is a flonum" 2.5 (string->json "2.5"))
  (test-assert "1.0 is inexact" (inexact? (string->json "1.0")))
  (test-equal "exponent" 100.0 (string->json "1e2"))
  (test-equal "exponent with sign" 0.01 (string->json "1E-2"))
  (test-equal "exponent plus" 1000.0 (string->json "1e+3"))
  (test-equal "huge exponent is infinite" +inf.0 (string->json "1e400"))
  (test-equal "tiny exponent is zero" 0.0 (string->json "1e-400"))
  (test-equal "correctly rounded" 2.2250738585072014e-308 (string->json "2.2250738585072011e-308"))
  (test-equal "0.1" 0.1 (string->json "0.1")))

(test-group "strings"
  (test-equal "escapes" (string #\" #\\ #\/ #\backspace #\xC #\newline #\return #\tab) (string->json "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""))
  (test-equal "\\u escape" "\xE9;" (string->json "\"\\u00e9\""))
  (test-equal "upper-case hex" "\xE9;" (string->json "\"\\u00E9\""))
  (test-equal "surrogate pair" "\x1F600;" (string->json "\"\\ud83d\\ude00\""))
  (test-equal "lone high surrogate" "\xFFFD;x" (string->json "\"\\ud83dx\""))
  (test-equal "lone low surrogate" "\xFFFD;" (string->json "\"\\ude00\""))
  (test-equal "high then a non-surrogate escape" "\xFFFD;A" (string->json "\"\\ud83d\\u0041\""))
  (test-equal "high then a simple escape" "\xFFFD;\n" (string->json "\"\\ud83d\\n\""))
  (test-equal "two high surrogates then a low" "\xFFFD;\x1F600;"
    (string->json "\"\\ud83d\\ud83d\\ude00\""))
  (test-equal "\\u0000" (string #\nul) (string->json "\"\\u0000\""))
  (test-equal "non-ASCII text" "λ😀" (string->json "\"λ😀\""))
  (test-equal "DEL is allowed raw" (string #\x7F) (string->json (string #\" #\x7F #\")))
  (test-equal "escape between plain text" "ab\ncd" (string->json "\"ab\\ncd\"")))

(test-group "options"
  (test-equal "string keys" '(("a" . 1)) (string->json "{\"a\":1}" #:key 'string))
  (test-equal "key procedure" '((A . 1)) (string->json "{\"a\":1}" #:key (lambda (s) (string->symbol (string-upcase s)))))
  (test-equal "list arrays" '(1 (2 3)) (string->json "[1,[2,3]]" #:array 'list))
  (test-equal "null value" '#(#f 1) (string->json "[null,1]" #:null #f))
  (let ([t (string->json "{\"a\":1,\"b\":{\"c\":2},\"a\":3}" #:object 'hashtable)])
    (test-assert "hashtable" (hashtable? t))
    (test-equal "hashtable, last duplicate wins" 3 (hashtable-ref t 'a #f))
    (test-equal "nested hashtable" 2 (hashtable-ref (hashtable-ref t 'b #f) 'c #f)))
  (let ([t (string->json "{\"a\":1}" #:object 'hashtable #:key 'string)])
    (test-equal "string-keyed hashtable" 1 (hashtable-ref t "a" #f)))
  (test-equal "object procedure" '(obj ("a" . 1))
    (string->json "{\"a\":1}" #:key 'string #:object (lambda (ms) (cons 'obj ms))))
  (test-equal "parameters" '(1 null)
    (parameterize ([json-array-type 'list]) (string->json "[1,null]")))
  (test-equal "null parameter" '#(none)
    (parameterize ([json-null 'none]) (string->json "[null]")))
  (test-equal "keywords win over parameters" '#(1)
    (parameterize ([json-array-type 'list]) (string->json "[1]" #:array 'vector)))
  (test-assert "bad option" (guard (e [(assertion-violation? e) #t]) (string->json "1" #:object 'tree) #f)))

(test-group "depth"
  (let ([deep (lambda (n) (string-append (make-string n #\[) (make-string n #\])))])
    (test-equal "512 levels" 1 (vector-length (string->json (deep 512))))
    (test-assert "513 levels" (rejects? (deep 513)))
    (test-assert "100000 opening brackets" (rejects? (make-string 100000 #\[)))
    (test-equal "max-depth keyword" '#(#()) (string->json "[[]]" #:max-depth 2))
    (test-assert "max-depth keyword rejects"
      (guard (e [(json-error? e) #t]) (string->json "[[[]]]" #:max-depth 2) #f))
    (test-assert "max-depth 1"
      (guard (e [(json-error? e) #t]) (string->json "[[]]" #:max-depth 1) #f))))

(test-group "errors"
  (test-equal "position of a bad character" '(2 7) (error-position (lambda () (string->json "{\n  \"a\" 1}"))))
  (test-equal "position at the start" '(1 1) (error-position (lambda () (string->json "x"))))
  (test-equal "empty input" '(1 1) (error-position (lambda () (string->json ""))))
  (test-equal "trailing garbage" '(1 4) (error-position (lambda () (string->json "[1]x"))))
  (test-equal "invalid number position" '(1 2) (error-position (lambda () (string->json "[01]"))))
  (test-equal "offset" 3
    (guard (e [(json-error? e) (json-error-offset e)]) (string->json "[1,]")))
  (test-assert "message names what was found"
    (contains? (error-message (lambda () (string->json "[1 2]"))) "expected ',' or ']'"))
  (test-assert "message says line and column"
    (contains? (error-message (lambda () (string->json "[1 2]"))) "line 1, column 4"))
  (test-assert "trailing comma hint"
    (contains? (error-message (lambda () (string->json "[1,]"))) "trailing commas"))
  (test-assert "single quote hint"
    (contains? (error-message (lambda () (string->json "['a']"))) "double quotes"))
  (test-assert "comment hint"
    (contains? (error-message (lambda () (string->json "[1]//"))) "comments"))
  (test-assert "control character"
    (contains? (error-message (lambda () (string->json (string #\" #\tab #\")))) "U+0009"))
  (test-assert "unterminated string"
    (contains? (error-message (lambda () (string->json "\"abc"))) "unterminated"))
  (test-equal "line counting" '(3 2)
    (error-position (lambda () (string->json "[\n1,\n ?]")))))

;; Cases in the style of JSONTestSuite (github.com/nst/JSONTestSuite): y_ must be accepted,
;; n_ rejected, i_ either, but never with a crash.
(define accept
  '("[[]   ]" "[\"\"]" "[]" "[\"a\"]" "[false]" "[null, 1, \"1\", {}]" "[null]" "[1\n]"
    " [1]" "[1,null,null,null,2]" "[2] " "[123e65]" "[0e+1]" "[0e1]" "[ 4]" "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]\n"
    "[20e1]" "[-0]" "[-123]" "[-1]" "[-0]" "[1E22]" "[1E-2]" "[1E+2]" "[123e45]" "[123.456e78]"
    "[1e-2]" "[1e+2]" "[123]" "[123.456789]" "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}" "{\"asd\":\"sdf\"}"
    "{\"a\":\"b\",\"a\":\"c\"}" "{\"a\":\"b\",\"a\":\"b\"}" "{}" "{\"\":0}" "{\"foo\\u0000bar\": 42}"
    "{ \"min\": -1.0e+28, \"max\": 1.0e+28 }" "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}"
    "{\"a\":[]}" "{\"title\":\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430\" }"
    "{\n\"a\": \"b\"\n}" "[\"\\u0060\\u012a\\u12AB\"]" "[\"\\uD801\\udc37\"]" "[\"\\ud83d\\ude39\\ud83d\\udc8d\"]"
    "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]" "[\"\\\\u0000\"]" "[\"\\\"\"]" "[\"a/*b*/c/*d//e\"]" "[\"\\\\a\"]"
    "[\"\\\\n\"]" "[\"\\u0012\"]" "[\"\\uFFFF\"]" "[\"asd\"]" "[ \"asd\"]" "[\"\\uDBFF\\uDFFF\"]"
    "[\"new\\u00A0line\"]" "[\"\xF4;\"]" "[\"\\u0000\"]" "[\"\\u002c\"]" "[\"\\u005C\"]" "[\"\\uDBFF\\uDFFE\"]"
    "[\"\\u200B\"]" "[\"\\u2064\"]" "[\"\\uFDD0\"]" "[\"\\uFFFE\"]" "[\"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4a\"]"
    "[\"π\"]" "[\"𛿿\"]" "[\"asd \"]" "\" \"" "[\"\\u0061\\u30af\\u30EA\\u30b9\"]" "[\"new\\u000Aline\"]"
    "[\"\x7F;\"]" "[\"\\uA66D\"]" "[\"\\u005C\"]" "[\"⍂㈴⍂\"]" "[\"\\uD834\\uDd1e\"]" "[\"\\u0821\"]"
    "[\"\\u0123\"]" "[\"\\u2028\"]" "[\"\\u2029\"]" "[\"\\u0022\"]" "[\"€𝄞\"]" "[\"aa\"]" "false" "42"
    "-0.1" "null" "\"asd\"" "true" "\"\"" "[\"a\"]\n" "[true]" " [] "))

(define reject
  '("[1 true]" "[a\xE5;]" "[\"\": 1]" "[\"\"]," "[,1]" "[1,,2]" "[\"x\",,]" "[\"x\"]]" "[\"\",]"
    "[\"x\"" "[x" "[3[4]]" "[\xFF;]" "[1:2]" "[,]" "[-]" "[   , \"\"]" "[\"a\",\n4\n,1,"
    "[1,]" "[1,,]" "[\"\va\"\\f]" "[*]" "[\"\"" "[1," "[1,\n1\n,1" "[{}" "[fals]" "[nul]" "[tru]"
    "[++1234]" "[+1]" "[+Inf]" "[-01]" "[-1.0.]" "[-2.]" "[-NaN]" "[.-1]" "[.2e-3]" "[0.1.2]"
    "[0.3e+]" "[0.3e]" "[0.e1]" "[0E+]" "[0E]" "[0e+]" "[0e]" "[1.0e+]" "[1.0e-]" "[1.0e]"
    "[1 000.0]" "[1eE2]" "[2.e+3]" "[2.e-3]" "[2.e3]" "[9.e+]" "[Inf]" "[NaN]" "[\xFF11;]" "[1+2]"
    "[0x1]" "[0x42]" "[Infinity]" "[0e+-1]" "[-123.123foo]" "[123\xE5;]" "[1e1\xE5;]" "[0\xE5;]"
    "[-Infinity]" "[-foo]" "[- 1]" "[-012]" "[-.123]" "[-1x]" "[1ea]" "[1e\xE5;]" "[1.]" "[.123]"
    "[1.2a-3]" "[1.8011670033376514H-308]" "[012]" "[\"x\", truth]" "{[: \"x\"}\n" "{\"x\", null}"
    "{\"x\"::\"b\"}" "{🇨🇭}" "{\"a\":\"a\" 123}" "{key: 'value'}" "{\"\xB9;\":\"0\",}" "{\"a\" b}"
    "{:\"b\"}" "{\"a\" \"b\"}" "{\"a\":" "{\"a\"" "{1:1}" "{9999E9999:1}" "{null:null,null:null}"
    "{\"id\":0,,,,,}" "{'a':0}" "{\"id\":0,}" "{\"a\":\"b\"}/**/" "{\"a\":\"b\"}/**//" "{\"a\":\"b\"}//"
    "{\"a\":\"b\"}/" "{\"a\":\"b\",,\"c\":\"d\"}" "{a: \"b\"}" "{\"a\":\"a" "{ \"foo\" : \"bar\", \"a\" }"
    "{\"a\":\"b\"}#" " " "[\"\\uD800\\\"]" "[\"\\uD800\\u\"]" "[\"\\uD800\\u1\"]" "[\"\\uD800\\u1x\"]"
    "[\xE9;]" "[\"\\x00\"]" "[\"\\\\\\\"]" "[\"\\\t\"]" "[\"\\🌀\"]" "[\"\\\"]" "[\"\\u00A\"]" "[\"\\uD834\\uDd\"]"
    "[\"\\uD800\\uD800\\x\"]" "[\"\\a\"]" "[\"\\uqqqq\"]" "[\\u0020\"asd\"]" "[\\n]" "\"" "['single quote']"
    "abc" "[\"\\" "[\"new\nline\"]" "[\"\t\"]" "\"\\UA66D\"" "\"\"x" "[\x2060;]" "<.>" "[<null>]"
    "[1]x" "[1]]" "[\"asd]" "a\xE5;" "[True]" "1]" "{\"x\": true," "[][]" "]" "{\"a\":" "[\"a\\u00\""
    "[" "{" "[1" "[1," "[\"a\"" "{\"a\":1" "{\"a\":1," "{]" "[}" "[\"a\":1]" "{\"a\",1}"
    "\xFEFF;{}" "[\x0;]" "nul" "tru" "fals" "nulll" "truee" "-" "--1" "1.0e" "1e" "01"))

(test-group "accepted (y_)"
  (for-each
    (lambda (text)
      (test-assert text (guard (e [#t #f]) (string->json text) #t)))
    accept))

(test-group "rejected (n_)"
  (for-each (lambda (text) (test-assert text (rejects? text))) reject))

(test-group "either way, no crash (i_)"
  (for-each
    (lambda (text)
      (test-assert text
        (guard (e [(json-error? e) #t] [#t #f])
          (string->json text)
          #t)))
    '("[123456789012345678901234567890]" "[1.5e+9999]" "[-1e+9999]" "[-123123e100000]"
      "[123123e100000]" "[0.4e00669999999999999999999999999999999999999999999999999999999999999999999999]"
      "[-237462374673276894279832749832423479823246327846]" "[1E-99999]" "[\"\\uDADA\"]"
      "[\"\\uD888\\u1234\"]" "[\"\\uDFAA\"]" "[\"\\uD800\\n\"]" "[\"\\uDd1ea\"]" "[\"\\uD800\\uD800\\n\"]"
      "[1e999999999]")))

(test-group "bytevectors"
  (test-equal "UTF-8" '#("λ") (bytevector->json (string->utf8 "[\"λ\"]")))
  (test-equal "byte order mark" '() (bytevector->json (bytevector #xEF #xBB #xBF #x7B #x7D)))
  (test-equal "malformed UTF-8 is U+FFFD" '#("\xFFFD;")
    (bytevector->json (bytevector #x5B #x22 #xFF #x22 #x5D)))
  (test-equal "json->bytevector" (string->utf8 "[\"λ\"]") (json->bytevector '#("λ"))))

(test-group "ports"
  (let ([p (open-string-input-port "{\"a\":1} [2]\n\"three\" 4 ")])
    (test-equal "first value" '((a . 1)) (json-read p))
    (test-equal "second value" '#(2) (json-read p))
    (test-equal "third value" "three" (json-read p))
    (test-equal "fourth value" 4 (json-read p))
    (test-assert "then eof" (eof-object? (json-read p))))
  (let ([p (open-string-input-port "[1]rest")])
    (json-read p)
    (test-equal "nothing past the value is consumed" "rest" (get-string-all p)))
  (let ([p (open-string-input-port "12,")])
    (json-read p)
    (test-equal "nothing past a number is consumed" #\, (get-char p)))
  (test-equal "json-read-all (JSON Lines)" '(((a . 1)) ((a . 2)) #(3))
    (json-read-all (open-string-input-port "{\"a\":1}\n{\"a\":2}\n[3]\n")))
  (test-equal "binary port" '((k . "λ😀é"))
    (json-read (open-bytevector-input-port (string->utf8 "{\"k\":\"λ😀\\u00e9\"}"))))
  (let ([p (open-bytevector-input-port (string->utf8 "\"λ\" 2 \"😀\""))])
    (test-equal "binary port, several values" '("λ" 2 "😀") (json-read-all p)))
  (test-equal "binary port, malformed UTF-8" "a\xFFFD;b\xFFFD;"
    (json-read (open-bytevector-input-port (bytevector #x22 #x61 #xC3 #x62 #xE2 #x82 #x22))))
  (test-equal "binary port, overlong encoding" "\xFFFD;"
    (json-read (open-bytevector-input-port (bytevector #x22 #xC0 #xAF #x22))))
  (test-equal "port error position" '(2 4)
    (error-position (lambda () (json-read (open-string-input-port "[1,\n 2 x]")))))
  (test-equal "binary port error position" '(1 6)
    (error-position (lambda () (json-read (open-bytevector-input-port (string->utf8 "[\"λ\" x]"))))))
  (test-equal "port string with escapes" "a\nb\x1F600;"
    (json-read (open-string-input-port "\"a\\nb\\ud83d\\ude00\"")))
  (test-equal "port long string" (make-string 1000 #\x)
    (json-read (open-string-input-port (string-append "\"" (make-string 1000 #\x) "\""))))
  (test-equal "port rejects what strings reject"
    (map rejects? reject)
    (map (lambda (text)
           (guard (e [(json-error? e) #t] [#t #f])
             (let ([p (open-string-input-port text)])
               (let ([v (json-read p)])
                 ;; a whole text must be one value
                 (when (or (eof-object? v) (not (eof-object? (json-read p))))
                   (raise (guard (c [#t c]) (string->json "")))))
               #f)))
      reject))
  (let-values ([(out get) (open-bytevector-output-port)])
    (json-write '#("λ") out)
    (test-equal "write to a binary port" (string->utf8 "[\"λ\"]") (get)))
  (test-equal "write to a textual port" "{\"a\":true}"
    (call-with-string-output-port (lambda (out) (json-write '((a . #t)) out)))))

(test-group "writing"
  (test-equal "compact" "{\"a\":[1,2.5,null,true,false],\"b\":{}}"
    (json->string '((a . #(1 2.5 null #t #f)) (b . ()))))
  (test-equal "string keys" "{\"a b\":1}" (json->string '(("a b" . 1))))
  (test-equal "empty array" "[]" (json->string '#()))
  (test-equal "list as an array" "[1,2,[3]]" (json->string '(1 2 (3))))
  (test-equal "symbols as strings" "[\"ok\",null]" (json->string '#(ok null)))
  (test-equal "string escapes" "\"\\\"\\\\\\n\\r\\t\\b\\f\\u0001/\""
    (json->string (string #\" #\\ #\newline #\return #\tab #\backspace #\xC #\x1 #\/)))
  (test-equal "non-ASCII is written as is" "\"λ😀\"" (json->string "λ😀"))
  (test-equal "ascii?" "\"\\u03bb\\ud83d\\ude00\"" (json->string "λ😀" #:ascii? #t))
  (test-equal "html-safe?" "\"\\u003c/script\\u003e \\u0026 \\u0027\\u2028\""
    (json->string "</script> & '\x2028;" #:html-safe? #t))
  (test-equal "hashtable keys are sorted" "{\"a\":1,\"b\":2,\"c\":3}"
    (let ([t (make-eq-hashtable)])
      (hashtable-set! t 'c 3) (hashtable-set! t 'a 1) (hashtable-set! t 'b 2)
      (json->string t)))
  (test-equal "sort-keys?" "{\"a\":1,\"b\":{\"x\":1,\"y\":2}}"
    (json->string '((b . ((y . 2) (x . 1))) (a . 1)) #:sort-keys? #t))
  (test-equal "custom null" "[null,false]" (json->string (vector 'nothing #f) #:null 'nothing))
  (test-equal "null parameter" "[null]" (parameterize ([json-null 'none]) (json->string '#(none)))))

(test-group "writing numbers"
  (test-equal "integer" "42" (json->string 42))
  (test-equal "bignum" "123456789012345678901234567890" (json->string 123456789012345678901234567890))
  (test-equal "negative" "-3" (json->string -3))
  (test-equal "positive zero" "0.0" (json->string 0.0))
  (test-equal "negative zero" "-0.0" (json->string -0.0))
  (test-equal "flonum" "2.5" (json->string 2.5))
  (test-equal "integral flonum" "100.0" (json->string 100.0))
  (test-equal "exponent" "1e21" (json->string 1e21))
  (test-equal "small" "5e-324" (json->string 5e-324))
  (test-equal "rational" "0.5" (json->string 1/2))
  (test-equal "shortest round trip" "0.1" (json->string 0.1))
  (for-each
    (lambda (x)
      (test-equal (string-append "round trip " (number->string x)) x
        (string->json (json->string x))))
    (list 0.1 1e21 1.7976931348623157e308 5e-324 -2.5e-10 123456789.125 (/ 1. 3)))
  (test-assert "NaN" (error-message (lambda () (json->string +nan.0))))
  (test-assert "infinity" (error-message (lambda () (json->string (vector +inf.0)))))
  (test-assert "complex" (error-message (lambda () (json->string 1+2i)))))

(test-group "write errors"
  (test-assert "a procedure"
    (contains? (error-message (lambda () (json->string (vector car)))) "no JSON form"))
  (test-assert "a bad key" (error-message (lambda () (json->string '((a . 1) (2 . 3))))))
  (test-equal "write errors have no position" '(#f #f)
    (error-position (lambda () (json->string (vector car)))))
  (test-assert "a cyclic value"
    (let ([v (vector 1)])
      (vector-set! v 0 v)
      (contains? (error-message (lambda () (json->string v))) "cyclic"))))

(test-group "pretty printing"
  (test-equal "indent 2"
    "{\n  \"a\": [\n    1,\n    2\n  ],\n  \"b\": {},\n  \"c\": [],\n  \"d\": {\n    \"e\": null\n  }\n}"
    (json->string '((a . #(1 2)) (b . ()) (c . #()) (d . ((e . null)))) #:indent 2))
  (test-equal "indent string" "[\n\t1\n]" (json->string '#(1) #:indent "\t"))
  (test-equal "indent 0" "[\n1,\n2\n]" (json->string '#(1 2) #:indent 0))
  (test-equal "scalar" "1" (json->string 1 #:indent 2))
  (test-equal "pretty output reads back" '((a . #(1 ((b . "c")))))
    (string->json (json->string '((a . #(1 ((b . "c"))))) #:indent 4))))

(test-group "first-class use"
  (test-equal "map string->json" '(1 #(2)) (map string->json '("1" "[2]")))
  (test-equal "map json->string" '("[1]" "{\"a\":2}") (map json->string '(#(1) ((a . 2)))))
  (test-equal "map json-read" '(3) (map json-read (list (open-string-input-port "3")))))

(test-group "round trips"
  (for-each
    (lambda (text) (test-equal text text (round-trip text)))
    '("{\"a\":[1,2,{\"b\":null}],\"c\":\"\\u0000\\\"\"}" "[]" "{}" "[0.5,-0.0,1e-7,12345678901234567890]"
      "\"\\u001f\"" "[true,false,null]")))

(test-group "json-ref"
  (define doc (string->json "{\"a\":{\"b\":[10,{\"c\":\"deep\"}]},\"n\":null}"))
  (test-equal "key" 'null (json-ref doc 'n))
  (test-equal "string key" 'null (json-ref doc "n"))
  (test-equal "missing" #f (json-ref doc 'z))
  (test-equal "default" 0 (json-ref doc 'z 0))
  (test-equal "path" "deep" (json-ref doc '(a b 1 c)))
  (test-equal "path with string keys" 10 (json-ref doc '("a" "b" 0)))
  (test-equal "path out of range" 'none (json-ref doc '(a b 7) 'none))
  (test-equal "not an object" #f (json-ref 5 'a))
  (test-equal "index into a vector" 20 (json-ref '#(10 20) 1))
  (test-equal "empty path" doc (json-ref doc '()))
  (let ([t (string->json "{\"a\":1}" #:object 'hashtable #:key 'string)])
    (test-equal "string-keyed hashtable, symbol key" 1 (json-ref t 'a)))
  (let ([t (string->json "{\"a\":1}" #:object 'hashtable)])
    (test-equal "symbol-keyed hashtable, string key" 1 (json-ref t "a")))
  (test-assert "json-null?" (json-null? (json-ref doc 'n)))
  (test-assert "json-object?" (json-object? doc))
  (test-assert "empty object" (json-object? '()))
  (test-assert "json-array?" (json-array? '#()))
  (test-assert "a vector is not an object" (not (json-object? '#()))))

;; The examples of RFC 6901, section 5.
(test-group "JSON Pointer"
  (define doc
    (string->json
      "{\"foo\": [\"bar\", \"baz\"], \"\": 0, \"a/b\": 1, \"c%d\": 2, \"e^f\": 3, \"g|h\": 4,
        \"i\\\\j\": 5, \"k\\\"l\": 6, \" \": 7, \"m~n\": 8}"))
  (test-equal "\"\"" doc (json-pointer-ref doc ""))
  (test-equal "/foo" '#("bar" "baz") (json-pointer-ref doc "/foo"))
  (test-equal "/foo/0" "bar" (json-pointer-ref doc "/foo/0"))
  (test-equal "/" 0 (json-pointer-ref doc "/"))
  (test-equal "/a~1b" 1 (json-pointer-ref doc "/a~1b"))
  (test-equal "/c%d" 2 (json-pointer-ref doc "/c%d"))
  (test-equal "/e^f" 3 (json-pointer-ref doc "/e^f"))
  (test-equal "/g|h" 4 (json-pointer-ref doc "/g|h"))
  (test-equal "/i\\j" 5 (json-pointer-ref doc "/i\\j"))
  (test-equal "/k\"l" 6 (json-pointer-ref doc "/k\"l"))
  (test-equal "/ " 7 (json-pointer-ref doc "/ "))
  (test-equal "/m~0n" 8 (json-pointer-ref doc "/m~0n"))
  (test-equal "token list" "baz" (json-pointer-ref doc '("foo" 1)))
  (test-equal "missing with a default" 'none (json-pointer-ref doc "/foo/2" 'none))
  (test-equal "\"-\" is past the end" 'none (json-pointer-ref doc "/foo/-" 'none))
  (test-equal "leading zero is not an index" 'none (json-pointer-ref doc "/foo/01" 'none))
  (test-equal "into a scalar" 'none (json-pointer-ref doc "/a~1b/x" 'none))
  (test-assert "missing without a default"
    (contains? (error-message (lambda () (json-pointer-ref doc "/foo/9"))) "no value at /foo/9"))
  (test-equal "parse" '("a/b" "m~n" "" "0") (string->json-pointer "/a~1b/m~0n//0"))
  (test-equal "parse empty" '() (string->json-pointer ""))
  (test-assert "parse without a slash" (error-message (lambda () (string->json-pointer "a"))))
  (test-assert "parse a bad escape" (error-message (lambda () (string->json-pointer "/~2"))))
  (test-equal "~01 is ~1, not /" '("~1") (string->json-pointer "/~01"))
  (test-equal "format" "/a~1b/m~0n/3/sym" (json-pointer->string '("a/b" "m~n" 3 sym)))
  (test-equal "format empty" "" (json-pointer->string '()))
  (test-equal "hashtable document" 2
    (json-pointer-ref (string->json "{\"a\":[1,2]}" #:object 'hashtable) "/a/1")))

(test-end)
