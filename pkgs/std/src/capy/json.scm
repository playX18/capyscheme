;;; JSON (RFC 8259): reading and writing from ports, strings and bytevectors, pretty printing,
;;; and JSON Pointer (RFC 6901).
;;;
;;;   (string->json "{\"a\": [1, 2.5, null]}")   => ((a . #(1 2.5 null)))
;;;   (json->string '((a . #(1 2.5 null))))      => "{\"a\":[1,2.5,null]}"
;;;   (json->string '((a . 1) (b . #())) #:indent 2)
;;;     => "{\n  \"a\": 1,\n  \"b\": []\n}"
;;;   (json-read port)                 ; the next value of PORT, or the eof object
;;;   (json-write value port)
;;;   (json-ref doc '(items 0 name))   ; keys and indexes, #f when absent
;;;   (json-pointer-ref doc "/items/0/name")
;;;
;;; The data model, by default:
;;;
;;;   object      an alist with symbol keys, in the order of the text; `()` is the empty object
;;;   array       a vector
;;;   string      a string
;;;   number      an exact integer when the text has no fraction and no exponent, otherwise a
;;;               flonum (`1` => 1, `1.0` => 1.0, `1e2` => 100.0)
;;;   true/false  #t / #f
;;;   null        the symbol `null`
;;;
;;; The reader can be told to build something else, per call with keywords or for a dynamic
;;; extent with the parameters of the same name:
;;;
;;;   #:object  'alist | 'hashtable | a procedure of the alist        (json-object-type)
;;;   #:array   'vector | 'list | a procedure of the list             (json-array-type)
;;;   #:key     'symbol | 'string | a procedure of the key string     (json-key-type)
;;;   #:null    the value of `null`                                   (json-null)
;;;   #:max-depth  how deeply arrays and objects may nest, 512        (json-max-depth)
;;;
;;;   (string->json "{\"a\":null}" #:object 'hashtable #:key 'string #:null #f)
;;;   (parameterize ([json-array-type 'list]) (string->json "[1,[2]]"))  => (1 (2))
;;;
;;; The writer takes any of these forms: alists with symbol or string keys, hashtables,
;;; vectors, proper lists that are not alists (as arrays), strings, symbols other than `null`
;;; (as strings), exact integers, exact rationals and finite flonums (as flonums), #t, #f, and
;;; `null` or the value of `#:null`. `()` is written as `{}`; write `#()` for an empty array.
;;;
;;; Reading is strict RFC 8259: no comments, trailing commas, single quotes, leading zeros, NaN
;;; or leading `+`. Whitespace may surround the value. A malformed text raises a `&json-error`
;;; whose message says what was expected and what was found, with the line and column (both
;;; 1-based, columns count characters); `json-error-line`, `json-error-column` and
;;; `json-error-offset` (0-based, in characters) read them back. Writing a value that has no
;;; JSON form also raises a `&json-error`, with no position.
;;;
;;; Escapes `😀` of a surrogate pair become one character; a lone surrogate, which
;;; no Scheme string can hold, becomes U+FFFD, as does malformed UTF-8 in a bytevector or a
;;; binary port. Duplicate keys are all kept in an alist (`json-ref` finds the first); in a
;;; hashtable the last one wins.

(library (capy json)
  (export
    ;; reading
    json-read
    json-read-all
    string->json
    bytevector->json
    ;; writing
    json-write
    json->string
    json->bytevector
    ;; options
    json-null
    json-object-type
    json-array-type
    json-key-type
    json-max-depth
    ;; values
    json-null?
    json-object?
    json-array?
    json-ref
    ;; JSON Pointer
    json-pointer-ref
    string->json-pointer
    json-pointer->string
    ;; errors
    &json-error
    json-error?
    json-error-line
    json-error-column
    json-error-offset)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (only (core parameters) make-parameter))

  ;; ---- Options ----

  ;; The value the reader returns for `null`; the writer writes it (and the symbol `null`) as
  ;; `null`.
  (define json-null (make-parameter 'null))

  ;; What the reader builds for an object: 'alist (the default), 'hashtable, or a procedure
  ;; that is given the alist of the members in the order of the text.
  (define json-object-type (make-parameter 'alist))

  ;; What the reader builds for an array: 'vector (the default), 'list, or a procedure that is
  ;; given the list of the elements.
  (define json-array-type (make-parameter 'vector))

  ;; What the reader makes of an object key: 'symbol (the default), 'string, or a procedure that
  ;; is given the key string.
  (define json-key-type (make-parameter 'symbol))

  ;; How deeply arrays and objects may nest, for the reader and the writer. A text nested
  ;; deeper is rejected rather than run the stack out; a value nested deeper (a cyclic one, say)
  ;; is not written.
  (define json-max-depth (make-parameter 512))

  ;; ---- Errors ----

  (define-condition-type &json-error &error
    make-json-error json-error?
    (line json-error-line)
    (column json-error-column)
    (offset json-error-offset))

  (define (raise-json-error who message line column offset irritants)
    (raise
      (condition
        (make-json-error line column offset)
        (make-who-condition who)
        (make-message-condition
          (if line
            (string-append message " at line " (number->string line)
              ", column " (number->string column))
            message))
        (make-irritants-condition irritants))))

  (define (write-error who message . irritants)
    (raise-json-error who message #f #f #f irritants))

  ;; How a character the reader did not expect is shown in a message.
  (define (describe c)
    (cond
      [(eof-object? c) "the end of the input"]
      [(char<? c #\space) (string-append "control character U+" (hex4-string (char->integer c)))]
      [(char=? c #\') "\"'\" (JSON strings are written with double quotes)"]
      [(char=? c #\/) "'/' (JSON has no comments)"]
      [else (string-append "'" (string c) "'")]))

  (define (hex4-string n)
    (let ([s (number->string n 16)])
      (string-append (make-string (max 0 (- 4 (string-length s))) #\0) (string-upcase s))))

  ;; ---- Reader options ----

  (define (object-builder object key who)
    (cond
      [(eq? object 'alist) (lambda (members) members)]
      [(eq? object 'hashtable)
        (lambda (members)
          (let ([table (cond
                         [(eq? key 'symbol) (make-eq-hashtable)]
                         [(eq? key 'string) (make-hashtable string-hash string=?)]
                         [else (make-hashtable equal-hash equal?)])])
            (for-each (lambda (m) (hashtable-set! table (car m) (cdr m))) members)
            table))]
      [(procedure? object) object]
      [else (assertion-violation who "#:object must be 'alist, 'hashtable or a procedure" object)]))

  (define (array-builder array who)
    (cond
      [(eq? array 'vector) list->vector]
      [(eq? array 'list) (lambda (elements) elements)]
      [(procedure? array) array]
      [else (assertion-violation who "#:array must be 'vector, 'list or a procedure" array)]))

  (define (key-builder key who)
    (cond
      [(eq? key 'symbol) string->symbol]
      [(eq? key 'string) (lambda (s) s)]
      [(procedure? key) key]
      [else (assertion-violation who "#:key must be 'symbol, 'string or a procedure" key)]))

  ;; ---- The parser ----
  ;;
  ;; One parser, specialised for each kind of source by `json-parser`, which is handed the
  ;; operations of the source:
  ;;
  ;;   (peek)          the next character, or the eof object; not consumed
  ;;   (advance!)      consumes the character that `peek` returned
  ;;   (take-run)      consumes and returns the characters of a string up to the next '"', '\',
  ;;                   control character or the end of the input
  ;;   (take-number)   consumes and returns the longest run of characters that can be part of
  ;;                   a number
  ;;   (here)          the current position, in a form `locate` understands
  ;;   (locate POS)    (values LINE COLUMN OFFSET) of a position
  ;;
  ;; It evaluates to a procedure of no arguments that reads one value, or returns the eof
  ;; object when only whitespace is left.

  (define-syntax json-parser
    (syntax-rules ()
      [(_ who make-object make-array make-key null max-depth
         peek advance! take-run take-number here locate)
        (let ()
          (define (fail-at pos message . irritants)
            (let-values ([(line column offset) (locate pos)])
              (raise-json-error who message line column offset irritants)))

          (define (expected what)
            (fail-at (here) (string-append "expected " what ", found " (describe (peek)))))

          (define (whitespace? c)
            (or (char=? c #\space) (char=? c #\newline) (char=? c #\return) (char=? c #\tab)))

          (define (skip-whitespace!)
            (let ([c (peek)])
              (when (and (char? c) (whitespace? c))
                (advance!)
                (skip-whitespace!))))

          (define (digit? c) (and (char? c) (char<=? #\0 c #\9)))

          (define (read-value depth)
            (skip-whitespace!)
            (let ([c (peek)])
              (cond
                [(eof-object? c) (expected "a JSON value")]
                [(char=? c #\") (advance!) (read-string)]
                [(char=? c #\{) (enter depth) (advance!) (read-object (+ depth 1))]
                [(char=? c #\[) (enter depth) (advance!) (read-array (+ depth 1))]
                [(char=? c #\t) (read-literal "true" #t)]
                [(char=? c #\f) (read-literal "false" #f)]
                [(char=? c #\n) (read-literal "null" null)]
                [(or (char=? c #\-) (digit? c)) (read-number)]
                [else (expected "a JSON value")])))

          (define (enter depth)
            (when (>= depth max-depth)
              (fail-at (here)
                (string-append "arrays and objects nest deeper than "
                  (number->string max-depth) " levels"))))

          (define (read-literal word value)
            (let ([start (here)] [n (string-length word)])
              (let loop ([i 0])
                (cond
                  [(= i n) value]
                  [(eqv? (peek) (string-ref word i)) (advance!) (loop (+ i 1))]
                  [else
                    (fail-at start
                      (string-append "invalid literal, expected '" word "'"))]))))

          ;; The number that TEXT, a run of number characters, is, or #f when it is not
          ;; -?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?. An integer is computed as the
          ;; digits are checked; anything with a fraction or an exponent is checked, then left
          ;; to `string->number`, which rounds correctly.
          (define (parse-number text)
            (let* ([n (string-length text)]
                   [negative? (and (fx>? n 0) (char=? (string-ref text 0) #\-))]
                   [start (if negative? 1 0)])
              (and (fx<? start n)
                (let ([c (string-ref text start)])
                  (cond
                    [(char=? c #\0)
                      (if (fx=? (fx+ start 1) n) 0 (parse-fraction text (fx+ start 1) n))]
                    [(char<=? #\1 c #\9)
                      (let loop ([i (fx+ start 1)] [value (fx- (char->integer c) 48)])
                        (if (fx=? i n)
                          (if negative? (- value) value)
                          (let ([d (string-ref text i)])
                            (if (char<=? #\0 d #\9)
                              (loop (fx+ i 1) (+ (* value 10) (fx- (char->integer d) 48)))
                              (parse-fraction text i n)))))]
                    [else #f])))))

          ;; TEXT from I, after the integer part: (\.[0-9]+)?([eE][+-]?[0-9]+)?
          (define (parse-fraction text i n)
            (define (digits i) (if (and (fx<? i n) (char<=? #\0 (string-ref text i) #\9)) (digits (fx+ i 1)) i))
            (let* ([i (if (char=? (string-ref text i) #\.)
                        (let ([j (digits (fx+ i 1))]) (and (fx>? j (fx+ i 1)) j))
                        i)]
                   [i (and i
                        (if (and (fx<? i n) (memv (string-ref text i) '(#\e #\E)))
                          (let* ([j (fx+ i 1)]
                                 [j (if (and (fx<? j n) (memv (string-ref text j) '(#\+ #\-))) (fx+ j 1) j)]
                                 [k (digits j)])
                            (and (fx>? k j) k))
                          i))])
              (and (eqv? i n) (inexact (string->number text 10)))))

          (define (read-number)
            (let* ([start (here)] [text (take-number)])
              (or (parse-number text)
                (fail-at start (string-append "invalid number '" text "'")))))

          (define (read-string)
            (let ([text (take-run)])
              (if (eqv? (peek) #\")
                (begin (advance!) text)
                (read-string-pieces (list text)))))

          ;; PIECES are the parts of the string so far, the last first; the next character is
          ;; a '\', a control character or the end of the input.
          (define (read-string-pieces pieces)
            (let* ([c (peek)]
                   [pieces (cond
                             [(eof-object? c) (fail-at (here) "unterminated string")]
                             [(char=? c #\\) (advance!) (cons (read-escape) pieces)]
                             [else
                               (fail-at (here)
                                 (string-append (describe c)
                                   " in a string (control characters must be escaped)"))])])
              (let ([text (take-run)])
                (if (eqv? (peek) #\")
                  (begin (advance!) (apply string-append (reverse (cons text pieces))))
                  (read-string-pieces (cons text pieces))))))

          ;; The text of the escape after a consumed '\'.
          (define (read-escape)
            (let ([start (here)] [e (peek)])
              (if (eof-object? e)
                (fail-at start "unterminated string")
                (begin
                  (advance!)
                  (case e
                    [(#\" #\\ #\/) (string e)]
                    [(#\b) (string #\backspace)]
                    [(#\f) (string #\xC)]
                    [(#\n) (string #\newline)]
                    [(#\r) (string #\return)]
                    [(#\t) (string #\tab)]
                    [(#\u) (code-point->string (read-hex4))]
                    [else
                      (fail-at start
                        (string-append "invalid escape '\\" (string e)
                          "' (expected one of \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\u)"))])))))

          (define (read-hex4)
            (let loop ([i 0] [value 0])
              (if (= i 4)
                value
                (let* ([c (peek)]
                       [d (and (char? c) (hex-digit-value c))])
                  (unless d
                    (fail-at (here)
                      (string-append "expected a hex digit in a \\u escape, found " (describe c))))
                  (advance!)
                  (loop (+ i 1) (+ (* value 16) d))))))

          ;; The text of a \u escape of CODE; a high surrogate takes the low one of the escape
          ;; that follows it, if there is one.
          (define (code-point->string code)
            (cond
              [(<= #xDC00 code #xDFFF) "\xFFFD;"]
              [(<= #xD800 code #xDBFF)
                (if (eqv? (peek) #\\)
                  (begin
                    (advance!)
                    (if (eqv? (peek) #\u)
                      (begin
                        (advance!)
                        (let ([low (read-hex4)])
                          (if (<= #xDC00 low #xDFFF)
                            (string (integer->char
                                      (+ #x10000 (* (- code #xD800) #x400) (- low #xDC00))))
                            (string-append "\xFFFD;" (code-point->string low)))))
                      (string-append "\xFFFD;" (read-escape))))
                  "\xFFFD;")]
              [else (string (integer->char code))]))

          (define (read-array depth)
            (skip-whitespace!)
            (if (eqv? (peek) #\])
              (begin (advance!) (make-array '()))
              (let loop ([elements (list (read-value depth))])
                (skip-whitespace!)
                (let ([c (peek)])
                  (cond
                    [(eqv? c #\,)
                      (advance!)
                      (skip-whitespace!)
                      (when (eqv? (peek) #\])
                        (fail-at (here) "expected a JSON value after ',', found ']' (JSON has no trailing commas)"))
                      (loop (cons (read-value depth) elements))]
                    [(eqv? c #\]) (advance!) (make-array (reverse elements))]
                    [else (expected "',' or ']' after an array element")])))))

          (define (read-member depth)
            (skip-whitespace!)
            (unless (eqv? (peek) #\") (expected "a string key"))
            (advance!)
            (let ([key (make-key (read-string))])
              (skip-whitespace!)
              (unless (eqv? (peek) #\:) (expected "':' after an object key"))
              (advance!)
              (cons key (read-value depth))))

          (define (read-object depth)
            (skip-whitespace!)
            (let ([c (peek)])
              (cond
                [(eqv? c #\}) (advance!) (make-object '())]
                [(eqv? c #\")
                  (let loop ([members (list (read-member depth))])
                    (skip-whitespace!)
                    (let ([c (peek)])
                      (cond
                        [(eqv? c #\,)
                          (advance!)
                          (skip-whitespace!)
                          (when (eqv? (peek) #\})
                            (fail-at (here) "expected a string key after ',', found '}' (JSON has no trailing commas)"))
                          (loop (cons (read-member depth) members))]
                        [(eqv? c #\}) (advance!) (make-object (reverse members))]
                        [else (expected "',' or '}' after an object member")])))]
                [else (expected "a string key or '}'")])))

          (lambda ()
            (skip-whitespace!)
            (if (eof-object? (peek))
              (peek)
              (read-value 0))))]))

  (define (hex-digit-value c)
    (cond
      [(char<=? #\0 c #\9) (- (char->integer c) 48)]
      [(char<=? #\a c #\f) (- (char->integer c) 87)]
      [(char<=? #\A c #\F) (- (char->integer c) 55)]
      [else #f]))

  (define (number-char? c)
    (or (char<=? #\0 c #\9) (char=? c #\-) (char=? c #\+) (char=? c #\.) (char=? c #\e)
      (char=? c #\E)))

  (define (plain-string-char? c)
    (not (or (char=? c #\") (char=? c #\\) (char<? c #\space))))

  ;; A parser of the string S from index START; also returns a thunk of the current index.
  (define (string-parser s start who make-object make-array make-key null max-depth)
    (let ([n (string-length s)] [i start])
      (define-syntax peek
        (syntax-rules () [(_) (if (fx<? i n) (string-ref s i) (eof-object))]))
      (define-syntax advance!
        (syntax-rules () [(_) (set! i (fx+ i 1))]))
      (define (take-run)
        (let ([from i])
          (let scan ([j i])
            (if (fx<? j n)
              (let ([c (string-ref s j)])
                (if (plain-string-char? c)
                  (scan (fx+ j 1))
                  (begin (set! i j) (substring s from j))))
              (begin (set! i j) (substring s from j))))))
      (define (take-number)
        (let ([from i])
          (let scan ([j i])
            (if (and (fx<? j n) (number-char? (string-ref s j)))
              (scan (fx+ j 1))
              (begin (set! i j) (substring s from j))))))
      (define (here) i)
      (define (locate pos)
        (let loop ([k 0] [line 1] [column 1])
          (cond
            [(= k pos) (values line column pos)]
            [(char=? (string-ref s k) #\newline) (loop (+ k 1) (+ line 1) 1)]
            [else (loop (+ k 1) line (+ column 1))])))
      (values
        (json-parser who make-object make-array make-key null max-depth
          peek advance! take-run take-number here locate)
        (lambda () i))))

  ;; The parser of a port that reads characters with PEEK-CHAR and ADVANCE-CHAR!, and keeps
  ;; the line, the column and the offset itself. Strings and numbers are gathered in a buffer.
  (define-syntax port-parser
    (syntax-rules ()
      [(_ who make-object make-array make-key null max-depth peek-char advance-char!)
        (let ([line 1] [column 1] [offset 0] [buffer (make-string 64)] [fill 0])
          (define-syntax peek (syntax-rules () [(_) (peek-char)]))
          (define (advance!)
            (let ([c (peek-char)])
              (advance-char!)
              (set! offset (+ offset 1))
              (if (eqv? c #\newline)
                (begin (set! line (+ line 1)) (set! column 1))
                (set! column (+ column 1)))))
          (define (push! c)
            (when (= fill (string-length buffer))
              (let ([bigger (make-string (* 2 fill))])
                (do ([k 0 (+ k 1)]) ((= k fill)) (string-set! bigger k (string-ref buffer k)))
                (set! buffer bigger)))
            (string-set! buffer fill c)
            (set! fill (+ fill 1)))
          (define (take-buffer)
            (let ([text (substring buffer 0 fill)])
              (set! offset (+ offset fill))
              (set! column (+ column fill))
              (set! fill 0)
              text))
          (define (take-run)
            (let loop ()
              (let ([c (peek-char)])
                (if (and (char? c) (plain-string-char? c))
                  (begin (advance-char!) (push! c) (loop))
                  (take-buffer)))))
          (define (take-number)
            (let loop ()
              (let ([c (peek-char)])
                (if (and (char? c) (number-char? c))
                  (begin (advance-char!) (push! c) (loop))
                  (take-buffer)))))
          (define (here) (vector line column offset))
          (define (locate pos) (values (vector-ref pos 0) (vector-ref pos 1) (vector-ref pos 2)))
          (json-parser who make-object make-array make-key null max-depth
            peek advance! take-run take-number here locate))]))

  (define (textual-port-parser port who make-object make-array make-key null max-depth)
    (define-syntax peek-char (syntax-rules () [(_) (lookahead-char port)]))
    (define-syntax advance-char! (syntax-rules () [(_) (get-char port)]))
    (port-parser who make-object make-array make-key null max-depth peek-char advance-char!))

  ;; A binary port is decoded as UTF-8 here. An ASCII byte is looked at without consuming it;
  ;; a longer sequence is consumed and its character kept until it is advanced over, which
  ;; only happens inside strings and in malformed input.
  (define (binary-port-parser port who make-object make-array make-key null max-depth)
    (define pending #f)
    (define (continuation)
      (let ([b (lookahead-u8 port)])
        (and (not (eof-object? b)) (fx=? (fxand b #xC0) #x80)
          (begin (get-u8 port) (fxand b #x3F)))))
    (define (decode lead)
      (let-values ([(count initial minimum)
                    (cond
                      [(fx=? (fxand lead #xE0) #xC0) (values 1 (fxand lead #x1F) #x80)]
                      [(fx=? (fxand lead #xF0) #xE0) (values 2 (fxand lead #x0F) #x800)]
                      [(fx=? (fxand lead #xF8) #xF0) (values 3 (fxand lead #x07) #x10000)]
                      [else (values 0 #f 0)])])
        (if (not initial)
          #\xFFFD
          (let loop ([k count] [code initial])
            (if (fx=? k 0)
              (if (or (< code minimum) (> code #x10FFFF) (<= #xD800 code #xDFFF))
                #\xFFFD
                (integer->char code))
              (let ([b (continuation)])
                (if b (loop (fx- k 1) (fxior (fxarithmetic-shift-left code 6) b)) #\xFFFD)))))))
    (define (peek-char)
      (or pending
        (let ([b (lookahead-u8 port)])
          (cond
            [(eof-object? b) b]
            [(fx<? b #x80) (integer->char b)]
            [else (get-u8 port) (set! pending (decode b)) pending]))))
    (define (advance-char!)
      (if pending (set! pending #f) (get-u8 port)))
    (port-parser who make-object make-array make-key null max-depth peek-char advance-char!))

  (define (port-reader port who object array key null max-depth)
    (let ([make-object (object-builder object key who)]
          [make-array (array-builder array who)]
          [make-key (key-builder key who)])
      (cond
        [(and (input-port? port) (textual-port? port))
          (textual-port-parser port who make-object make-array make-key null max-depth)]
        [(and (input-port? port) (binary-port? port))
          (binary-port-parser port who make-object make-array make-key null max-depth)]
        [else (assertion-violation who "not an input port" port)])))

  ;; ---- Reading ----

  ;; The next JSON value of PORT (a textual or a binary input port, read as UTF-8), or the eof
  ;; object when only whitespace is left. Only the characters of the value are consumed (and,
  ;; after a number, nothing past it), so a port with several values one after the other, as
  ;; in JSON Lines, is read by calling `json-read` until it returns the eof object.
  (define (json-read [port (current-input-port)]
            #:object [object (json-object-type)]
            #:array [array (json-array-type)]
            #:key [key (json-key-type)]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    ((port-reader port 'json-read object array key null max-depth)))

  ;; The list of all the JSON values of PORT, one after the other (JSON Lines, or values only
  ;; separated by whitespace).
  (define (json-read-all [port (current-input-port)]
            #:object [object (json-object-type)]
            #:array [array (json-array-type)]
            #:key [key (json-key-type)]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (let ([next (port-reader port 'json-read-all object array key null max-depth)])
      (let loop ([values '()])
        (let ([v (next)])
          (if (eof-object? v)
            (reverse values)
            (loop (cons v values)))))))

  (define (parse-text text who object array key null max-depth)
    (let-values ([(next index)
                  (string-parser text 0 who
                    (object-builder object key who) (array-builder array who)
                    (key-builder key who) null max-depth)])
      (let ([v (next)])
        (when (eof-object? v)
          (raise-json-error who "expected a JSON value, found the end of the input"
            1 1 0 '()))
        ;; only whitespace may follow
        (let ([n (string-length text)])
          (let loop ([i (index)])
            (cond
              [(= i n) v]
              [(memv (string-ref text i) '(#\space #\newline #\return #\tab)) (loop (+ i 1))]
              [else
                (let-values ([(line column offset)
                              (let count ([k 0] [line 1] [column 1])
                                (cond
                                  [(= k i) (values line column i)]
                                  [(char=? (string-ref text k) #\newline)
                                    (count (+ k 1) (+ line 1) 1)]
                                  [else (count (+ k 1) line (+ column 1))]))])
                  (raise-json-error who
                    (string-append "unexpected " (describe (string-ref text i))
                      " after the JSON value")
                    line column offset '()))]))))))

  ;; The JSON value that the string TEXT holds; anything but whitespace around it is an error.
  (define (string->json text
            #:object [object (json-object-type)]
            #:array [array (json-array-type)]
            #:key [key (json-key-type)]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (unless (string? text) (assertion-violation 'string->json "not a string" text))
    (parse-text text 'string->json object array key null max-depth))

  ;; The text of the UTF-8 BYTES, with U+FFFD for each malformed sequence (`utf8->string`
  ;; rejects them).
  (define (decode-utf8-leniently bytes)
    (let ([n (bytevector-length bytes)])
      (define (continuation i)
        (and (< i n)
          (let ([b (bytevector-u8-ref bytes i)])
            (and (fx=? (fxand b #xC0) #x80) (fxand b #x3F)))))
      (call-with-string-output-port
        (lambda (out)
          (let loop ([i 0])
            (when (< i n)
              (let ([lead (bytevector-u8-ref bytes i)])
                (if (fx<? lead #x80)
                  (begin (put-char out (integer->char lead)) (loop (+ i 1)))
                  (let-values ([(count initial minimum)
                                (cond
                                  [(fx=? (fxand lead #xE0) #xC0) (values 1 (fxand lead #x1F) #x80)]
                                  [(fx=? (fxand lead #xF0) #xE0) (values 2 (fxand lead #x0F) #x800)]
                                  [(fx=? (fxand lead #xF8) #xF0) (values 3 (fxand lead #x07) #x10000)]
                                  [else (values 0 #f 0)])])
                    (if (not initial)
                      (begin (put-char out #\xFFFD) (loop (+ i 1)))
                      (let decode ([k count] [j (+ i 1)] [code initial])
                        (cond
                          [(fx=? k 0)
                            (put-char out
                              (if (or (< code minimum) (> code #x10FFFF) (<= #xD800 code #xDFFF))
                                #\xFFFD
                                (integer->char code)))
                            (loop j)]
                          [(continuation j)
                            => (lambda (b)
                                 (decode (fx- k 1) (+ j 1) (fxior (fxarithmetic-shift-left code 6) b)))]
                          [else (put-char out #\xFFFD) (loop j)]))))))))))))

  ;; The JSON value that the UTF-8 bytevector BYTES holds; a byte order mark is skipped.
  (define (bytevector->json bytes
            #:object [object (json-object-type)]
            #:array [array (json-array-type)]
            #:key [key (json-key-type)]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (unless (bytevector? bytes) (assertion-violation 'bytevector->json "not a bytevector" bytes))
    (let* ([text (guard (e [#t (decode-utf8-leniently bytes)]) (utf8->string bytes))]
           [text (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\xFEFF))
                   (substring text 1 (string-length text))
                   text)])
      (parse-text text 'bytevector->json object array key null max-depth)))

  ;; ---- Writing ----

  (define hex-digits "0123456789abcdef")

  (define (put-u-escape out code)
    (put-string out "\\u")
    (put-char out (string-ref hex-digits (fxand (fxarithmetic-shift-right code 12) 15)))
    (put-char out (string-ref hex-digits (fxand (fxarithmetic-shift-right code 8) 15)))
    (put-char out (string-ref hex-digits (fxand (fxarithmetic-shift-right code 4) 15)))
    (put-char out (string-ref hex-digits (fxand code 15))))

  ;; MODE is 0 for plain output, else a bit set: 1 escapes non-ASCII, 2 escapes what HTML
  ;; would see (< > & ' U+2028 U+2029).
  (define (needs-escape? c mode)
    (or (char=? c #\") (char=? c #\\) (char<? c #\space)
      (and (fx>? mode 0)
        (or (and (fx=? (fxand mode 1) 1) (char>? c #\x7E))
          (and (fx=? (fxand mode 2) 2)
            (memv c '(#\< #\> #\& #\' #\x2028 #\x2029)))))))

  (define (put-escaped-char out c)
    (case c
      [(#\") (put-string out "\\\"")]
      [(#\\) (put-string out "\\\\")]
      [(#\newline) (put-string out "\\n")]
      [(#\return) (put-string out "\\r")]
      [(#\tab) (put-string out "\\t")]
      [(#\backspace) (put-string out "\\b")]
      [(#\xC) (put-string out "\\f")]
      [else
        (let ([code (char->integer c)])
          (if (> code #xFFFF)
            (let ([v (- code #x10000)])
              (put-u-escape out (+ #xD800 (fxarithmetic-shift-right v 10)))
              (put-u-escape out (+ #xDC00 (fxand v #x3FF))))
            (put-u-escape out code)))]))

  (define (write-string s out mode)
    (put-char out #\")
    (let ([n (string-length s)])
      (let loop ([i 0] [from 0])
        (if (fx=? i n)
          (when (fx<? from n) (put-string out s from (fx- n from)))
          (let ([c (string-ref s i)])
            (if (needs-escape? c mode)
              (begin
                (when (fx<? from i) (put-string out s from (fx- i from)))
                (put-escaped-char out c)
                (loop (fx+ i 1) (fx+ i 1)))
              (loop (fx+ i 1) from))))))
    (put-char out #\"))

  (define (write-number x out who)
    (cond
      [(and (exact? x) (integer? x)) (put-string out (number->string x))]
      [(and (exact? x) (rational? x)) (write-number (inexact x) out who)]
      [(and (flonum? x) (or (nan? x) (infinite? x)))
        (write-error who "NaN and infinities have no JSON form" x)]
      [(flonum? x)
        ;; number->string writes 0.0 as "+0.0"
        (let ([s (number->string x)])
          (if (char=? (string-ref s 0) #\+)
            (put-string out s 1 (- (string-length s) 1))
            (put-string out s)))]
      [else (write-error who "this number has no JSON form" x)]))

  (define (key-string k who)
    (cond
      [(symbol? k) (symbol->string k)]
      [(string? k) k]
      [else (write-error who "an object key must be a symbol or a string" k)]))

  (define (alist? x)
    (and (pair? x)
      (let loop ([x x])
        (cond
          [(null? x) #t]
          [(and (pair? x) (pair? (car x)) (or (symbol? (caar x)) (string? (caar x))))
            (loop (cdr x))]
          [else #f]))))

  (define (sort-members members)
    (list-sort (lambda (a b) (string<? (car a) (car b))) members))

  ;; The members of the object X as (key-string . value) pairs, sorted by key.
  (define (object-members x who sort?)
    (let ([members
           (if (hashtable? x)
             (let-values ([(keys values) (hashtable-entries x)])
               (map (lambda (k v) (cons (key-string k who) v))
                 (vector->list keys) (vector->list values)))
             (map (lambda (m) (cons (key-string (car m) who) (cdr m))) x))])
      (if (or sort? (hashtable? x)) (sort-members members) members)))

  (define (write-json x out who indent sort? mode null max-depth)
    (define unit
      (cond
        [(not indent) #f]
        [(and (integer? indent) (exact? indent) (>= indent 0)) (make-string indent #\space)]
        [(string? indent) indent]
        [else (assertion-violation who "#:indent must be #f, a count of spaces or a string" indent)]))
    (define (newline-indent level)
      (when unit
        (put-char out #\newline)
        (let loop ([k level]) (when (> k 0) (put-string out unit) (loop (- k 1))))))
    (define (write-elements count ref level)
      (put-char out #\[)
      (unless (= count 0)
        (let loop ([i 0])
          (when (< i count)
            (when (> i 0) (put-char out #\,))
            (newline-indent (+ level 1))
            (write-value (ref i) (+ level 1))
            (loop (+ i 1))))
        (newline-indent level))
      (put-char out #\]))
    (define (write-members members level)
      (put-char out #\{)
      (unless (null? members)
        (let loop ([ms members] [first? #t])
          (unless (null? ms)
            (unless first? (put-char out #\,))
            (newline-indent (+ level 1))
            (write-string (key-string (caar ms) who) out mode)
            (put-char out #\:)
            (when unit (put-char out #\space))
            (write-value (cdar ms) (+ level 1))
            (loop (cdr ms) #f)))
        (newline-indent level))
      (put-char out #\}))
    (define (deeper level)
      (when (>= level max-depth)
        (write-error who
          (string-append "the value nests deeper than " (number->string max-depth)
            " levels (is it cyclic?)"))))
    (define (write-value x level)
      (cond
        [(string? x) (write-string x out mode)]
        [(eq? x #t) (put-string out "true")]
        [(eq? x #f) (put-string out "false")]
        [(or (eq? x 'null) (eq? x null)) (put-string out "null")]
        [(fixnum? x) (put-string out (number->string x))]
        [(number? x) (write-number x out who)]
        [(vector? x)
          (deeper level)
          (write-elements (vector-length x) (lambda (i) (vector-ref x i)) level)]
        [(null? x) (put-string out "{}")]
        [(alist? x)
          (deeper level)
          (write-members (if sort? (object-members x who #t) x) level)]
        [(hashtable? x)
          (deeper level)
          (write-members (object-members x who #t) level)]
        [(and (pair? x) (list? x))
          (deeper level)
          (let ([v (list->vector x)])
            (write-elements (vector-length v) (lambda (i) (vector-ref v i)) level))]
        [(symbol? x) (write-string (symbol->string x) out mode)]
        [else (write-error who "this value has no JSON form" x)]))
    (write-value x 0))

  (define (escape-mode ascii? html-safe?)
    (fxior (if ascii? 1 0) (if html-safe? 2 0)))

  ;; Writes VALUE as JSON to PORT (a textual or binary output port; UTF-8 on a binary one).
  ;; #:indent is #f for compact output, or the number of spaces (or the string) to indent each
  ;; level by. #:sort-keys? writes the members of objects sorted by key (those of a hashtable
  ;; always are). #:ascii? escapes every character outside ASCII as \uXXXX; #:html-safe?
  ;; escapes < > & ' U+2028 and U+2029, so the text can go in a <script> element.
  (define (json-write value [port (current-output-port)]
            #:indent [indent #f]
            #:sort-keys? [sort-keys? #f]
            #:ascii? [ascii? #f]
            #:html-safe? [html-safe? #f]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (cond
      [(and (output-port? port) (textual-port? port))
        (write-json value port 'json-write indent sort-keys? (escape-mode ascii? html-safe?)
          null max-depth)]
      [(and (output-port? port) (binary-port? port))
        (put-bytevector port
          (string->utf8
            (call-with-string-output-port
              (lambda (out)
                (write-json value out 'json-write indent sort-keys?
                  (escape-mode ascii? html-safe?) null max-depth)))))]
      [else (assertion-violation 'json-write "not an output port" port)]))

  ;; VALUE as a JSON string; the keywords are those of `json-write`.
  (define (json->string value
            #:indent [indent #f]
            #:sort-keys? [sort-keys? #f]
            #:ascii? [ascii? #f]
            #:html-safe? [html-safe? #f]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (call-with-string-output-port
      (lambda (out)
        (write-json value out 'json->string indent sort-keys? (escape-mode ascii? html-safe?)
          null max-depth))))

  ;; VALUE as the UTF-8 bytes of its JSON; the keywords are those of `json-write`.
  (define (json->bytevector value
            #:indent [indent #f]
            #:sort-keys? [sort-keys? #f]
            #:ascii? [ascii? #f]
            #:html-safe? [html-safe? #f]
            #:null [null (json-null)]
            #:max-depth [max-depth (json-max-depth)])
    (string->utf8
      (call-with-string-output-port
        (lambda (out)
          (write-json value out 'json->bytevector indent sort-keys?
            (escape-mode ascii? html-safe?) null max-depth)))))

  ;; ---- Values ----

  ;; Whether X is `null`: the symbol `null` or the value of `(json-null)`.
  (define (json-null? x)
    (or (eq? x 'null) (eq? x (json-null))))

  ;; Whether X is an object the reader could have built: an alist with symbol or string keys,
  ;; `()`, or a hashtable.
  (define (json-object? x)
    (or (null? x) (alist? x) (hashtable? x)))

  ;; Whether X is an array: a vector.
  (define (json-array? x)
    (vector? x))

  ;; The member KEY (a symbol or a string) of the object OBJ, or the element KEY (an exact
  ;; integer) of the array OBJ; DEFAULT (#f) when there is none, or when OBJ is not an object
  ;; or an array. KEY may also be a list of keys, followed one after the other:
  ;;
  ;;   (json-ref '((a . #(((b . 1))))) '(a 0 b))  => 1
  (define (json-ref obj key [default #f])
    (if (list? key)
      (let loop ([obj obj] [path key])
        (if (null? path)
          obj
          (let ([next (member-ref obj (car path) missing-mark)])
            (if (eq? next missing-mark) default (loop next (cdr path))))))
      (let ([v (member-ref obj key missing-mark)])
        (if (eq? v missing-mark) default v))))

  ;; A value no document holds.
  (define missing-mark (list 'missing))

  (define (same-key? k key)
    (cond
      [(eq? k key) #t]
      [(and (symbol? k) (string? key)) (string=? (symbol->string k) key)]
      [(and (string? k) (symbol? key)) (string=? k (symbol->string key))]
      [(and (string? k) (string? key)) (string=? k key)]
      [else #f]))

  (define (member-ref obj key absent)
    (cond
      [(and (integer? key) (exact? key))
        (cond
          [(vector? obj)
            (if (and (>= key 0) (< key (vector-length obj))) (vector-ref obj key) absent)]
          [(and (list? obj) (not (alist? obj)))
            (let loop ([l obj] [k key])
              (cond [(or (null? l) (< k 0)) absent] [(= k 0) (car l)] [else (loop (cdr l) (- k 1))]))]
          [else absent])]
      [(or (symbol? key) (string? key))
        (cond
          [(hashtable? obj)
            ;; the table may hash only one of the two kinds of key
            (let ([lookup (lambda (k)
                            (guard (e [#t missing-mark]) (hashtable-ref obj k missing-mark)))])
              (let ([v (lookup key)])
                (if (eq? v missing-mark)
                  (let ([v (lookup (if (symbol? key) (symbol->string key) (string->symbol key)))])
                    (if (eq? v missing-mark) absent v))
                  v)))]
          [(pair? obj)
            (let loop ([l obj])
              (cond
                [(not (pair? l)) absent]
                [(and (pair? (car l)) (same-key? (caar l) key)) (cdar l)]
                [else (loop (cdr l))]))]
          [else absent])]
      [else (assertion-violation 'json-ref "a key must be a symbol, a string or an index" key)]))

  ;; ---- JSON Pointer (RFC 6901) ----

  ;; The reference tokens of the JSON Pointer string TEXT: "" is the whole document, "/a/0" is
  ;; ("a" "0"); "~1" stands for '/' and "~0" for '~'.
  (define (string->json-pointer text)
    (let ([n (string-length text)])
      (cond
        [(= n 0) '()]
        [(not (char=? (string-ref text 0) #\/))
          (raise-json-error 'string->json-pointer "a JSON Pointer must be empty or start with '/'"
            #f #f #f (list text))]
        [else
          (let loop ([i 1] [token '()] [tokens '()])
            (define (token-string) (list->string (reverse token)))
            (cond
              [(= i n) (reverse (cons (token-string) tokens))]
              [(char=? (string-ref text i) #\/) (loop (+ i 1) '() (cons (token-string) tokens))]
              [(char=? (string-ref text i) #\~)
                (let ([e (and (< (+ i 1) n) (string-ref text (+ i 1)))])
                  (case e
                    [(#\0) (loop (+ i 2) (cons #\~ token) tokens)]
                    [(#\1) (loop (+ i 2) (cons #\/ token) tokens)]
                    [else
                      (raise-json-error 'string->json-pointer
                        "'~' must be followed by 0 or 1 in a JSON Pointer" #f #f #f (list text))]))]
              [else (loop (+ i 1) (cons (string-ref text i) token) tokens)]))])))

  ;; The JSON Pointer string of a list of tokens (strings, symbols or exact integers).
  (define (json-pointer->string tokens)
    (call-with-string-output-port
      (lambda (out)
        (for-each
          (lambda (token)
            (put-char out #\/)
            (string-for-each
              (lambda (c)
                (case c
                  [(#\~) (put-string out "~0")]
                  [(#\/) (put-string out "~1")]
                  [else (put-char out c)]))
              (cond
                [(string? token) token]
                [(symbol? token) (symbol->string token)]
                [(and (integer? token) (exact? token) (>= token 0)) (number->string token)]
                [else (assertion-violation 'json-pointer->string "not a pointer token" token)])))
          tokens))))

  (define (array-index token)
    (let ([n (string-length token)])
      (and (> n 0)
        (for-all (lambda (c) (char<=? #\0 c #\9)) (string->list token))
        (or (= n 1) (not (char=? (string-ref token 0) #\0)))
        (string->number token 10))))

  ;; The value that POINTER (a JSON Pointer string or a list of tokens) refers to in DOC. When
  ;; there is none, DEFAULT if it is given, else a `&json-error` is raised.
  ;;
  ;;   (json-pointer-ref '((a . #(10 20))) "/a/1")  => 20
  (define (json-pointer-ref doc pointer [default missing-mark])
    (let ([tokens (if (string? pointer) (string->json-pointer pointer) pointer)])
      (let loop ([v doc] [ts tokens] [seen '()])
        (if (null? ts)
          v
          (let* ([token (let ([t (car ts)])
                          (cond [(symbol? t) (symbol->string t)]
                            [(integer? t) (number->string t)]
                            [else t]))]
                 [next (cond
                         [(or (vector? v) (and (pair? v) (list? v) (not (alist? v))))
                           (let ([i (array-index token)])
                             (if i (member-ref v i missing-mark) missing-mark))]
                         [(or (null? v) (pair? v) (hashtable? v))
                           (member-ref v token missing-mark)]
                         [else missing-mark])])
            (if (eq? next missing-mark)
              (if (eq? default missing-mark)
                (raise-json-error 'json-pointer-ref
                  (string-append "no value at "
                    (json-pointer->string (reverse (cons token seen))))
                  #f #f #f (list pointer))
                default)
              (loop next (cdr ts) (cons token seen))))))))
  )
