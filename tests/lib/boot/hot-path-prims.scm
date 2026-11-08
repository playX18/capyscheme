;; Regression tests for the compile-time rewrites and natives on the hot path
;; of text readers: memv/memq/eqv? against constants, one-pass reverse and
;; list->string, and the fixed-arity read-char / peek-char / write-char.
(import (scheme base)
        (scheme char)
        (scheme write)
        (srfi 64))

(test-begin "hot path prims")

(define (circular . items)
  (let ((l (apply list items)))
    (let loop ((p l))
      (if (null? (cdr p)) (set-cdr! p l) (loop (cdr p))))
    l))

(define (count-up from n)
  (let loop ((i (+ from n -1)) (acc '()))
    (if (< i from) acc (loop (- i 1) (cons i acc)))))

;;; memv / memq / eqv? against constants

(define (ws? c) (memv c '(#\space #\tab #\newline #\return #\page)))

(test-equal "memv 5-element constant list, first" '(#\space #\tab #\newline #\return #\page) (ws? #\space))
(test-equal "memv 5-element constant list, last" '(#\page) (ws? #\page))
(test-equal "memv 5-element constant list, miss" #f (ws? #\a))
(test-equal "memv non-char key misses" #f (ws? 32))

(define (big-memv x) (memv x '(1 2.5 100000000000000000000 #\a)))
(test-equal "memv bignum constant" '(100000000000000000000 #\a) (big-memv 100000000000000000000))
(test-equal "memv flonum constant" '(2.5 100000000000000000000 #\a) (big-memv 2.5))
(test-equal "memv fixnum vs flonum" '(1 2.5 100000000000000000000 #\a) (big-memv 1))
(test-equal "memv 1.0 is not 1" #f (big-memv 1.0))

(define (one-bignum x) (memv x '(100000000000000000000)))
(test-equal "memv single bignum constant hit" '(100000000000000000000) (one-bignum 100000000000000000000))
(test-equal "memv single bignum constant miss" #f (one-bignum 1))

(define (memq-sym x) (memq x '(a b c d e)))
(test-equal "memq 5 symbols" '(e) (memq-sym 'e))
(test-equal "memq 5 symbols miss" #f (memq-sym 'z))

(define (eqv-char c) (eqv? c #\e))
(define (eqv-sym s) (eqv? 'foo s))
(define (eqv-fix n) (eqv? n 42))
(define (eqv-big n) (eqv? n 100000000000000000000))
(define (eqv-flo n) (eqv? n 1.5))
(test-assert "eqv? char hit" (eqv-char #\e))
(test-assert "eqv? char miss" (not (eqv-char #\f)))
(test-assert "eqv? char vs int" (not (eqv-char 101)))
(test-assert "eqv? symbol" (eqv-sym 'foo))
(test-assert "eqv? fixnum" (eqv-fix 42))
(test-assert "eqv? fixnum vs flonum" (not (eqv-fix 42.0)))
(test-assert "eqv? bignum hit" (eqv-big 100000000000000000000))
(test-assert "eqv? bignum computed" (eqv-big (* 10000000000 10000000000)))
(test-assert "eqv? flonum" (eqv-flo 1.5))

;;; char comparisons

(define (char-chain a b c) (char<? a b c))
(test-assert "char=? 2" (char=? #\a #\a))
(test-assert "char=? 3 true" (char=? #\a #\a #\a))
(test-assert "char=? 3 false" (not (char=? #\a #\a #\b)))
(test-assert "char<? chain" (char-chain #\a #\b #\c))
(test-assert "char<? chain false" (not (char-chain #\a #\c #\b)))
(test-error "char=? rejects a non-char" (char=? #\a 97))

;;; reverse

(test-equal "reverse" '(3 2 1) (reverse '(1 2 3)))
(test-equal "reverse empty" '() (reverse '()))
(test-equal "reverse long" 5000 (car (reverse (count-up 1 5000))))
(test-error "reverse improper" (reverse '(1 2 . 3)))
(test-error "reverse non-list" (reverse 5))
(test-error "reverse circular" (reverse (circular 1 2 3)))
(test-equal "reverse as a value" '((3 2 1)) (map reverse '((1 2 3))))

;;; list->string

(test-equal "list->string" "abc" (list->string '(#\a #\b #\c)))
(test-equal "list->string empty" "" (list->string '()))
(test-equal "list->string non-ascii" "λx" (list->string (list #\λ #\x)))
(test-error "list->string improper" (list->string '(#\a . #\b)))
(test-error "list->string non-char" (list->string '(#\a 1)))
(test-error "list->string circular" (list->string (circular #\a #\b)))
(test-equal "string->number via reversed digits" 1234
  (string->number (list->string (reverse (list #\4 #\3 #\2 #\1)))))

;;; ports

(let ((p (open-input-string "xyz")))
  (test-equal "peek-char" #\x (peek-char p))
  (test-equal "peek-char does not consume" #\x (peek-char p))
  (test-equal "read-char" #\x (read-char p))
  (test-equal "read-char 2" #\y (read-char p))
  (test-equal "read-char 3" #\z (read-char p))
  (test-assert "read-char eof" (eof-object? (read-char p)))
  (test-assert "peek-char eof" (eof-object? (peek-char p))))

(let ((p (open-input-string "a\nb\r\nc")))
  (test-equal "newline" (list #\a #\newline #\b)
    (list (read-char p) (read-char p) (read-char p))))

(test-equal "read-char from current-input-port" #\q
  (parameterize ((current-input-port (open-input-string "q"))) (read-char)))
(test-equal "peek-char from current-input-port" #\q
  (parameterize ((current-input-port (open-input-string "q"))) (peek-char)))

(test-error "read-char on a non-port" (read-char 5))
(test-error "peek-char on a non-port" (peek-char 'x))
(test-error "read-char with two arguments" (read-char (open-input-string "a") 1))
(test-error "read-char on a closed port"
  (let ((p (open-input-string "a"))) (close-port p) (read-char p)))

(test-equal "write-char to a port" "hi"
  (let ((o (open-output-string)))
    (write-char #\h o)
    (write-char #\i o)
    (get-output-string o)))
(test-equal "write-char to current-output-port" "z"
  (let ((o (open-output-string)))
    (parameterize ((current-output-port o)) (write-char #\z))
    (get-output-string o)))

(test-end "hot path prims")
