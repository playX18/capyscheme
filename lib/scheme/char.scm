(define-library (scheme char)
  (import (core primitives)
    (core unicode))
  (export char-alphabetic?
    char-ci<?
    char-ci>=?
    char-downcase
    char-lower-case?
    char-upcase
    char-whitespace?
    string-ci<=?
    string-ci=?
    string-ci>?
    string-foldcase
    char-ci<=?
    char-ci=?
    char-ci>?
    char-foldcase
    char-numeric?
    char-upper-case?
    digit-value
    string-ci<?
    string-ci>=?
    string-downcase
    string-upcase)
  (begin
    ;; R7RS-small: digit-value of a decimal digit character, #f otherwise.
    (define digit-value
      (lambda (c)
        (if (char-numeric? c)
          (- (char->integer c) 48)
          #f)))))
