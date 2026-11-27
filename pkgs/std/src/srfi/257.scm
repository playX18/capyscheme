;;; SPDX-FileCopyrightText: 2024 Sergei Egorov
;;; SPDX-License-Identifier: MIT

;; SRFI 257: the implementation is (core pattern-match), which the compiler of core uses.
(define-library (srfi 257)
  (import (core pattern-match))
  (export
    ; aux keywords re-exported from (core)
    _
    ...
    =>
    quote
    quasiquote
    unquote
    unquote-splicing
    match
    ~value
    ~cons
    ~list
    ~append
    ~append/ng
    ~append/t
    ~etc ; et cetera sine errore
    ~etcse
    ~vector
    ~vector-append
    ~vector-append/ng
    ~string
    ~string-append
    ~string-append/ng
    ~vector->list
    ~string->list
    ~list->vector
    ~list->string
    ~string->symbol
    ~symbol->string
    ~string->number
    ~number->string
    ~null?
    ~pair?
    ~list?
    ~boolean?
    ~number?
    ~integer?
    ~vector?
    ~string?
    ~symbol?
    ~char?
    ~and
    ~or
    ~not
    ~cut!
    ~list*
    ~list-no-order
    ~list-no-order*
    ~=
    ~?
    ~prop
    ~test
    ~iterate
    ~if-id-member
    ~replace-specials
    define-match-pattern
    define-record-match-pattern
    value
    etc))
