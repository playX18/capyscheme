;; mop/forward-classes: a superclass name that is not defined yet makes a
;; forward-referenced placeholder, resolved by the later definition (CLISP
;; semantics). The AMOP-semantics mode is deferred (decision D11).
(import (srfi 64)
        (capy)
        (capy mop)
        (rnrs eval))

(test-begin "mop feature forward classes")

(define-class fwc-child (fwc-parent) ())

(test-group "CLISP semantics: before the parent is defined"
  (test-assert "the child is a class before its parent exists"
    (class? fwc-child))
  (test-assert "the parent is a forward-referenced placeholder"
    (is-a? (car (class-direct-supers fwc-child)) <forward-referenced-class>)))

(define-class fwc-parent () ((p #:init-value 1)))

(test-group "CLISP semantics: after the parent is defined"
  ;; The variable names the descriptor from before the resolution (see
  ;; mop-forward-refs.scm); instances carry the current one.
  (test-assert "the current subclass has the real parent in its CPL"
    (and (memq fwc-parent (class-precedence-list (class-of (make fwc-child)))) #t))
  (test-equal "instances see the resolved superclass slots"
    1
    (slot-ref (make fwc-child) 'p)))

(test-group "amop mode"
  (test-expect-fail "AMOP forward-reference semantics are selectable")
  (test-assert "AMOP forward-reference semantics are selectable"
    (eval 'forward-reference-mode (environment '(capy mop)))))

(test-end "mop feature forward classes")
