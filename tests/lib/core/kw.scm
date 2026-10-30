;; Tests for keyword arguments (lib/core/kw.scm): keyword-aware lambda, define,
;; |#%app| and apply as exported by (core).
(import (core) (only (capy) expand) (srfi 64))

(test-begin "kw")

(define (kw-values proc)
  (call-with-values (lambda () (procedure-keywords proc)) list))

;; ---- plain and optional-only definitions (no keyword machinery) ----

(define (plain x) (+ x 1))
(test-equal "plain define call" 11 (plain 10))

(define (with-opts a [b 1] [c 2]) (list a b c))
(test-equal "optionals all default" '(0 1 2) (with-opts 0))
(test-equal "optionals one given" '(0 10 2) (with-opts 0 10))
(test-equal "optionals all given" '(0 10 20) (with-opts 0 10 20))

(test-equal "lambda plain" 5 ((lambda (x) (+ x 1)) 4))
(test-equal "lambda optionals" '(1 2) ((lambda (a [b 2]) (list a b)) 1))
(define opt-fn (lambda (a [b 2]) (list a b)))
(test-equal "lambda through variable" '(1 9) (opt-fn 1 9))
(test-equal "plain procedure-keywords" '(() ()) (kw-values plain))
(test-equal "optional-only procedure-keywords" '(() ()) (kw-values with-opts))
(test-assert "optional-only is a procedure" (procedure? with-opts))

;; ---- optional keywords ----

(define (greet name #:greeting [g "hello"] #:punct [p "!"])
  (string-append g " " name p))
(test-equal "kw defaults" "hello ada!" (greet "ada"))
(test-equal "kw one given" "hello ada?" (greet "ada" #:punct "?"))
(test-equal "kw out of order" "hi ada?" (greet "ada" #:punct "?" #:greeting "hi"))
(test-equal "kw both given" "hi ada?" (greet "ada" #:greeting "hi" #:punct "?"))
(test-equal "kw procedure-keywords" '(() (#:greeting #:punct)) (kw-values greet))
(test-assert "kw procedure is a procedure" (procedure? greet))
(test-error "kw unknown keyword" #t (greet "ada" #:bogus 1))
(test-error "kw too many positionals" #t (greet "a" "b"))
(test-error "kw too few positionals" #t (greet))

;; ---- required keywords ----

(define (need-kw x #:req r #:opt [o 0]) (list x r o))
(test-equal "required minimal" '(1 2 0) (need-kw 1 #:req 2))
(test-equal "required full" '(1 2 3) (need-kw 1 #:req 2 #:opt 3))
(test-equal "required procedure-keywords"
  '((#:req) (#:opt #:req)) (kw-values need-kw))
(test-error "required missing, no kws" #t (need-kw 1))
(test-error "required missing, other kw" #t (need-kw 1 #:opt 5))
(test-error "required unknown kw" #t (need-kw 1 #:req 2 #:bogus 3))

;; ---- rest arguments ----

(define (with-rest a [b 1] . r) (list a b r))
(test-equal "rest empty" '(0 1 ()) (with-rest 0))
(test-equal "rest omitted optional" '(0 1 (2 3)) (with-rest 0 1 2 3))
(test-equal "rest given optional" '(0 9 (2 3)) (with-rest 0 9 2 3))

(define (kw-rest x #:k [k 0] . r) (list x k r))
(test-equal "kw+rest defaults" '(1 0 ()) (kw-rest 1))
(test-equal "kw+rest full" '(1 2 (3 4)) (kw-rest 1 3 4 #:k 2))
(test-equal "kw+rest kw only" '(1 2 ()) (kw-rest 1 #:k 2))

;; ---- dependent and effectful defaults ----

(define (dep a [b a] #:k [k b]) (list a b k))
(test-equal "dependent defaults" '(1 1 1) (dep 1))
(test-equal "dependent positional" '(1 2 2) (dep 1 2))
(test-equal "dependent kw" '(1 2 3) (dep 1 2 #:k 3))

(define counter 0)
(define (counting #:n [n (begin (set! counter (+ counter 1)) counter)]) n)
(test-equal "default evaluated on call" 1 (counting))
(test-equal "default re-evaluated" 2 (counting))
(test-equal "default skipped when supplied" 99 (counting #:n 99))
(test-equal "counter untouched by supplied call" 2 counter)

;; ---- slow path: computed rators ----

(test-equal "slow path if-rator"
  "hi ada?" ((if #t greet plain) "ada" #:greeting "hi" #:punct "?"))
(define greet-alias greet)
(test-equal "slow path alias with kws" "hello ada?" (greet-alias "ada" #:punct "?"))
(test-equal "slow path alias no kws" "hello ada!" (greet-alias "ada"))
(test-error "slow path unknown kw" #t (greet-alias "ada" #:bogus 1))
(test-error "slow path missing required" #t ((if #t need-kw plain) 1))
(test-error "slow path plain proc rejects kws" #t ((if #f greet plain) 1 #:k 2))
(test-error "slow path non-procedure" #t ((if #f greet 42) 1 #:k 2))
(test-error "slow path bad arity" #t (greet-alias "a" "b" #:punct "?"))

;; ---- keyword-apply ----

(test-equal "keyword-apply basic"
  "hi ada?" (keyword-apply greet '(#:greeting #:punct) '("hi" "?") '("ada")))
(test-equal "keyword-apply empty kws" 11 (keyword-apply plain '() '() '(10)))
(test-equal "keyword-apply extra arg lists"
  '(0 10 20) (keyword-apply with-opts '() '() 0 '(10 20)))
(test-error "keyword-apply unsorted kws" #t
  (keyword-apply greet '(#:punct #:greeting) '("?" "hi") '("ada")))
(test-error "keyword-apply length mismatch" #t
  (keyword-apply greet '(#:greeting) '("hi" "?" ) '("ada")))
(test-error "keyword-apply unknown kw" #t
  (keyword-apply greet '(#:bogus) '(1) '("ada")))
(test-error "keyword-apply missing required" #t
  (keyword-apply need-kw '() '() '(1)))

;; ---- make-keyword-procedure ----

(define kp (make-keyword-procedure (lambda (kws vals x) (list kws vals x))))
(test-equal "make-keyword-procedure call" '((#:a) (1) 5) (kp 5 #:a 1))
(test-equal "make-keyword-procedure any kws"
  '((#:a #:z) (1 2) 5) (kp 5 #:z 2 #:a 1))
(test-equal "make-keyword-procedure keywords" '(() #f) (kw-values kp))
(test-equal "make-keyword-procedure no-kw call" '(() () 5) (kp 5))

;; ---- procedure-reduce-keyword-arity ----

(define greet-g (procedure-reduce-keyword-arity greet 1 '(#:greeting) '(#:greeting)))
(test-equal "reduced keeps allowed kw" "hi ada!" (greet-g "ada" #:greeting "hi"))
(test-error "reduced no-kw call missing required" #t (greet-g "ada"))
(test-error "reduced rejects removed kw" #t (greet-g "ada" #:punct "?"))
(test-equal "reduced procedure-keywords" '((#:greeting) (#:greeting)) (kw-values greet-g))

(define need-also
  (procedure-reduce-keyword-arity need-kw 1 '(#:opt #:req) '(#:opt #:req)))
(test-equal "reduced requiring optional" '(1 2 3) (need-also 1 #:req 2 #:opt 3))
(test-error "reduced newly required missing" #t (need-also 1 #:req 2))

(define plain-only (procedure-reduce-keyword-arity greet 1 '() '()))
(test-equal "reduced to plain" "hello ada!" (plain-only "ada"))
(test-error "reduced to plain rejects kws" #t (plain-only "ada" #:greeting "hi"))

;; ---- expansion-time errors (via expand + guard) ----

(define (expand-error-message expr)
  (guard (c [#t (condition-message c)])
    (expand expr)
    #f))

(test-equal "dup call kw rejected" "duplicate keyword in application"
  (expand-error-message '(greet "a" #:x 1 #:x 2)))
(test-equal "missing kw value rejected" "missing argument expression after keyword"
  (expand-error-message '(greet "a" #:x)))
(test-equal "kw in value position rejected"
  "keyword in expression position (immediately after another keyword)"
  (expand-error-message '(greet #:x #:y 1)))
(test-equal "dup optional formal rejected" "duplicate keyword for argument"
  (expand-error-message '(lambda (x #:a [a 1] #:a [b 2]) x)))
(test-equal "dup required formal rejected" "duplicate keyword for argument"
  (expand-error-message '(lambda (#:a a #:a b) a)))
(test-equal "set! on kw binding rejected" "cannot assign to a keyword-procedure binding"
  (expand-error-message '(set! greet 1)))
(test-equal "bad lambda form rejected" "bad lambda form"
  (expand-error-message '(lambda (x))))
(test-assert "define plain rhs expands"
  (not (expand-error-message '(define plain-rhs-val 5))))

;; ---- keyword utilities ----

(test-assert "keyword<?" (keyword<? '#:a '#:b))
(test-assert "keyword<? not reversed" (not (keyword<? '#:b '#:a)))
(test-equal "string->keyword" '#:hi (string->keyword "hi"))


;; ---- keyword-aware apply ----

(test-equal "apply plain" 6 (apply + 1 '(2 3)))
(test-equal "apply keywords" "hi ada?"
  (apply greet "ada" #:greeting "hi" #:punct "?" '()))
(test-equal "apply keywords + list" "hi bo!"
  (apply greet #:greeting "hi" '("bo")))
(test-equal "apply as value" 3 ((lambda (f) (f + 1 '(2))) apply))


;; ---- plain procedures receive keywords as trailing arguments ----

(define (collect . args) args)
(test-equal "keywords pass through to plain procedures"
  '(1 #:a 2 #:b 3) (collect 1 #:a 2 #:b 3))
(test-equal "|#%plain-app| leaves keywords alone"
  '(#:a 2) (|#%plain-app| collect #:a 2))

;; ---- define-method with keyword formals, make-instance ----

(define klass (make-class 'kw-point '(x y)))
(define inst (make-instance klass #:y 2 #:x 1))
(test-equal "make-instance with literal keywords"
  '(1 2) (list (slot-ref inst 'x) (slot-ref inst 'y)))

(test-end "kw")
