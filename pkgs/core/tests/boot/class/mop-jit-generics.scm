(import (srfi 64)
        (rnrs)
        (capy)
        (capy mop))

;; Tier 2 generic calls (-) must give the same answers as dispatch after

;; Redefinitions are top-level forms: a second `define-class` of one name in
;; the same body is a duplicate internal definition.

(test-begin "boot class mop jit generics")

;; The last result of calling `f` on `x` 300 times (the profiled call site).
(define (hot f x)
  (let loop ([i 0] [r #f])
    (if (= i 300) r (loop (+ i 1) (f x)))))

;; The results of 300 calls of `thunk`, with `flip` run before call 150.
(define (flip-trace thunk flip)
  (let loop ([i 0] [acc '()])
    (cond [(= i 300) (reverse acc)]
          [else
           (when (= i 150) (flip))
           (loop (+ i 1) (cons (thunk) acc))])))

;; The trace is `before` for calls 0..149 and `after` for calls 150..299.
(define (flip-ok? trace before after)
  (let loop ([t trace] [i 0])
    (cond [(null? t) (= i 300)]
          [(equal? (car t) (if (< i 150) before after))
           (loop (cdr t) (+ i 1))]
          [else #f])))

(define (method-for gf cls)
  (car (filter (lambda (m) (equal? (method-specializers m) (list cls)))
               (generic-function-methods gf))))

(define (make-list n x)
  (let loop ([i 0] [acc (quote ())])
    (if (= i n) acc (loop (+ i 1) (cons x acc)))))

(define (iota-list n)
  (let loop ([i (- n 1)] [acc (quote ())])
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

(define (warm thunk)
  (do ((i 0 (+ i 1))) ((= i 3)) (flip-trace thunk (lambda () #f))))

;; ---------------------------------------------------------------------------
;; Single argument, one method cache shape.

(define-class <jg-a> () ())
(define-class <jg-b> (<jg-a>) ())
(define-class <jg-c> (<jg-b>) ())
(define-generic jg-who)
(define-method (jg-who (o <jg-a>)) 'a)
(define-method (jg-who (o <jg-b>)) 'b)

(define c1 (make <jg-c>))
(define b1 (make <jg-b>))
(define a1 (make <jg-a>))

(test-equal "warm by profiled call" 'b (hot jg-who b1))
(warm (lambda () (jg-who c1)))

(test-assert "add-method! flips a hot call"
  (flip-ok? (flip-trace (lambda () (jg-who c1))
                        (lambda ()
                          (add-method! jg-who (list <jg-c>) 1
                                       (lambda (next o) 'c)
                                       #f #f #f #f '())))
            'b 'c))

(test-equal "a fresh call after add-method!" 'c (hot jg-who c1))

(test-assert "remove-method! flips it back"
  (flip-ok? (flip-trace (lambda () (jg-who c1))
                        (lambda () (remove-method! jg-who (method-for jg-who <jg-c>))))
            'c 'b))

(test-equal "after remove-method!" 'b (hot jg-who c1))

;; ---------------------------------------------------------------------------
;; Class redefinition between hot loops changes the answer for new instances.

(define-class <jg-d> (<jg-a>) ())
(define-generic jg-redef)
(define-method (jg-redef (o <jg-a>)) 'a)
(define-method (jg-redef (o <jg-b>)) 'b)
(define d1 (make <jg-d>))
(warm (lambda () (jg-redef d1)))
(test-equal "before redefinition" 'a (hot jg-redef d1))

(define-class <jg-d> (<jg-b>) ())
(test-equal "redefined class is now a jg-b" 'b (hot jg-redef (make <jg-d>)))

;; ---------------------------------------------------------------------------
;; A custom discriminating function installed after compilation, then removed.

(define-generic jg-dfun)
(define-method (jg-dfun (o <jg-b>)) 'native)
(define b2 (make <jg-b>))
(warm (lambda () (jg-dfun b2)))

(test-assert "set-funcallable-instance-function! flips a hot call"
  (flip-ok? (flip-trace (lambda () (jg-dfun b2))
                        (lambda ()
                          (set-funcallable-instance-function! jg-dfun
                                                              (lambda (o) 'custom))))
            'native 'custom))

(test-assert "restoring native dispatch flips back"
  (flip-ok? (flip-trace (lambda () (jg-dfun b2))
                        (lambda () (set-funcallable-instance-function! jg-dfun #f)))
            'custom 'native))

;; ---------------------------------------------------------------------------
;; Two arguments, a few shapes: the new method takes the mixed (b a) shape.

(define-generic jg-pair)
(define-method (jg-pair (x <jg-a>) (y <jg-a>)) 'aa)
(define-method (jg-pair (x <jg-b>) (y <jg-b>)) 'bb)
(define pa a1)
(define pb b1)
(define (pairs) (list (jg-pair pa pa) (jg-pair pb pb) (jg-pair pb pa)))
(warm pairs)

(test-assert "add-method! on the second argument flips a shape"
  (flip-ok? (flip-trace pairs
                        (lambda ()
                          (add-method! jg-pair (list <jg-b> <jg-a>) 2
                                       (lambda (next x y) 'ba)
                                       #f #f #f #f '())))
            '(aa bb aa) '(aa bb ba)))

;; ---------------------------------------------------------------------------
;; Many shapes: more cached classes than the class-test arms hold.

(define-class <jg-k1> (<jg-a>) ())
(define-class <jg-k2> (<jg-a>) ())
(define-class <jg-k3> (<jg-a>) ())
(define-class <jg-k4> (<jg-a>) ())
(define-class <jg-k5> (<jg-a>) ())
(define-class <jg-k6> (<jg-a>) ())
(define-generic jg-many)
(define-method (jg-many (o <jg-a>)) 'a)
(define ks (list (make <jg-k1>) (make <jg-k2>) (make <jg-k3>)
                 (make <jg-k4>) (make <jg-k5>) (make <jg-k6>)))
(define (many) (map jg-many ks))
(warm many)

(test-assert "add-method! flips one of six shapes"
  (flip-ok? (flip-trace many
                        (lambda ()
                          (add-method! jg-many (list <jg-k6>) 1
                                       (lambda (next o) 'k6)
                                       #f #f #f #f '())))
            '(a a a a a a) '(a a a a a k6)))

;; ---------------------------------------------------------------------------
;; An eql method added after the fixnum method was compiled.

(define-generic jg-eql)
(define-method (jg-eql (x <fixnum>)) 'fix)
(warm (lambda () (jg-eql 1)))

(test-assert "an eql method flips a call"
  (flip-ok? (flip-trace (lambda () (jg-eql 0))
                        (lambda ()
                          (add-method! jg-eql (list (intern-eql-specializer 0)) 1
                                       (lambda (next x) 'zero)
                                       #f #f #f #f '())))
            'fix 'zero))

;; ---------------------------------------------------------------------------
;; Slot readers: a hot reader must see a slot write between calls, a reader

(define-class <js-a> () ((x #:init-value 1 #:accessor js-x)))
(define-class <js-b> (<js-a>) ())
(define js-b1 (make <js-b>))
(warm (lambda () (js-x js-b1)))
(test-equal "reader before any write" 1 (hot js-x js-b1))

(test-assert "a slot write flips a hot read"
  (flip-ok? (flip-trace (lambda () (js-x js-b1))
                        (lambda () (slot-set! js-b1 'x 2)))
            1 2))

(test-assert "a reader method for the subclass flips a hot read"
  (flip-ok? (flip-trace (lambda () (js-x js-b1))
                        (lambda ()
                          (add-method! js-x (list <js-b>) 1
                                       (lambda (next o) 'over)
                                       #f #f #f #f '())))
            2 'over))

(define-class <js-r> () ((x #:init-value 1 #:accessor js-r-x)))
(define js-r1 (make <js-r>))
(warm (lambda () (js-r-x js-r1)))

(test-assert "redefinition that moves the slot keeps the value by name"
  (flip-ok? (flip-trace (lambda () (js-r-x js-r1))
                        (lambda () (redefine-class! <js-r> 'js-r-v2 '(y x))))
            1 1))

;; ---------------------------------------------------------------------------
;; A next-method chain: removing the middle method of a hot chain ( deferred).

(define-generic jg-chain)
(define-method (jg-chain (o <jg-a>)) 'a)
(define-method (jg-chain (o <jg-b>)) (list 'b (next-method)))
(define-method (jg-chain (o <jg-c>)) (list 'c (next-method)))
(warm (lambda () (jg-chain c1)))
(test-equal "a next-method chain" '(c (b a)) (hot jg-chain c1))

(test-assert "remove-method! breaks a hot chain at the middle"
  (flip-ok? (flip-trace (lambda () (jg-chain c1))
                        (lambda () (remove-method! jg-chain (method-for jg-chain <jg-b>))))
            '(c (b a)) '(c a)))

;; ---------------------------------------------------------------------------
;; Sealing a class a hot generic dispatches on changes no answer.

(define-class <jg-sealme> (<jg-a>) ())
(define-generic jg-sealed)
(define-method (jg-sealed (o <jg-a>)) 'a)
(define-method (jg-sealed (o <jg-sealme>)) 'sealme)
(define sm1 (make <jg-sealme>))
(warm (lambda () (jg-sealed sm1)))

(test-assert "class-seal! mid-loop keeps every answer"
  (equal? (flip-trace (lambda () (jg-sealed sm1))
                      (lambda () (class-seal! <jg-sealme>)))
          (map (lambda (i) 'sealme) (iota-list 300))))

;; ---------------------------------------------------------------------------
;; A generic named by a global, called where its result feeds `+`: the call's

(define-class <jg-g> () ())
(define-class <jg-h> (<jg-g>) ())
(define-generic jg-glob-a)
(define-method (jg-glob-a (o <jg-g>)) 1)
(define-generic jg-glob-b)
(define-method (jg-glob-b (o <jg-g>)) 2)
(define-generic jg-glob-c)
(define-method (jg-glob-c (o <jg-g>)) 3)
(define jg-glob jg-glob-a)
(define g1 (make <jg-g>))
(define h1 (make <jg-h>))

;; The results of 300 calls through the global, with `flip` run before call 150.
(define (global-trace obj flip)
  (let loop ([i 0] [acc '()])
    (cond [(= i 300) (reverse acc)]
          [else
           (when (= i 150) (flip))
           (loop (+ i 1) (cons (+ 0 (jg-glob obj)) acc))])))

(define (global-ok? trace before after)
  (equal? trace (map (lambda (i) (if (< i 150) before after)) (iota-list 300))))

(warm (lambda () (global-trace g1 (lambda () #f))))

(test-assert "rebinding the global mid-loop flips the callee"
  (global-ok? (global-trace g1 (lambda () (set! jg-glob jg-glob-b))) 1 2))

(test-assert "rebinding back to the first generic flips it again"
  (begin
    (set! jg-glob jg-glob-a)
    (global-ok? (global-trace g1 (lambda () (set! jg-glob jg-glob-b))) 1 2)))

(set! jg-glob jg-glob-a)
(warm (lambda () (global-trace h1 (lambda () #f))))

(test-assert "add-method! on a subclass flips a call through the global"
  (global-ok? (global-trace h1 (lambda ()
                                 (add-method! jg-glob-a (list <jg-h>) 1
                                              (lambda (next o) 5)
                                              #f #f #f #f '())))
              1 5))

(test-assert "remove-method! on that subclass flips it back"
  (global-ok? (global-trace h1 (lambda ()
                                 (remove-method! jg-glob-a
                                                 (method-for jg-glob-a <jg-h>))))
              5 1))

(test-assert "a custom dfun behind the global flips the call"
  (begin
    (set! jg-glob jg-glob-c)
    (warm (lambda () (global-trace g1 (lambda () #f))))
    (global-ok? (global-trace g1 (lambda ()
                                   (set-funcallable-instance-function! jg-glob-c
                                                                       (lambda (o) 7))))
                3 7)))

;; ---------------------------------------------------------------------------
;;: an unbound slot. Unbinding makes the hot read raise; binding it again
;; returns the new value.

(define-class <js-u> () ((x #:init-value 4 #:accessor js-u-x)))
(define js-u1 (make <js-u>))
(warm (lambda () (js-u-x js-u1)))

(test-assert "slot-unbind! flips a hot read to the unbound error"
  (flip-ok? (flip-trace (lambda () (guard (e (#t 'unbound)) (js-u-x js-u1)))
                        (lambda () (slot-unbind! js-u1 'x)))
            4 'unbound))

(test-assert "slot-set! after the unbind flips the read back"
  (flip-ok? (flip-trace (lambda () (guard (e (#t 'unbound)) (js-u-x js-u1)))
                        (lambda () (slot-set! js-u1 'x 5)))
            'unbound 5))

;; ---------------------------------------------------------------------------
;; and the slot protocol: a slot-ref-using-class method for a user
;; metaclass installed after the reader was compiled must be seen.

(define-class <jg-meta> (<class>) ())
(define-class <js-m> () ((x #:init-value 1 #:accessor js-m-x)) #:metaclass <jg-meta>)
(define js-m1 (make <js-m>))
(warm (lambda () (js-m-x js-m1)))
(test-equal "metaclass reader before a protocol method" 1 (js-m-x js-m1))

(test-assert "a slot-ref-using-class method for the metaclass flips a hot read"
  (flip-ok? (flip-trace (lambda () (js-m-x js-m1))
                        (lambda ()
                          (add-method! slot-ref-using-class (list <jg-meta>) 3
                                       (lambda (next class obj slot . rest) 'protocol)
                                       #f #f #f #f '())))
            1 'protocol))

;; ---------------------------------------------------------------------------
;;: a subclass added after the leaf's call was compiled, with its own
;; method. The leaf's answers stay put, and the subclass answers its method.

(define-class <jg-p> () ())
(define-class <jg-q> (<jg-p>) ())
(define-generic jg-cha)
(define-method (jg-cha (o <jg-p>)) 'p)
(define q1 (make <jg-q>))
(warm (lambda () (jg-cha q1)))
(test-equal "the leaf before any subclass" 'p (hot jg-cha q1))

(define-class <jg-r> (<jg-q>) ())
(define r1 (make <jg-r>))

(test-assert "a method for a new subclass leaves its leaf's hot call alone"
  (flip-ok? (flip-trace (lambda () (jg-cha q1))
                        (lambda ()
                          (add-method! jg-cha (list <jg-r>) 1
                                       (lambda (next o) 'r) #f #f #f #f '())))
            'p 'p))

(test-equal "the new subclass answers its own method" 'r (hot jg-cha r1))

;; ---------------------------------------------------------------------------
;;: eight and nine cached shapes (the chain, then the helper).

(define-class <jg-e1> (<jg-a>) ())
(define-class <jg-e2> (<jg-a>) ())
(define-class <jg-e3> (<jg-a>) ())
(define-class <jg-e4> (<jg-a>) ())
(define-class <jg-e5> (<jg-a>) ())
(define-class <jg-e6> (<jg-a>) ())
(define-class <jg-e7> (<jg-a>) ())
(define-class <jg-e8> (<jg-a>) ())
(define-class <jg-e9> (<jg-a>) ())
(define-generic jg-eight)
(define-method (jg-eight (o <jg-a>)) 'a)
(define es8 (list (make <jg-e1>) (make <jg-e2>) (make <jg-e3>) (make <jg-e4>)
                  (make <jg-e5>) (make <jg-e6>) (make <jg-e7>) (make <jg-e8>)))
(define es9 (append es8 (list (make <jg-e9>))))
(define (eight) (map jg-eight es8))
(define (nine) (map jg-eight es9))
(warm eight)
(warm nine)

(test-assert "add-method! flips one of eight shapes (the chain)"
  (flip-ok? (flip-trace eight
                        (lambda ()
                          (add-method! jg-eight (list <jg-e8>) 1
                                       (lambda (next o) 'e8) #f #f #f #f '())))
            (make-list 8 'a) (append (make-list 7 'a) '(e8))))

(test-assert "add-method! flips one of nine shapes (the helper)"
  (flip-ok? (flip-trace nine
                        (lambda ()
                          (add-method! jg-eight (list <jg-e9>) 1
                                       (lambda (next o) 'e9) #f #f #f #f '())))
            ;; The eight-shape test above left e8 answering e8.
            (append (make-list 7 'a) '(e8 a)) (append (make-list 7 'a) '(e8 e9))))

;; ---------------------------------------------------------------------------
;; with a taking site: the call's values feed `+`, six shapes, one of
;; them changed mid-loop. Only the k6 call changes.

(define-generic jg-six)
(define-method (jg-six (o <jg-a>)) 1)
(define (six-trace flip)
  (let loop ([i 0] [acc '()])
    (cond [(= i 300) (reverse acc)]
          [else
           (when (= i 150) (flip))
           (loop (+ i 1) (cons (+ 0 (jg-six (list-ref ks (modulo i 6)))) acc))])))
(warm (lambda () (six-trace (lambda () #f))))

(test-assert "a taking site over six shapes flips the one changed shape"
  (equal? (six-trace (lambda ()
                       (add-method! jg-six (list <jg-k6>) 1
                                    (lambda (next o) 60) #f #f #f #f '())))
          (map (lambda (i) (if (and (= (modulo i 6) 5) (>= i 150)) 60 1))
               (iota-list 300))))

;; ---------------------------------------------------------------------------
;;: a combined generic. An around method added mid-loop wraps the hot
;; call's effective method, and removing it restores the primary's answer.

(define-generic jg-comb)
(define-method (jg-comb (o <jg-a>)) 'a)
(warm (lambda () (jg-comb b1)))
(test-equal "a combined call without the around method" 'a (hot jg-comb b1))

(test-assert "an around method flips a hot combined call"
  (flip-ok? (flip-trace (lambda () (jg-comb b1))
                        (lambda ()
                          (add-method! jg-comb (list <jg-b>) 1
                                       (lambda (next o)
                                         (list 'around (next-method-invoke next)))
                                       #f #f #f #f '(#:around))))
            'a '(around a)))

(test-assert "removing the around method flips it back"
  (flip-ok? (flip-trace (lambda () (jg-comb b1))
                        (lambda ()
                          (remove-method! jg-comb (method-for jg-comb <jg-b>))))
            '(around a) 'a))

;; ---------------------------------------------------------------------------
;; Sealing: sealing and unsealing a generic or a class mid-loop changes no
;; answer.

(define-generic jg-seal2)
(define-method (jg-seal2 (o <jg-a>)) 'a)
(warm (lambda () (jg-seal2 b1)))

(test-assert "generic-seal! mid-loop keeps every answer"
  (equal? (flip-trace (lambda () (jg-seal2 b1)) (lambda () (generic-seal! jg-seal2)))
          (map (lambda (i) 'a) (iota-list 300))))

(test-assert "generic-unseal! mid-loop keeps every answer"
  (equal? (flip-trace (lambda () (jg-seal2 b1)) (lambda () (generic-unseal! jg-seal2)))
          (map (lambda (i) 'a) (iota-list 300))))

(test-assert "class-unseal! mid-loop keeps every answer"
  (equal? (flip-trace (lambda () (jg-sealed sm1))
                      (lambda () (class-unseal! <jg-sealme>)))
          (map (lambda (i) 'sealme) (iota-list 300))))

;; ---------------------------------------------------------------------------
;; Cold method cache (Tier 2 recompile gap): the first three calls pass

(define-generic jg-cold)
(define-method (jg-cold (o <jg-a>)) 'a)
(define-method (jg-cold (o <number>)) 'n)

(define (cold-site n)
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons (jg-cold (if (< i 3) i a1)) acc)))))

(define cold-expected (append (make-list 3 'n) (make-list 297 'a)))
(test-equal "a cold site answers the same as a warm one" cold-expected (cold-site 300))
(test-equal "a second run of the cold site" cold-expected (cold-site 300))


(define-generic jg-lst)
(%set-generic-combination! jg-lst (find-method-combination 'list) '())
(define-method (jg-lst (o <jg-a>)) #:list 1)
(define-method (jg-lst (o <jg-b>)) #:list 2)
(warm (lambda () (jg-lst b1)))
(test-equal "a combined call before the option change" '(2 1) (hot jg-lst b1))

(test-assert "changing the combination options flips a hot combined call"
  (flip-ok? (flip-trace (lambda () (jg-lst b1))
                        (lambda ()
                          (%set-generic-combination! jg-lst
                                                     (find-method-combination 'list)
                                                     '(#:most-specific-last))))
            '(2 1) '(1 2)))

;; A subclass of the hot leaf with its own around method changes that
;; subclass's answers only: the parent's combined call stays as it was.
(define-class <jg-g> (<jg-b>) ())
(define g1 (make <jg-g>))
(warm (lambda () (jg-comb b1)))

(test-equal "a subclass around leaves the parent's combined call"
  (make-list 300 'a)
  (flip-trace (lambda () (jg-comb b1))
              (lambda ()
                (add-method! jg-comb (list <jg-g>) 1
                             (lambda (next o)
                               (list 'around (next-method-invoke next)))
                             #f #f #f #f '(#:around)))))

(test-equal "the subclass answers its own around method"
  '(around a)
  (hot jg-comb g1))



(define-class <cha-root> () ())
(define-class <cha-mid> (<cha-root>) ())
(define-class <cha-deep> (<cha-mid>) ())
(define-class <cha-mixin> () ())
(define-generic cha-f)
(define-method (cha-f (o <cha-root>)) 'root)
(define-method (cha-f (o <cha-mixin>)) 'mixin)
(define cha-mid1 (make <cha-mid>))
(define cha-deep1 (make <cha-deep>))
(define (cha-both) (list (cha-f cha-mid1) (cha-f cha-deep1)))
(warm (lambda () (cha-f cha-mid1)))
(test-equal "the cached class and its subclass before any change" '(root root) (cha-both))

(define cha-plain #f)
(test-assert "a subclass with no method of its own leaves the answers alone"
  (flip-ok? (flip-trace cha-both
                        (lambda ()
                          (set! cha-plain (make-class 'cha-plain '() (list <cha-mid>)))))
            '(root root) '(root root)))
(test-equal "the new subclass inherits the cached method" 'root (cha-f (make cha-plain)))

(define cha-own #f)
(define cha-own1 #f)
(test-assert "a subclass with its own method, added mid-loop, leaves the cached class alone"
  (flip-ok? (flip-trace cha-both
                        (lambda ()
                          (set! cha-own (make-class 'cha-own '() (list <cha-mid>)))
                          (set! cha-own1 (make cha-own))
                          (add-method! cha-f (list cha-own) 1
                                       (lambda (next o) 'own) #f #f #f #f '())))
            '(root root) '(root root)))
(test-equal "the overriding subclass answers its method" 'own (cha-f cha-own1))

(test-assert "a descendant redefined with a mixin superclass flips its answer"
  (flip-ok? (flip-trace cha-both
                        (lambda ()
                          ;; The mixin comes first, so its method is the more specific.
                          (redefine-class! <cha-deep> 'cha-deep '()
                                           (list <cha-mixin> <cha-mid>))))
            '(root root) '(root mixin)))

(define-class <cha-b> (<cha-mid>) ())
(define cha-b1 (make <cha-b>))
(define-generic cha-g)
(define-method (cha-g (o <cha-root>)) 'root)
(define-method (cha-g (o <cha-mid>)) 'mid)
(define (cha-g-both) (list (cha-g cha-mid1) (cha-g cha-b1)))
(warm (lambda () (cha-g cha-mid1)))
(test-equal "a cached class and its subclass answer the cached method" '(mid mid) (cha-g-both))

(test-assert "remove-method! on the cached class flips it and its subclass"
  (flip-ok? (flip-trace cha-g-both
                        (lambda () (remove-method! cha-g (method-for cha-g <cha-mid>))))
            '(mid mid) '(root root)))

(test-end "boot class mop jit generics")
