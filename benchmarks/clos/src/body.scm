
(define (m x) (modulo x 1000003))
(define (mix a b) (modulo (+ (* a 31) b) 1000003))

(define seed 12345)
(define (rand! n)
  (set! seed (modulo (* seed 48271) 2147483647))
  (modulo seed n))

;;; ------------------------------------------------------------------
;;; 1. Slot reads: generic accessors vs. slot-ref
;;; ------------------------------------------------------------------
(define-class <vec3> ()
  (x #:init-keyword #:x #:init-value 0 #:accessor vec-x)
  (y #:init-keyword #:y #:init-value 0 #:accessor vec-y)
  (z #:init-keyword #:z #:init-value 0 #:accessor vec-z))

(define (make-vecs n)
  (let ([v (make-vector n #f)])
    (do ([i 0 (+ i 1)]) ((= i n) v)
      (vector-set! v i (make <vec3> #:x i #:y (* 2 i) #:z (* 3 i))))))

(define vecs (make-vecs 1024))

(define (bench-accessor-read)
  (let outer ([r 0] [sum 0])
    (if (= r 6000)
        (m sum)
        (outer (+ r 1)
               (let loop ([i 0] [s sum])
                 (if (= i 1024)
                     s
                     (let ([v (vector-ref vecs i)])
                       (loop (+ i 1) (+ s (vec-x v) (vec-y v) (vec-z v))))))))))

(define (bench-slot-ref-read)
  (let outer ([r 0] [sum 0])
    (if (= r 3000)
        (m sum)
        (outer (+ r 1)
               (let loop ([i 0] [s sum])
                 (if (= i 1024)
                     s
                     (let ([v (vector-ref vecs i)])
                       (loop (+ i 1)
                             (+ s (slot-ref v 'x) (slot-ref v 'y) (slot-ref v 'z))))))))))

;;; ------------------------------------------------------------------
;;; 2. Slot writes: particle simulation with bouncing walls
;;; ------------------------------------------------------------------
(define-class <particle> ()
  (x  #:init-keyword #:x  #:init-value 0 #:accessor p-x)
  (y  #:init-keyword #:y  #:init-value 0 #:accessor p-y)
  (vx #:init-keyword #:vx #:init-value 0 #:accessor p-vx)
  (vy #:init-keyword #:vy #:init-value 0 #:accessor p-vy))

(define (step-particle! p)
  (let ([x (+ (p-x p) (p-vx p))]
        [y (+ (p-y p) (p-vy p))])
    (when (or (< x 0) (> x 10000)) (slot-set! p 'vx (- (p-vx p))))
    (when (or (< y 0) (> y 10000)) (slot-set! p 'vy (- (p-vy p))))
    (slot-set! p 'x x)
    (slot-set! p 'y y)))

(define (bench-particles)
  (set! seed 777)
  (let ([ps (make-vector 512 #f)])
    (do ([i 0 (+ i 1)]) ((= i 512))
      (let* ([x (rand! 10000)] [y (rand! 10000)]
             [vx (- (rand! 41) 20)] [vy (- (rand! 41) 20)])
        (vector-set! ps i (make <particle> #:x x #:y y #:vx vx #:vy vy))))
    (do ([t 0 (+ t 1)]) ((= t 4000))
      (do ([i 0 (+ i 1)]) ((= i 512))
        (step-particle! (vector-ref ps i))))
    (let loop ([i 0] [s 0])
      (if (= i 512)
          (m s)
          (loop (+ i 1) (+ s (p-x (vector-ref ps i)) (* 3 (p-y (vector-ref ps i)))))))))

;;; ------------------------------------------------------------------
;;; 3. Single dispatch over a class hierarchy (square inherits from rect)
;;; ------------------------------------------------------------------
(define-class <shape> () (name #:init-value 0))
(define-class <circle> (<shape>) (r #:init-keyword #:r #:accessor circle-r))
(define-class <rect> (<shape>)
  (w #:init-keyword #:w #:accessor rect-w)
  (h #:init-keyword #:h #:accessor rect-h))
(define-class <square> (<rect>))
(define-class <tri> (<shape>)
  (b #:init-keyword #:b #:accessor tri-b)
  (h #:init-keyword #:h #:accessor tri-h))

(define-generic area)
(define-generic perimeter)
(define-method (area (s <circle>)) (* 3 (circle-r s) (circle-r s)))
(define-method (area (s <rect>)) (* (rect-w s) (rect-h s)))
(define-method (area (s <tri>)) (quotient (* (tri-b s) (tri-h s)) 2))
(define-method (perimeter (s <circle>)) (* 6 (circle-r s)))
(define-method (perimeter (s <rect>)) (* 2 (+ (rect-w s) (rect-h s))))
(define-method (perimeter (s <square>)) (* 4 (rect-w s)))
(define-method (perimeter (s <tri>)) (+ (tri-b s) (tri-h s) (tri-b s)))

(define (make-shapes n)
  (set! seed 99)
  (let ([v (make-vector n #f)])
    (do ([i 0 (+ i 1)]) ((= i n) v)
      (vector-set!
       v i
       (case (modulo i 4)
         [(0) (make <circle> #:r (+ 1 (rand! 50)))]
         [(1) (let* ([w (+ 1 (rand! 50))] [h (+ 1 (rand! 50))]) (make <rect> #:w w #:h h))]
         [(2) (make <square> #:w (+ 1 (rand! 50)) #:h 1)]
         [else (let* ([b (+ 1 (rand! 50))] [h (+ 1 (rand! 50))]) (make <tri> #:b b #:h h))])))))

(define shapes (make-shapes 1024))

(define (bench-shapes)
  (let outer ([r 0] [sum 0])
    (if (= r 3000)
        (m sum)
        (outer (+ r 1)
               (let loop ([i 0] [s sum])
                 (if (= i 1024)
                     s
                     (let ([sh (vector-ref shapes i)])
                       (loop (+ i 1) (+ s (area sh) (perimeter sh))))))))))

;;; ------------------------------------------------------------------
;;; 4. Multi-methods: collision table with fallbacks and specificity
;;; ------------------------------------------------------------------
(define-class <entity> () (hp #:init-keyword #:hp #:init-value 1 #:accessor hp))
(define-class <asteroid> (<entity>))
(define-class <ship> (<entity>))
(define-class <bullet> (<entity>))
(define-class <shield> (<entity>))

(define-generic collide)
(define-method (collide (a <entity>) (b <entity>)) 1)
(define-method (collide (a <asteroid>) (b <asteroid>)) 2)
(define-method (collide (a <asteroid>) (b <ship>)) (+ 3 (hp a)))
(define-method (collide (a <ship>) (b <asteroid>)) (+ 5 (hp b)))
(define-method (collide (a <bullet>) (b <asteroid>)) 7)
(define-method (collide (a <bullet>) (b <ship>)) 11)
(define-method (collide (a <ship>) (b <shield>)) 13)
(define-method (collide (a <shield>) (b <entity>)) (+ 17 (hp b)))
(define-method (collide (a <entity>) (b <bullet>)) (+ 19 (hp a)))

(define (make-entities n)
  (let ([v (make-vector n #f)])
    (do ([i 0 (+ i 1)]) ((= i n) v)
      (vector-set!
       v i
       (case (modulo (* i 7) 4)
         [(0) (make <asteroid> #:hp (+ 1 (modulo i 5)))]
         [(1) (make <ship> #:hp (+ 1 (modulo i 3)))]
         [(2) (make <bullet>)]
         [else (make <shield> #:hp (+ 1 (modulo i 7)))])))))

(define entities (make-entities 1024))

(define (bench-collide)
  (let outer ([r 0] [sum 0])
    (if (= r 3000)
        (m sum)
        (outer (+ r 1)
               (let loop ([i 0] [s sum])
                 (if (= i 1024)
                     s
                     (loop (+ i 1)
                           (+ s (collide (vector-ref entities i)
                                         (vector-ref entities (modulo (+ i r 1) 1024)))))))))))

;;; ------------------------------------------------------------------
;;; 5. next-method chains: layered pricing rules
;;; ------------------------------------------------------------------
(define-class <item> () (price #:init-keyword #:price #:accessor item-price))
(define-class <taxed> (<item>))
(define-class <discounted> (<taxed>) (pct #:init-keyword #:pct #:init-value 10 #:accessor item-pct))
(define-class <premium> (<discounted>))

(define-generic cost)
(define-method (cost (o <item>) qty) (* qty (item-price o)))
(define-method (cost (o <taxed>) qty) (let ([c (next-method)]) (+ c (quotient c 10))))
(define-method (cost (o <discounted>) qty)
  (let ([c (next-method)]) (- c (quotient (* c (item-pct o)) 100))))
(define-method (cost (o <premium>) qty) (+ 5 (next-method)))

(define items
  (let ([v (make-vector 1024 #f)])
    (do ([i 0 (+ i 1)]) ((= i 1024) v)
      (vector-set!
       v i
       (case (modulo i 4)
         [(0) (make <item> #:price (+ 10 i))]
         [(1) (make <taxed> #:price (+ 10 i))]
         [(2) (make <discounted> #:price (+ 10 i) #:pct (+ 5 (modulo i 20)))]
         [else (make <premium> #:price (+ 10 i) #:pct 15)])))))

(define (bench-next-method)
  (let outer ([r 0] [sum 0])
    (if (= r 3000)
        (m sum)
        (outer (+ r 1)
               (let loop ([i 0] [s sum])
                 (if (= i 1024)
                     s
                     (loop (+ i 1) (+ s (cost (vector-ref items i) (+ 1 (modulo r 3)))))))))))

;;; ------------------------------------------------------------------
;;; 6. Allocation: binary trees of CLOS instances
;;; ------------------------------------------------------------------
(define-class <tnode> ()
  (left #:init-keyword #:left #:init-value #f #:accessor tnode-left)
  (right #:init-keyword #:right #:init-value #f #:accessor tnode-right)
  (val #:init-keyword #:val #:init-value 0 #:accessor tnode-val))

(define (build-tree d)
  (if (= d 0)
      (make <tnode> #:val 1)
      (let* ([l (build-tree (- d 1))] [r (build-tree (- d 1))])
        (make <tnode> #:left l #:right r #:val d))))

(define-generic tree-sum)
(define-method (tree-sum (n <boolean>)) 0)
(define-method (tree-sum (n <tnode>))
  (+ (tnode-val n) (tree-sum (tnode-left n)) (tree-sum (tnode-right n))))

(define (bench-alloc)
  (let loop ([i 0] [s 0])
    (if (= i 60)
        (m s)
        (loop (+ i 1) (+ s (tree-sum (build-tree 14)))))))

;;; ------------------------------------------------------------------
;;; 7. AST interpreter: closure-free tree walker with call frames
;;; ------------------------------------------------------------------
(define-class <node> ())
(define-class <lit> (<node>) (val #:init-keyword #:val #:accessor lit-val))
(define-class <ref> (<node>) (idx #:init-keyword #:idx #:accessor ref-idx))
(define-class <setl> (<node>)
  (idx #:init-keyword #:idx #:accessor setl-idx)
  (expr #:init-keyword #:expr #:accessor setl-expr))
(define-class <binop> (<node>)
  (a #:init-keyword #:a #:accessor bin-a)
  (b #:init-keyword #:b #:accessor bin-b))
(define-class <add> (<binop>))
(define-class <sub> (<binop>))
(define-class <mul> (<binop>))
(define-class <lt> (<binop>))
(define-class <seq> (<binop>))
(define-class <iff> (<node>)
  (c #:init-keyword #:c #:accessor iff-c)
  (t #:init-keyword #:t #:accessor iff-t)
  (e #:init-keyword #:e #:accessor iff-e))
(define-class <while> (<node>)
  (c #:init-keyword #:c #:accessor while-c)
  (body #:init-keyword #:body #:accessor while-body))
(define-class <fn> ()
  (nlocals #:init-keyword #:nlocals #:accessor fn-nlocals)
  (body #:init-value #f #:accessor fn-body))
(define-class <call> (<node>)
  (fn #:init-keyword #:fn #:init-value #f #:accessor call-fn)
  (args #:init-keyword #:args #:accessor call-args))

(define-generic ev)
(define-method (ev (n <lit>) fr) (lit-val n))
(define-method (ev (n <ref>) fr) (vector-ref fr (ref-idx n)))
(define-method (ev (n <setl>) fr)
  (let ([v (ev (setl-expr n) fr)]) (vector-set! fr (setl-idx n) v) v))
(define-method (ev (n <add>) fr) (m (+ (ev (bin-a n) fr) (ev (bin-b n) fr))))
(define-method (ev (n <sub>) fr) (m (- (ev (bin-a n) fr) (ev (bin-b n) fr))))
(define-method (ev (n <mul>) fr) (m (* (ev (bin-a n) fr) (ev (bin-b n) fr))))
(define-method (ev (n <lt>) fr) (if (< (ev (bin-a n) fr) (ev (bin-b n) fr)) 1 0))
(define-method (ev (n <seq>) fr) (ev (bin-a n) fr) (ev (bin-b n) fr))
(define-method (ev (n <iff>) fr)
  (if (= 0 (ev (iff-c n) fr)) (ev (iff-e n) fr) (ev (iff-t n) fr)))
(define-method (ev (n <while>) fr)
  (let loop ()
    (if (= 0 (ev (while-c n) fr))
        0
        (begin (ev (while-body n) fr) (loop)))))
(define-method (ev (n <call>) fr)
  (let* ([f (call-fn n)]
         [args (call-args n)]
         [new (make-vector (fn-nlocals f) 0)])
    (do ([i 0 (+ i 1)]) ((= i (vector-length args)))
      (vector-set! new i (ev (vector-ref args i) fr)))
    (ev (fn-body f) new)))

(define (make-fib)
  (let* ([f (make <fn> #:nlocals 1)]
         [call (lambda (k)
                 (make <call> #:fn f
                       #:args (vector (make <sub> #:a (make <ref> #:idx 0) #:b (make <lit> #:val k)))))])
    (slot-set! f 'body
               (make <iff>
                 #:c (make <lt> #:a (make <ref> #:idx 0) #:b (make <lit> #:val 2))
                 #:t (make <ref> #:idx 0)
                 #:e (make <add> #:a (call 1) #:b (call 2))))
    f))

;; sum = 0; i = 0; while (i < n) { sum = sum + i*i; i = i + 1 } sum
(define (make-sum-loop n)
  (let ([f (make <fn> #:nlocals 3)])
    (slot-set! f 'body
      (make <seq>
        #:a (make <while>
              #:c (make <lt> #:a (make <ref> #:idx 0) #:b (make <lit> #:val n))
              #:body
              (make <seq>
                #:a (make <setl> #:idx 1
                          #:expr (make <add> #:a (make <ref> #:idx 1)
                                       #:b (make <mul> #:a (make <ref> #:idx 0) #:b (make <ref> #:idx 0))))
                #:b (make <setl> #:idx 0 #:expr (make <add> #:a (make <ref> #:idx 0) #:b (make <lit> #:val 1)))))
        #:b (make <ref> #:idx 1)))
    f))

(define fib-fn (make-fib))
(define loop-fn (make-sum-loop 800000))

(define (bench-ast-eval)
  (mix (ev (fn-body fib-fn) (vector 27))
       (ev (fn-body loop-fn) (make-vector 3 0))))

;;; ------------------------------------------------------------------
;;; 8. AST optimiser: constant folding via multi-methods + size pass
;;; ------------------------------------------------------------------
(define (gen d)
  (if (= d 0)
      (if (< (rand! 10) 6) (make <lit> #:val (rand! 4)) (make <ref> #:idx (rand! 3)))
      (let* ([a (gen (- d 1))] [b (gen (- d 1))])
        (case (rand! 3)
          [(0) (make <add> #:a a #:b b)]
          [(1) (make <sub> #:a a #:b b)]
          [else (make <mul> #:a a #:b b)]))))

(define-generic mk-add)
(define-generic mk-sub)
(define-generic mk-mul)
(define-method (mk-add (a <lit>) (b <lit>)) (make <lit> #:val (m (+ (lit-val a) (lit-val b)))))
(define-method (mk-add (a <lit>) (b <node>))
  (if (= 0 (lit-val a)) b (make <add> #:a a #:b b)))
(define-method (mk-add (a <node>) (b <lit>))
  (if (= 0 (lit-val b)) a (make <add> #:a a #:b b)))
(define-method (mk-add (a <node>) (b <node>)) (make <add> #:a a #:b b))
(define-method (mk-sub (a <lit>) (b <lit>)) (make <lit> #:val (m (- (lit-val a) (lit-val b)))))
(define-method (mk-sub (a <node>) (b <lit>))
  (if (= 0 (lit-val b)) a (make <sub> #:a a #:b b)))
(define-method (mk-sub (a <node>) (b <node>)) (make <sub> #:a a #:b b))
(define-method (mk-mul (a <lit>) (b <lit>)) (make <lit> #:val (m (* (lit-val a) (lit-val b)))))
(define-method (mk-mul (a <lit>) (b <node>))
  (case (lit-val a) [(0) a] [(1) b] [else (make <mul> #:a a #:b b)]))
(define-method (mk-mul (a <node>) (b <lit>))
  (case (lit-val b) [(0) b] [(1) a] [else (make <mul> #:a a #:b b)]))
(define-method (mk-mul (a <node>) (b <node>)) (make <mul> #:a a #:b b))

(define-generic simplify)
(define-method (simplify (n <lit>)) n)
(define-method (simplify (n <ref>)) n)
(define-method (simplify (n <add>)) (mk-add (simplify (bin-a n)) (simplify (bin-b n))))
(define-method (simplify (n <sub>)) (mk-sub (simplify (bin-a n)) (simplify (bin-b n))))
(define-method (simplify (n <mul>)) (mk-mul (simplify (bin-a n)) (simplify (bin-b n))))

(define-generic size)
(define-method (size (n <lit>)) 1)
(define-method (size (n <ref>)) 1)
(define-method (size (n <binop>)) (+ 1 (size (bin-a n)) (size (bin-b n))))

(define (bench-ast-simplify)
  (set! seed 4242)
  (let ([fr (vector 3 5 7)])
    (let loop ([i 0] [s 0])
      (if (= i 100)
          s
          (let* ([t (gen 11)]
                 [u (simplify t)]
                 [v1 (ev t fr)]
                 [v2 (ev u fr)])
            (loop (+ i 1)
                  (mix (mix (mix s (size t)) (size u)) (mix v1 (if (= v1 v2) 0 999)))))))))

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------
(define warmup 2)
(define timed 5)

(define (insert-sorted x l)
  (cond [(null? l) (list x)]
        [(< x (car l)) (cons x l)]
        [else (cons (car l) (insert-sorted x (cdr l)))]))
(define (median l)
  (let loop ([s '()] [l l])
    (if (null? l) (list-ref s (quotient (length s) 2)) (loop (insert-sorted (car l) s) (cdr l)))))

(define (run-bench name thunk)
  (let ([result #f])
    (do ([i 0 (+ i 1)]) ((= i warmup)) (set! result (thunk)))
    (let ([times (let loop ([i 0] [acc '()])
                   (if (= i timed)
                       acc
                       (let* ([t0 (now)] [r (thunk)] [t1 (now)])
                         (set! result r)
                         (loop (+ i 1) (cons (- t1 t0) acc)))))])
      (display "RESULT ") (display name) (display " ")
      (display (median times)) (display " ") (display result)
      (newline))))

(define (go name thunk) (run-bench name thunk))

(go "accessor-read" bench-accessor-read)
(go "slot-ref-read" bench-slot-ref-read)
(go "particles" bench-particles)
(go "shapes" bench-shapes)
(go "collide" bench-collide)
(go "next-method" bench-next-method)
(go "alloc-trees" bench-alloc)
(go "ast-eval" bench-ast-eval)
(go "ast-simplify" bench-ast-simplify)
