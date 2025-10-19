(define-library (rope)
    (import (core records)
            (core optargs))    
    (export 
        *short-leaf*
        *long-leaf*
        rope?
        branch?
        leaf?
        make-rope
        make-leaf
        rope-balance
        rope-concat
        rope-append
        rope-prepent
        rope-insert
        rope-split
        rope-index
        rope-substring
        rope-kill
        rope-length
        write-rope
        rope->list
        rope-map
        rope-find
        rope-rfind)
(begin

(define *short-leaf* (make-parameter 16))
(define *long-leaf* (make-parameter 128))

(define-record-type 
    (<rope> %make-rope rope?)
    (fields 
        (immutable depth rope-depth)
        (immutable length rope-length)))

(define-record-type 
    (<branch> %make-branch branch?)
    (parent <rope>)
    (fields 
        (immutable left branch-left)
        (immutable right branch-right)))

(define-record-type 
    (<leaf> %make-leaf leaf?)
    (parent <rope>)
    (fields
        (immutable string leaf-string)))

(define (branch-weight branch)
    (rope-length (branch-left branch)))

(define (leaf-short? leaf . opts)
    (let-optionals opts ([other #f])
        (>= (*short-leaf*)
            (if other 
                (+ (rope-length leaf) (rope-length other))
                (rope-length leaf)))))

(define (make-leaf string)
    (%make-leaf 
        0 (string-length string) string))


(define (rope-map proc rope)
    (cond 
        [(leaf? rope)
         (proc rope)]
        [(branch? rope)
         (let* ([left (branch-left rope)]
                [right (branch-right rope)])
           (rope-map proc left)
           (rope-map proc right))]))

(define (write-rope rope port)
    (cond 
        [(not port) (call-with-output-string (lambda (p) (write-rope rope p)))]
        [(and (boolean? port) (boolean=? port #t)) (write-rope rope (current-output-port))]
        [else 
            (rope-map 
                (lambda (leaf)
                    (write-string (leaf-string leaf) port))
                rope)]))

(define (rope->list rope)
    (let loop ([r rope] [acc '()])
        (cond 
            [(leaf? r)
             (cons (leaf-string r) acc)]
            [(branch? r)
             (loop (branch-left r)
                   (loop (branch-right r) acc))])))

(define (balance-factor rope)
    (if (leaf? rope)
        0
        (- (rope-depth (branch-left rope))
           (rope-depth (branch-right rope)))))

(define (rotate-left rope)
    (define left (branch-left rope))
    (define right (branch-right rope))
    (rope-concat* 
        (rope-concat left (branch-left right))
        (branch-right right)))

(define (rotate-right rope)
    (define left (branch-left rope))
    (define right (branch-right rope))
    (rope-concat* 
        (branch-left left)
        (rope-concat (branch-right left) right)))
(define (rotate-left-right rope)
    (define left (branch-left rope))
    (define right (branch-right rope))
    (rotate-right 
        (rope-concat* 
            (rotate-left left)
            right)))
(define (rotate-right-left rope)
    (define left (branch-left rope))
    (define right (branch-right rope))
    (rotate-left 
        (rope-concat* 
            left
            (rotate-right right))))

(define (rope-balance rope)
    (cond 
        [(leaf? rope) rope]
        [else 
            (let () (define left (branch-left rope))
            (define right (branch-right rope))
            (define bf (balance-factor rope))
            (cond 
                [(< 1 bf)
                    (rope-balance 
                        (if (negative? (balance-factor left))
                            (rotate-left-right rope)
                            (rotate-right rope)))]
                [(> -1 bf)
                    (rope-balance
                        (if (positive? (balance-factor right))
                            (rotate-right-left rope)
                            (rotate-left rope)))]
                [else rope]))]))

(define (normalize-leaves leaves carry)
   
        (define leaf (car leaves))
        (cond 
            [(and carry (null? leaf))
                (list (make-rope carry))]
            [(null? leaf) '()]
            [carry 
                (append (rope->list (make-rope (string-append (leaf-string carry) (leaf-string leaf))))
                        (normalize-leaves (cdr leaves) #f))]
            [(leaf-short? leaf)
                (normalize-leaves (cdr leaves) (leaf-string leaf))]
            [else 
                (cons leaf (normalize-leaves (cdr leaves) #f))]))

(define (merge-leaves leaves start end)
    (define range (- end start))
    (case range 
        [(1) (list-ref leaves start)]
        [(2) (rope-concat (list-ref leaves start)
                          (list-ref leaves (+ start 1)))]
        [else 
            (let ([mid (+ start (quotient range 2))])
                (rope-concat 
                    (merge-leaves leaves start mid)
                    (merge-leaves leaves mid end)))]))

(define (rope-rebuild rope)
 "Reconstruct a rope from the bottom up.
Doing this occasionally can reduce the number of leaves in a rope,
but it is expensive - O(n)."
    (define leaves (normalize-leaves (rope->list rope) #f))
    (merge-leaves leaves 0 (length leaves)))
(define (rope-concat* left right)
    (%make-branch 
        (+ 1 (max (rope-depth left) (rope-depth right)))
        (+ (rope-length left) (rope-length right))
        left 
        right))

(define (rope-concat left right)
    (rope-balance (rope-concat* left right)))

(define (make-rope source)
    (cond 
        [(rope? source) source]
        [(char? source) 
            (make-rope (string source))]
        [(string? source)
            (let () (define len (string-length source))
            (if (<= (*long-leaf*) len)
                (rope-concat (make-rope (substring source 0 (quotient len 2)))
                             (make-rope (substring source (quotient len 2) len)))
                (make-leaf source)))]))

(define (rope-prepend rope source)
    "Return a new rope with a string or rope inserted at the beginning."
    (cond 
        [(branch? source)
            (rope-concat source rope)]
        [(and (leaf? rope) (leaf? source))
            (if (leaf-short? rope source)
                (make-leaf (string-append (leaf-string source)
                                          (leaf-string rope)))
                (rope-concat* source rope))]
        [(and (branch? rope) (leaf? source))
            (let ([left (branch-left rope)]
                  [right (branch-right rope)])
                (rope-concat* (rope-prepend left source) right))]
        [else 
            (rope-prepend rope (make-rope source))]))

(define (rope-append rope source)
    (cond 
        [(branch? source)
            (rope-concat rope source)]
        [(and (leaf? rope) (leaf? source))
            (if (leaf-short? rope source)
                (make-leaf (string-append (leaf-string rope)
                                          (leaf-string source)))
                (rope-concat* rope source))]
        [(and (branch? rope) (leaf? source))
            (let ([left (branch-left rope)]
                  [right (branch-right rope)])
                (rope-concat* left (rope-append right source)))]
        [else 
            (rope-append rope (make-rope source))]))
(define (rope-split rope index)
    "Return balanced ropes split at index."
    (cond 
        [(leaf? rope)
            (values (make-rope (substring (leaf-string rope) 0 index))
                    (make-rope (substring (leaf-string rope) index)))]
        [(branch? rope)
            (let* ([left (branch-left rope)]
                   [right (branch-right rope)]
                   [weight (branch-weight rope)])
                (cond 
                    [(= index weight) (values left right)]
                    [(< index weight)
                        (receive (before after) (rope-split left index)
                            (values (rope-balance before)
                                    (rope-concat after right)))]
                    [else
                        (receive (before after) (rope-split right (- index weight))
                            (values (rope-concat left before)
                                    (rope-balance after)))]))]))

(define (rope-index rope index)
    "Get a character at specified index"
    (cond 
        [(leaf? rope)
            (string-ref (leaf-string rope) index)]
        [(branch? rope)
            (let* ([left (branch-left rope)]
                   [right (branch-right rope)]
                   [weight (branch-weight rope)])
                (cond 
                    [(< index weight)
                        (rope-index left index)]
                    [else
                        (rope-index right (- index weight))]))]))

(define (rope-insert rope index source)
    "Insert a string or rope at specified index."
    (cond 
        [(zero? index)
            (rope-prepend rope source)]
        [(= index (rope-length rope))
            (rope-append rope source)]
        [else 
            (receive (before after) (rope-split rope index)
                (rope-concat 
                    (rope-append before source)
                    after))]))

(define (rope-substring rope from . maybe-to)
    (define to (if maybe-to (car maybe-to) (rope-length rope)))
    (receive (before after) (rope-split rope to)
        (receive (before2 after) (rope-split before from)
            (write-rope before2 #f))))  

(define (rope-find rope char . opts)
    "Find the first occurrence of char in rope."
    (let-optionals opts ([start 0] [end (rope-length rope)])
        (define range (- end start))
        (let loop ([index start])
            (cond
                [(>= index end) #f]
                [(char=? (rope-index rope index) char) index]
                [else (loop (+ index 1))]))))

(define (rope-rfind rope char . opts)
    "Find the last occurrence of char in rope."
    (let-optionals opts ([start 0] [end (rope-length rope)])
        (define range (- end start))
        (let loop ([index (- end 1)])
            (cond
                [(< index start) #f]
                [(char=? (rope-index rope index) char) index]
                [else (loop (- index 1))]))))

(define (rope-kill rope from . maybe-to)
    "Remove a substring from the rope between from and to."
    (define to (if (null? maybe-to) (+ from 1) (car maybe-to)))
    (receive (before after) (rope-split rope from)
        (receive (before2 after2) (rope-split rope to)
            (rope-concat before after2))))))