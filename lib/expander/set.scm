(define andmap
  (lambda (f . lists)
    (cond ((null? (car lists)) (and))
          ((null? (cdr (car lists)))
           (apply f (map car lists)))
          (else
           (and (apply f (map car lists))
                (apply andmap f (map cdr lists)))))))

(define ormap
    (lambda (f . lists)
    (cond ((null? (car lists)) (or))
          ((null? (cdr (car lists)))
           (apply f (map car lists)))
          (else
           (or (apply f (map car lists))
               (apply ormap f (map cdr lists)))))))

(define (set-member? s x) (pair? (member x s)))
(define (set=? s1 s2)
    (unless (list? s2)
        (assertion-violation 'set=? "expected list" s2))
    (and (andmap (lambda (x) (member x s2)) s1)
         (andmap (lambda (x) (member x s1)) s2)
         #t))

(define (set-subset? s1 s2)
    (and (andmap (lambda (x) (member x s2)) s1)
         #t))

(define (set-proper-subset? s1 s2)
    (unless (list? s2)
        (assertion-violation 'set-proper-subset? "expected list" s2))
    (and (andmap (lambda (x) (member x s2)) s2)
         (ormap  (lambda (x) (not (member x s1))) s2)
         #t))

(define (set-map s f) (map f s))

(define (set-for-each s f) (for-each f s))

(define (set-add s x)
    (if (member x s) s
        (cons x s)))

(define (set-remove s . xs)
    (remove* xs s))

(define (set-clear s) '())

(define (set-union s . sets)
    (foldl (lambda (acc s2)
                (foldl (lambda (acc2 x)
                            (set-add acc2 x))
                        acc
                        s2))
            s
            sets))
(define (set-intersection s . sets)
    (foldl (lambda (s1 x)
        (if (andmap (lambda (s2) (set-member? s2 x)) sets)
            (set-add s1 x)
            s1)) 
        '()
        s))

(define (set-subtract s . sets)
    (foldl (lambda (s1 x)
        (if (not (ormap (lambda (s2) (set-member? s2 x)) sets))
            (set-add s1 x)
            s1))
        '()
        s))

(define (set-empty? s)
    (null? s))

(define (argmax f s)
    (foldl (lambda (best x)
                (if (or (null? best)
                        (> (f x) (f best)))
                    x
                    best))
            #f
            s))