;; Enumerations


(define make-enumeration #f)
(define enum-set? #f)
(define enum-set-universe #f)
(define enum-set-indexer #f)
(define enum-set-constructor #f)
(define enum-set->list #f)
(define enum-set-member? #f)
(define enum-set-subset? #f)
(define enum-set=? #f)
(define enum-set-union #f)
(define enum-set-intersection #f)
(define enum-set-complement #f)
(define enum-set-projection #f)
(define enum-set-difference #f)

(let () 
    (define remove-duplicate-symbols
        (lambda (lst)
        (let loop ((lst lst) (ans '()))
            (if (null? lst)
                (reverse ans)
                (if (memq (car lst) ans)
                    (loop (cdr lst) ans)
                    (loop (cdr lst) (cons (car lst) ans)))))))

    (define (make-enum-type 
        universe 
        members
        indexer 
        constructor)
        (tuple 'type:enum-type 
            universe 
            members 
            indexer 
            constructor))

    (define (make-enum-set type members)
        (tuple 'type:enum-set 
            type 
            members))

    (define (enum-type-universe enum-type)
        (tuple-ref enum-type 1))
    
    (define (enum-type-members enum-type)
        (tuple-ref enum-type 2))
    
    (define (enum-type-indexer enum-type)
        (tuple-ref enum-type 3))
    (define (enum-type-constructor enum-type)
        (tuple-ref enum-type 4))

    (define (enum-set-type enum-set)
        (tuple-ref enum-set 1))
    (define (enum-set-members enum-set)
        (tuple-ref enum-set 2))

    (define construct-enum-type
        (lambda (universe symbol-list)
        (make-enum-type universe
                        symbol-list
                        (lambda (set)
                            (lambda (symbol)
                            (core-hash-ref (enum-type-universe (enum-set-type set)) symbol #f)))
                        (lambda (set)
                            (lambda (symbol-list)
                            (let ((lst (remove-duplicate-symbols symbol-list))
                                    (universe (enum-type-universe (enum-set-type set))))
                                (for-each (lambda (e)
                                            (or (symbol? e)
                                                (assertion-violation "enum-set constructor" (format "expected list of symbols, but got ~r as argument 1" symbol-list)))
                                            (or (core-hash-ref universe e #f)
                                                (assertion-violation "enum-set constructor" (format "excpectd symbols which belong to the universe, but got ~r as argument 1" symbol-list))))
                                        lst)
                                (make-enum-set (enum-set-type set) lst)))))))

    (set! enum-set? 
        (lambda (obj)
            (and (tuple? obj)
                (> (tuple-size obj) 0)
                (eq? (tuple-ref obj 0) 'type:enum-set))))

    (set! make-enumeration
        (lambda (symbol-list)
            (let ((symbol-list (remove-duplicate-symbols symbol-list)))
                (let ((ht (make-core-hashtable)) (index 0))
                    (for-each 
                        (lambda (e)
                            (or (symbol? e)
                                (assertion-violation 'make-enumeration (format "expected list of symbols, but got ~r as argument 1" symbol-list)))
                            (core-hash-set! ht e index)
                            (set! index (+ index 1)))
                        symbol-list)
                    (let ((type (construct-enum-type ht symbol-list)))
                        (make-enum-set type symbol-list))))))

    (set! enum-set-universe
        (lambda (set)
            (or (enum-set? set)
                (assertion-violation 'enum-set-universe (format "expected enum-set, but got ~r as argument 1" set)))
            (make-enum-set (enum-set-type set)
                (enum-type-members (enum-set-type set)))))

    (set! enum-set-indexer
        (lambda (set)
            (or (enum-set? set)
                (assertion-violation 'enum-set-indexer (format "expected enum-set, but got ~r as argument 1" set)))
            ((enum-type-indexer (enum-set-type set)) set)))

    (set! enum-set-constructor
        (lambda (set)
            (or (enum-set? set)
                (assertion-violation 'enum-set-constructor (format "expected enum-set, but got ~r as argument 1" set)))
            ((enum-type-constructor (enum-set-type set)) set)))

    (set! enum-set->list
        (lambda (set)
            (or (enum-set? set)
                (assertion-violation 'enum-set->list (format "expected enum-set, but got ~r as argument 1" set)))
            (let ((universe (enum-type-universe (enum-set-type set))))
                (map car
                    (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                        (map (lambda (e) (cons e (core-hash-ref universe e #f)))
                            (enum-set-members set)))))))

    (set! enum-set-member?
        (lambda (symbol set)
            (or (symbol? symbol)
                (assertion-violation 'enum-set-member? (format "expected enum-set, but got ~r as argument 1" symbol) (list symbol set)))
            (or (enum-set? set)
                (assertion-violation 'enum-set-member? (format "expected enum-set, but got ~r as argument 2" set) (list symbol set)))
            (and (memq symbol (enum-set-members set)) #t)))

    (set! enum-set-subset?
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set-subset? (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set-subset? (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (and (for-all (lambda (e) (enum-set-member? e set2)) (enum-set-members set1))
                (let ((m2 (enum-type-members (enum-set-type set2))))
                    (for-all (lambda (e) (memq e m2)) (enum-type-members (enum-set-type set1))))
                #t)))

    (set! enum-set=?
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set=? (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set=? (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (and (enum-set-subset? set2 set1)
                (enum-set-subset? set1 set2)
                #t)))

    (set! enum-set-union
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set-union (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set-union (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (or (eq? (enum-set-type set1) (enum-set-type set2))
                (assertion-violation 'enum-set-union "expected same type enum-sets" set1 set2))
            (make-enum-set (enum-set-type set1)
                (remove-duplicate-symbols (append (enum-set-members set1) (enum-set-members set2))))))

    (set! enum-set-intersection
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set-intersection (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set-intersection (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (or (eq? (enum-set-type set1) (enum-set-type set2))
                (assertion-violation 'enum-set-intersection "expected same type enum-sets" set1 set2))
            (let ((set2-members (enum-set-members set2)))
                (make-enum-set (enum-set-type set1)
                    (filter values (map (lambda (e) (and (memq e set2-members) e))
                                            (enum-set-members set1)))))))

    (set! enum-set-difference
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set-difference (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set-difference (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (or (eq? (enum-set-type set1) (enum-set-type set2))
                (assertion-violation 'enum-set-difference "expected same type enum-sets" set1 set2))
            (let ((set2-members (enum-set-members set2)))
                (make-enum-set (enum-set-type set1)
                                (filter values (map (lambda (e) (and (not (memq e set2-members)) e))
                                (enum-set-members set1)))))))

    (set! enum-set-complement
        (lambda (set)
        (or (enum-set? set)
            (assertion-violation 'enum-set-complement (format "expected enum-set, but got ~r as argument 1" set)))
        (let ((set-members (enum-set-members set)))
            (make-enum-set (enum-set-type set)
                        (filter values (map (lambda (e) (and (not (memq e set-members)) e))
                                            (enum-type-members (enum-set-type set))))))))

    (set! enum-set-projection
        (lambda (set1 set2)
            (or (enum-set? set1)
                (assertion-violation 'enum-set-projection (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
            (or (enum-set? set2)
                (assertion-violation 'enum-set-projection (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
            (let ((set2-universe-members (enum-type-members (enum-set-type set2))))
                (make-enum-set (enum-set-type set2)
                            (filter values (map (lambda (e) (and (memq e set2-universe-members) e))
                                                (enum-set-members set1)))))))

)