(seq
 (define max (lambda (a x)
  (if  (#%> (lref a) (lref x))  (lref a)  (lref x))))
 (define boolean? (lambda (x) (#%boolean? (lref x))))
 (define alist-cons (lambda (key datum alist)
  (#%cons (#%cons (lref key) (lref datum)) (lref alist))))
 (define flatten (lambda (lst)
  (if
    (#%null? (lref lst))
    (const '())
    (let
        ([first
          (#%car (lref lst))]
        [rest
          (call (toplevel-ref false flatten) (#%cdr (lref lst)))])
      (if
        (#%list? (lref first))
        (#%append (call (toplevel-ref false flatten) (lref first))
         (lref rest))
        (#%cons (lref first) (lref rest)))))))
 (define remove* (lambda (l r)
  (seq
   (if
     (#%not (#%list? (lref l)))
     (call (toplevel-ref false assertion-violation) (const remove*)
      (const expected a list)
      (lref l))
     (const #<undefined>))
   (if
     (#%not (#%list? (lref r)))
     (call (toplevel-ref false assertion-violation) (const remove*)
      (const expected a list)
      (lref r))
     (const #<undefined>))
   (fix
     ([rloop (lambda (r)
       (let
          ([cond-test-tmp
            (#%null? (lref r))])
        (if
          (lref cond-test-tmp)
          (lref r)
          (let
              ([first-r
                (#%car (lref r))])
            (fix
              ([loop (lambda (l-rest)
                (let
                   ([cond-test-tmp
                     (#%null? (lref l-rest))])
                 (if
                   (lref cond-test-tmp)
                   (let
                      ([next
                        (call (lref rloop) (#%cdr (lref r)))])
                    (if
                      (#%eq? (lref next) (#%cdr (lref r)))
                      (lref r)
                      (#%cons (lref first-r) (lref next))))
                   (let
                       ([cond-test-tmp
                         (#%equal? (#%car (lref l-rest))
                          (lref first-r))])
                     (if
                       (lref cond-test-tmp)
                       (call (lref rloop) (#%cdr (lref r)))
                       (call (lref loop) (#%cdr (lref l-rest))))))))])
             (call (lref loop) (lref l)))))))])
    (call (lref rloop) (lref r))))))
 (define member (lambda (key alist)
  (fix
    ([loop (lambda (alist)
      (if
        (#%null? (lref alist))
        (const false)
        (if
           (#%equal? (lref key) (#%car (lref alist)))
           (lref alist)
           (call (lref loop) (#%cdr (lref alist))))))])
   (call (lref loop) (lref alist)))))
 (define memq (lambda (key alist)
  (fix
    ([loop (lambda (alist)
      (if
        (#%null? (lref alist))
        (const false)
        (if
           (#%eq? (lref key) (#%car (lref alist)))
           (lref alist)
           (call (lref loop) (#%cdr (lref alist))))))])
   (call (lref loop) (lref alist)))))
 (define memv (lambda (key alist)
  (fix
    ([loop (lambda (alist)
      (if
        (#%null? (lref alist))
        (const false)
        (if
           (#%eqv? (lref key) (#%car (lref alist)))
           (lref alist)
           (call (lref loop) (#%cdr (lref alist))))))])
   (call (lref loop) (lref alist)))))
 (define assq (lambda (key alist)
  (if
    (#%null? (lref alist))
    (const false)
    (if
       (#%eq? (lref key) (#%car (#%car (lref alist))))
       (#%car (lref alist))
       (call (toplevel-ref false assq) (lref key)
         (#%cdr (lref alist)))))))
 (define alist? (lambda (x)
  (let
     ([and-tmp
       (#%list? (lref x))])
   (if
     (lref and-tmp)
     (call (toplevel-ref false every1) #%pair? (lref x))
     (const false)))))
 (define assoc (lambda (key alist)
  (if
    (#%null? (lref alist))
    (const false)
    (if
       (#%equal? (lref key) (#%car (#%car (lref alist))))
       (#%car (lref alist))
       (call (toplevel-ref false assoc) (lref key)
         (#%cdr (lref alist)))))))
 (define assv (lambda (key alist)
  (if
    (#%null? (lref alist))
    (const false)
    (if
       (#%eqv? (lref key) (#%car (#%car (lref alist))))
       (#%car (lref alist))
       (call (toplevel-ref false assv) (lref key)
         (#%cdr (lref alist)))))))
 (define assq-ref (lambda (alist key)
  (let
     ([pair
       (call (toplevel-ref false assq) (lref key) (lref alist))])
   (if  (#%pair? (lref pair))  (#%cdr (lref pair))  (const false)))))
 (define assoc-ref (lambda (alist key)
  (let
     ([pair
       (call (toplevel-ref false assoc) (lref key) (lref alist))])
   (if  (#%pair? (lref pair))  (#%cdr (lref pair))  (const false)))))
 (define assoc-remove (lambda (alist key)
  (let
     ([cond-test-tmp
       (#%null? (lref alist))])
   (if
     (lref cond-test-tmp)
     (const '())
     (let
         ([cond-test-tmp
           (#%equal? (lref key) (#%car (#%car (lref alist))))])
       (if
         (lref cond-test-tmp)
         (#%cdr (lref alist))
         (#%cons (#%car (lref alist))
           (call (toplevel-ref false assoc-remove) (#%cdr (lref alist))
            (lref key)))))))))
 (define append-map (lambda (proc lst)
  (if
    (#%null? (lref lst))
    (const '())
    (let
        ([res
          (call (lref proc) (#%car (lref lst)))])
      (if
        (#%null? (lref res))
        (call (toplevel-ref false append-map) (lref proc)
         (#%cdr (lref lst)))
        (#%append (lref res)
          (call (toplevel-ref false append-map) (lref proc)
           (#%cdr (lref lst)))))))))
 (define every1 (lambda (pred lst)
  (let
     ([or-tmp
       (#%null? (lref lst))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (fix
        ([loop (lambda (head rest)
          (let
             ([and-tmp
               (call (lref pred) (lref head))])
           (if
             (lref and-tmp)
             (let
                ([or-tmp
                  (#%null? (lref rest))])
              (if
                (lref or-tmp)
                (lref or-tmp)
                (call (lref loop) (#%car (lref rest))
                  (#%cdr (lref rest)))))
             (const false))))])
       (call (lref loop) (#%car (lref lst)) (#%cdr (lref lst))))))))
 (define every2 (lambda (pred lst1 lst2)
  (let
     ([or-tmp
       (#%null? (lref lst1))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (let
         ([or-tmp
           (#%null? (lref lst2))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (fix
            ([loop (lambda (head1 rest1 head2 rest2)
              (let
                 ([and-tmp
                   (call (lref pred) (lref head1) (lref head2))])
               (if
                 (lref and-tmp)
                 (let
                    ([or-tmp
                      (#%null? (lref rest1))])
                  (if
                    (lref or-tmp)
                    (lref or-tmp)
                    (let
                        ([or-tmp
                          (#%null? (lref rest2))])
                      (if
                        (lref or-tmp)
                        (lref or-tmp)
                        (call (lref loop) (#%car (lref rest1))
                          (#%cdr (lref rest1))
                          (#%car (lref rest2))
                          (#%cdr (lref rest2)))))))
                 (const false))))])
           (call (lref loop) (#%car (lref lst1))
            (#%cdr (lref lst1))
            (#%car (lref lst2))
            (#%cdr (lref lst2))))))))))
 (define any1 (lambda (pred lst)
  (if
    (#%null? (lref lst))
    (const false)
    (let
        ([r
          (call (lref pred) (#%car (lref lst)))])
      (if
        (lref r)
        (lref r)
        (call (toplevel-ref false any1) (lref pred)
          (#%cdr (lref lst))))))))
 (define any2 (lambda (pred lst1 lst2)
  (let
     ([and-tmp
       (#%not (#%null? (lref lst1)))])
   (if
     (lref and-tmp)
     (let
        ([and-tmp
          (#%not (#%null? (lref lst2)))])
      (if
        (lref and-tmp)
        (let
           ([or-tmp
             (call (lref pred) (#%car (lref lst1))
              (#%car (lref lst2)))])
         (if
           (lref or-tmp)
           (lref or-tmp)
           (call (toplevel-ref false any2) (lref pred)
             (#%cdr (lref lst1))
             (#%cdr (lref lst2)))))
        (const false)))
     (const false)))))
 (define filter (lambda (pred lst)
  (fix
    ([loop (lambda (lst)
      (let
         ([cond-test-tmp
           (#%null? (lref lst))])
       (if
         (lref cond-test-tmp)
         (const '())
         (let
             ([cond-test-tmp
               (call (lref pred) (#%car (lref lst)))])
           (if
             (lref cond-test-tmp)
             (#%cons (#%car (lref lst))
              (call (lref loop) (#%cdr (lref lst))))
             (call (lref loop) (#%cdr (lref lst))))))))])
   (call (lref loop) (lref lst)))))
 (define partition (lambda (pred lst)
  (fix
    ([loop (lambda (lst acc1 acc2)
      (let
         ([cond-test-tmp
           (#%null? (lref lst))])
       (if
         (lref cond-test-tmp)
         (values (call (toplevel-ref false reverse) (lref acc1)) (call (toplevel-ref false reverse) (lref acc2)))
         (let
             ([cond-test-tmp
               (call (lref pred) (#%car (lref lst)))])
           (if
             (lref cond-test-tmp)
             (call (lref loop) (#%cdr (lref lst))
              (#%cons (#%car (lref lst)) (lref acc1))
              (lref acc2))
             (call (lref loop) (#%cdr (lref lst))
               (lref acc1)
               (#%cons (#%car (lref lst)) (lref acc2))))))))])
   (call (lref loop) (lref lst) (const '()) (const '())))))
 (define for-all (lambda (pred lst)
  (if
    (#%null? (lref lst))
    (const true)
    (let
        ([and-tmp
          (call (lref pred) (#%car (lref lst)))])
      (if
        (lref and-tmp)
        (call (toplevel-ref false for-all) (lref pred)
         (#%cdr (lref lst)))
        (const false))))))
 (define safe-length (lambda (lst)
  (fix
    ([loop (lambda (lst n)
      (if
        (#%pair? (lref lst))
        (call (lref loop) (#%cdr (lref lst)) (#%+ (lref n) (const 1)))
        (let
            ([or-tmp
              (let
                 ([and-tmp
                   (#%null? (lref lst))])
               (if  (lref and-tmp)  (lref n)  (const false)))])
          (if  (lref or-tmp)  (lref or-tmp)  (const -1)))))])
   (call (lref loop) (lref lst) (const 0)))))
 (define split-at (lambda (lst n)
  (values (call (toplevel-ref false take) (lref lst)
   (lref n)) (call (toplevel-ref false drop) (lref lst) (lref n)))))
 (define map (lambda (f x rest)
  (fix
    ([map1 (lambda (f x)
      (if
        (#%pair? (lref x))
        (let
           ([a
             (call (lref f) (#%car (lref x)))])
         (let
            ([b
              (call (lref map1) (lref f) (#%cdr (lref x)))])
          (#%cons (lref a) (lref b))))
        (if
           (#%null? (lref x))
           (const '())
           (call (toplevel-ref false assertion-violation) (const map)
             (const expected a proper list)
             (lref x)))))])
   (fix
     ([map2 (lambda (f x y)
       (if
         (let
             ([and-tmp
               (#%pair? (lref x))])
           (if  (lref and-tmp)  (#%pair? (lref y))  (const false)))
         (let
            ([a
              (call (lref f) (#%car (lref x)) (#%car (lref y)))])
          (let
             ([b
               (call (lref map2) (lref f)
                (#%cdr (lref x))
                (#%cdr (lref y)))])
           (#%cons (lref a) (lref b))))
         (if
            (let
                ([or-tmp
                  (#%null? (lref x))])
              (if  (lref or-tmp)  (lref or-tmp)  (#%null? (lref y))))
            (const '())
            (call (toplevel-ref false assertion-violation) (const map)
              (const expected proper lists)
              (#%cons (lref x) (#%cons (lref y) (const '())))))))])
    (fix
      ([map3 (lambda (f x y z)
        (if
          (let
              ([and-tmp
                (#%pair? (lref x))])
            (if
              (lref and-tmp)
              (let
                 ([and-tmp
                   (#%pair? (lref y))])
               (if
                 (lref and-tmp)
                 (#%pair? (lref z))
                 (const false)))
              (const false)))
          (let
             ([a
               (call (lref f) (#%car (lref x))
                (#%car (lref y))
                (#%car (lref z)))])
           (let
              ([b
                (call (lref map3) (lref f)
                 (#%cdr (lref x))
                 (#%cdr (lref y))
                 (#%cdr (lref z)))])
            (#%cons (lref a) (lref b))))
          (if
             (let
                 ([or-tmp
                   (#%null? (lref x))])
               (if
                 (lref or-tmp)
                 (lref or-tmp)
                 (let
                     ([or-tmp
                       (#%null? (lref y))])
                   (if
                     (lref or-tmp)
                     (lref or-tmp)
                     (#%null? (lref z))))))
             (const '())
             (call (toplevel-ref false assertion-violation) (const map)
               (const expected proper lists)
               (#%cons (lref x)
                (#%cons (lref y) (#%cons (lref z) (const '()))))))))])
     (let
        ([l
          (#%length (lref rest))])
      (let
         ([cond-test-tmp
           (#%= (lref l) (const 0))])
       (if
         (lref cond-test-tmp)
         (call (lref map1) (lref f) (lref x))
         (let
             ([cond-test-tmp
               (#%= (lref l) (const 1))])
           (if
             (lref cond-test-tmp)
             (call (lref map2) (lref f) (lref x) (#%car (lref rest)))
             (let
                 ([cond-test-tmp
                   (#%= (lref l) (const 2))])
               (if
                 (lref cond-test-tmp)
                 (call (lref map3) (lref f)
                  (lref x)
                  (#%car (lref rest))
                  (#%car (#%cdr (lref rest))))
                 (const #<undefined>)))))))))))))
 (define for-each (lambda (proc lst)
  (fix
    ([loop (lambda (lst)
      (if
        (#%pair? (lref lst))
        (seq
         (call (lref proc) (#%car (lref lst)))
         (call (lref loop) (#%cdr (lref lst))))
        (if
           (#%null? (lref lst))
           (const '())
           (call (toplevel-ref false assertion-violation) (const for-each)
             (const expected a proper list)
             (lref lst)))))])
   (call (lref loop) (lref lst)))))
 (define and-map (lambda (pred lst)
  (fix
    ([loop (lambda (lst)
      (if
        (#%null? (lref lst))
        (const true)
        (if
           (call (lref pred) (#%car (lref lst)))
           (call (lref loop) (#%cdr (lref lst)))
           (const false))))])
   (call (lref loop) (lref lst)))))
 (define or-map (lambda (pred lst)
  (fix
    ([loop (lambda (lst)
      (if
        (#%null? (lref lst))
        (const false)
        (if
           (call (lref pred) (#%car (lref lst)))
           (const true)
           (call (lref loop) (#%cdr (lref lst))))))])
   (call (lref loop) (lref lst)))))
 (define every? (lambda (pred lst)
  (let
     ([and-tmp
       (#%list? (lref lst))])
   (if
     (lref and-tmp)
     (call (toplevel-ref false every1) (lref pred) (lref lst))
     (const false)))))
 (define make-parameter (lambda (init converter)
  (let
     ([f
       (call (toplevel-ref false make-fluid) (lref init))]
     [conv
       (if
         (#%null? (lref converter))
         (lambda (x) (lref x))
         (#%car (lref converter)))])
   (lambda args
    (if
      (#%null? (lref args))
      (call (toplevel-ref false fluid-ref) (lref f))
      (let
          ([old
            (call (toplevel-ref false fluid-ref) (lref f))])
        (seq
         (if
           (#%not (call (lref conv) (#%car (lref args))))
           (call (toplevel-ref false assertion-violation) (const parameter)
            (const bad value for parameter)
            (#%car (lref args)))
           (const #<undefined>))
         (call (toplevel-ref false fluid-set!) (lref f)
          (#%car (lref args)))
         (lref old))))))))
 (define tuple-printer (call (toplevel-ref false make-parameter) (const false)))
 (define make-rtd (lambda (name parent uid sealed? opaque? fields)
  (let
     ([tup
       (#%make-tuple (const 8) (const #<undefined>))])
   (seq
    (#%tuple-set! (lref tup)
     (const 0)
     (const type:record-type-descriptor))
    (#%tuple-set! (lref tup) (const 1) (lref name))
    (#%tuple-set! (lref tup) (const 2) (lref parent))
    (#%tuple-set! (lref tup) (const 3) (lref uid))
    (#%tuple-set! (lref tup) (const 4) (lref sealed?))
    (#%tuple-set! (lref tup) (const 5) (lref opaque?))
    (#%tuple-set! (lref tup) (const 6) (lref fields))
    (#%tuple-set! (lref tup) (const 7) (const false))
    (lref tup)))))
 (define record-type-descriptor? (lambda (obj)
  (let
     ([and-tmp
       (#%tuple? (lref obj))])
   (if
     (lref and-tmp)
     (#%eq? (#%tuple-ref (lref obj) (const 0))
      (const type:record-type-descriptor))
     (const false)))))
 (define rtd-name (lambda (rtd) (#%tuple-ref (lref rtd) (const 1))))
 (define rtd-parent (lambda (rtd) (#%tuple-ref (lref rtd) (const 2))))
 (define rtd-uid (lambda (rtd) (#%tuple-ref (lref rtd) (const 3))))
 (define rtd-sealed? (lambda (rtd)
  (#%tuple-ref (lref rtd) (const 4))))
 (define rtd-opaque? (lambda (rtd)
  (#%tuple-ref (lref rtd) (const 5))))
 (define rtd-fields (lambda (rtd) (#%tuple-ref (lref rtd) (const 6))))
 (define rtd-printer (lambda (rtd)
  (#%tuple-ref (lref rtd) (const 7))))
 (define set-rtd-printer! (lambda (rtd printer)
  (#%tuple-set! (lref rtd) (const 7) (lref printer))))
 (define rtd-ancestor? (lambda (parent rtd)
  (fix
    ([loop (lambda (rtd)
      (let
         ([or-tmp
           (#%eq? (lref rtd) (lref parent))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (let
             ([and-tmp
               (call (toplevel-ref false rtd-parent) (lref rtd))])
           (if
             (lref and-tmp)
             (call (lref loop) (call (toplevel-ref false rtd-parent) (lref rtd)))
             (const false))))))])
   (call (lref loop) (lref rtd)))))
 (define rtd-inherited-field-count (lambda (rtd)
  (fix
    ([loop (lambda (rtd count)
      (if
        (lref rtd)
        (call (lref loop) (call (toplevel-ref false rtd-parent) (lref rtd))
         (#%+ (lref count)
          (#%length (call (toplevel-ref false rtd-fields) (lref rtd)))))
        (lref count)))])
   (call (lref loop) (call (toplevel-ref false rtd-parent) (lref rtd))
    (const 0)))))
 (define rtd-total-field-count (lambda (rtd)
  (#%+ (call (toplevel-ref false rtd-inherited-field-count) (lref rtd))
   (#%length (call (toplevel-ref false rtd-fields) (lref rtd))))))
 (define record-type-name (lambda (rtd)
  (call (toplevel-ref false rtd-name) (lref rtd))))
 (define record-type-parent (lambda (rtd)
  (call (toplevel-ref false rtd-parent) (lref rtd))))
 (define record-type-uid (lambda (rtd)
  (call (toplevel-ref false rtd-uid) (lref rtd))))
 (define record-type-sealed? (lambda (rtd)
  (call (toplevel-ref false rtd-sealed?) (lref rtd))))
 (define record-type-opaque? (lambda (rtd)
  (call (toplevel-ref false rtd-opaque?) (lref rtd))))
 (define record-type-fields (lambda (rtd)
  (call (toplevel-ref false rtd-fields) (lref rtd))))
 (define record-type-field-names (lambda (rtd)
  (call (toplevel-ref false list->vector) (call (toplevel-ref false map) #%cdr
    (call (toplevel-ref false rtd-fields) (lref rtd))))))
 (define record-field-mutable? (lambda (rtd k)
  (#%car (call (toplevel-ref false list-ref) (call (toplevel-ref false rtd-fields) (lref rtd))
    (lref k)))))
 (define cadr (lambda (x) (#%car (#%cdr (lref x)))))
 (define make-record-type-descriptor (lambda (name parent uid sealed? opaque? fields)
  (seq
   (let
      ([or-tmp
        (call (module-ref (capy)::symbol? #t) (lref name))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const make-record-type-descriptor)
        (const expected a symbol for name)
        (lref name))))
   (let
      ([or-tmp
        (#%vector? (lref fields))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const make-record-type-descriptor)
        (const expected a vector for fields)
        (lref fields))))
   (let
      ([and-tmp
        (lref parent)])
    (if
      (lref and-tmp)
      (let
         ([and-tmp
           (let
              ([or-tmp
                (call (toplevel-ref false record-type-descriptor?) (lref parent))])
            (if
              (lref or-tmp)
              (lref or-tmp)
              (call (toplevel-ref false assertion-violation) (const make-record-type-descriptor)
                (const expected a record-type descriptor or #f for parent)
                (lref parent))))])
       (if
         (lref and-tmp)
         (let
            ([and-tmp
              (call (toplevel-ref false rtd-sealed?) (lref parent))])
          (if
            (lref and-tmp)
            (call (toplevel-ref false assertion-violation) (const make-record-type-descriptor)
             (const cannot extend a sealed record type)
             (lref parent))
            (const false)))
         (const false)))
      (const false)))
   (let
      ([opaque?
        (let
           ([or-tmp
             (lref opaque?)])
         (if
           (lref or-tmp)
           (lref or-tmp)
           (let
               ([and-tmp
                 (lref parent)])
             (if
               (lref and-tmp)
               (call (toplevel-ref false rtd-opaque?) (lref parent))
               (const false)))))]
      [fields
        (call (toplevel-ref false map) (lambda (field)
          (if
            (#%eq? (#%car (lref field)) (const mutable))
            (#%cons (const true) (#%car (#%cdr (lref field))))
            (#%cons (const false) (#%car (#%cdr (lref field))))))
         (call (toplevel-ref false vector->list) (lref fields)))])
    (let
       ([cond-test-tmp
         (#%not (lref uid))])
     (if
       (lref cond-test-tmp)
       (call (toplevel-ref false make-rtd) (lref name)
        (lref parent)
        (const false)
        (lref sealed?)
        (lref opaque?)
        (lref fields))
       (let
           ([current
             (call (toplevel-ref false core-hash-ref) (toplevel-ref false nongenerative-record-types)
              (lref uid)
              (const false))])
         (let
            ([cond-test-tmp
              (lref current)])
          (if
            (lref cond-test-tmp)
            (if
              (let
                  ([and-tmp
                    (#%eqv? (call (toplevel-ref false rtd-uid) (lref current))
                     (lref uid))])
                (if
                  (lref and-tmp)
                  (let
                     ([and-tmp
                       (#%eqv? (lref parent)
                        (call (toplevel-ref false rtd-parent) (lref current)))])
                   (if
                     (lref and-tmp)
                     (#%equal? (lref fields)
                      (call (toplevel-ref false rtd-fields) (lref current)))
                     (const false)))
                  (const false)))
              (lref current)
              (call (toplevel-ref false assertion-violation) (const make-record-type-descriptor)
                (const mismatched subsequent call for nongenerative record-type)
                (#%cons (lref name)
                 (#%cons (lref parent)
                  (#%cons (lref uid)
                   (#%cons (lref sealed?)
                    (#%cons (lref opaque?)
                     (#%cons (lref fields) (const '())))))))))
            (let
                ([new
                  (call (toplevel-ref false make-rtd) (lref name)
                   (lref parent)
                   (lref uid)
                   (lref sealed?)
                   (lref opaque?)
                   (lref fields))])
              (seq
               (call (toplevel-ref false core-hash-put!) (toplevel-ref false nongenerative-record-types)
                (lref uid)
                (lref new))
               (lref new))))))))))))
 (define make-rcd (lambda (rtd protocol custom-protocol? parent)
  (let
     ([tup
       (#%make-tuple (const 5) (const #<undefined>))])
   (seq
    (#%tuple-set! (lref tup)
     (const 0)
     (const type:record-constructor-descriptor))
    (#%tuple-set! (lref tup) (const 1) (lref rtd))
    (#%tuple-set! (lref tup) (const 2) (lref protocol))
    (#%tuple-set! (lref tup) (const 3) (lref custom-protocol?))
    (#%tuple-set! (lref tup) (const 4) (lref parent))
    (lref tup)))))
 (define record-constructor-descriptor? (lambda (obj)
  (let
     ([and-tmp
       (#%tuple? (lref obj))])
   (if
     (lref and-tmp)
     (#%eq? (#%tuple-ref (lref obj) (const 0))
      (const type:record-constructor-descriptor))
     (const false)))))
 (define rcd-rtd (lambda (rcd) (#%tuple-ref (lref rcd) (const 1))))
 (define rcd-protocol (lambda (rcd)
  (#%tuple-ref (lref rcd) (const 2))))
 (define rcd-custom-protocol? (lambda (rcd)
  (#%tuple-ref (lref rcd) (const 3))))
 (define rcd-parent (lambda (rcd) (#%tuple-ref (lref rcd) (const 4))))
 (define default-protocol (lambda (rtd)
  (let
     ([parent
       (call (toplevel-ref false rtd-parent) (lref rtd))])
   (if
     (lref parent)
     (let
        ([parent-field-count
          (call (toplevel-ref false rtd-total-field-count) (lref parent))])
      (lambda (p)
       (lambda field-values
        (receive (parent-field-values this-field-values)
          (call (toplevel-ref false split-at) (lref field-values)
            (lref parent-field-count))
          (call (module-ref (capy)::apply #t) (call (module-ref (capy)::apply #t) (lref p)
             (lref parent-field-values))
            (lref this-field-values))))))
     (lambda (p)
       (lambda field-values
        (call (module-ref (capy)::apply #t) (lref p)
         (lref field-values))))))))
 (define make-simple-conser (lambda (desc rtd argc)
  (call (call (toplevel-ref false rcd-protocol) (lref desc)) (lambda field-values
    (call (module-ref (capy)::apply #t) #%tuple
     (lref rtd)
     (lref field-values))))))
 (define make-nested-conser (lambda (desc rtd argc)
  (call (call (toplevel-ref false rcd-protocol) (lref desc)) (call (fix
     ([loop (lambda (desc)
       (if
         (call (toplevel-ref false rcd-parent) (lref desc))
         (let
            ([parent
              (call (toplevel-ref false rcd-parent) (lref desc))])
          (lambda extra-field-values
           (lambda protocol-args
            (lambda this-field-values
             (call (module-ref (capy)::apply #t) (call (call (toplevel-ref false rcd-protocol) (lref parent)) (call (module-ref (capy)::apply #t) (call (lref loop) (lref parent))
                (#%append (lref this-field-values)
                 (lref extra-field-values))))
              (lref protocol-args))))))
         (lambda extra-field-values
           (lambda this-field-values
            (let
               ([field-values
                 (#%append (lref this-field-values)
                  (lref extra-field-values))])
             (call (module-ref (capy)::apply #t) #%tuple
              (lref rtd)
              (lref field-values)))))))])
    (call (lref loop) (lref desc)))))))
 (define make-record-constructor-descriptor (lambda (rtd parent protocol)
  (let
     ([custom-protocol?
       (let
          ([and-tmp
            (lref protocol)])
        (if  (lref and-tmp)  (const true)  (const false)))]
     [protocol
       (let
          ([or-tmp
            (lref protocol)])
        (if
          (lref or-tmp)
          (lref or-tmp)
          (call (toplevel-ref false default-protocol) (lref rtd))))]
     [parent
       (let
          ([or-tmp
            (lref parent)])
        (if
          (lref or-tmp)
          (lref or-tmp)
          (if
             (call (toplevel-ref false rtd-parent) (lref rtd))
             (call (toplevel-ref false make-record-constructor-descriptor) (call (toplevel-ref false rtd-parent) (lref rtd))
              (const false)
              (const false))
             (const false))))])
   (call (toplevel-ref false make-rcd) (lref rtd)
    (lref protocol)
    (lref custom-protocol?)
    (lref parent)))))
 (define flat-field-offset (lambda (rtd k)
  (#%+ (lref k)
   (#%+ (call (toplevel-ref false rtd-inherited-field-count) (lref rtd))
    (const 1)))))
 (define make-accessor (lambda (rtd k)
  (lambda (obj)
   (if
     (let
         ([or-tmp
           (#%eq? (lref rtd) (#%tuple-ref (lref obj) (const 0)))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (call (toplevel-ref false rtd-ancestor?) (lref rtd)
           (#%tuple-ref (lref obj) (const 0)))))
     (#%tuple-ref (lref obj) (lref k))
     (call (toplevel-ref false assertion-violation) (const record accessor)
       (call (toplevel-ref false wrong-type-argument-message) (call (toplevel-ref false format) (const record of type ~a)
         (call (toplevel-ref false rtd-name) (lref rtd)))
        (lref obj)))))))
 (define wrong-type-argument-message (lambda args (lref args)))
 (define format (lambda (msg rest) (lref msg)))
 (define make-mutator (lambda (rtd k)
  (lambda (obj datum)
   (if
     (let
         ([or-tmp
           (#%eq? (lref rtd) (#%tuple-ref (lref obj) (const 0)))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (call (toplevel-ref false rtd-ancestor?) (lref rtd)
           (#%tuple-ref (lref obj) (const 0)))))
     (#%tuple-set! (lref obj) (lref k) (lref datum))
     (call (toplevel-ref false assertion-violation) (const record mutator)
       (call (toplevel-ref false wrong-type-argument-message) (call (toplevel-ref false format) (const record of type ~a)
         (call (toplevel-ref false rtd-name) (lref rtd)))
        (#%cons (lref obj) (#%cons (lref datum) (const '())))))))))
 (define make-predicate (lambda (rtd)
  (lambda (obj)
   (let
      ([and-tmp
        (#%tuple? (lref obj))])
    (if
      (lref and-tmp)
      (let
         ([or-tmp
           (#%eq? (lref rtd) (#%tuple-ref (lref obj) (const 0)))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (call (toplevel-ref false rtd-ancestor?) (lref rtd)
           (#%tuple-ref (lref obj) (const 0)))))
      (const false))))))
 (define record-constructor (lambda (desc)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-constructor-descriptor?) (lref desc))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-constructor)
        (call (toplevel-ref false wrong-type-argument-message) (const record-constructor-descriptor)
         (lref desc)))))
   (let
      ([rtd
        (call (toplevel-ref false rcd-rtd) (lref desc))])
    (if
      (call (toplevel-ref false rcd-parent) (lref desc))
      (call (toplevel-ref false make-nested-conser) (lref desc)
       (lref rtd)
       (call (toplevel-ref false rtd-total-field-count) (lref rtd)))
      (call (toplevel-ref false make-simple-conser) (lref desc)
        (lref rtd)
        (#%length (call (toplevel-ref false rtd-fields) (lref rtd)))))))))
 (define record-predicate (lambda (rtd)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-type-descriptor?) (lref rtd))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-predicate)
        (call (toplevel-ref false wrong-type-argument-message) (const record-type-descriptor)
         (lref rtd)))))
   (call (toplevel-ref false make-predicate) (lref rtd)))))
 (define record-accessor (lambda (rtd k)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-type-descriptor?) (lref rtd))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-accessor)
        (call (toplevel-ref false wrong-type-argument-message) (const record-type-descriptor)
         (lref rtd))
        (#%cons (lref rtd) (#%cons (lref k) (const '()))))))
   (let
      ([or-tmp
        (let
           ([<-tmp
             (#%< (const -1) (lref k))])
         (if
           (lref <-tmp)
           (#%< (lref k)
            (#%length (call (toplevel-ref false rtd-fields) (lref rtd))))
           (const false)))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-accessor)
        (const field index out of range))))
   (call (toplevel-ref false make-accessor) (lref rtd)
    (call (toplevel-ref false flat-field-offset) (lref rtd)
     (lref k))))))
 (define record-mutator (lambda (rtd k)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-type-descriptor?) (lref rtd))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-mutator)
        (call (toplevel-ref false wrong-type-argument-message) (const record-type-descriptor)
         (lref rtd))
        (#%cons (lref rtd) (#%cons (lref k) (const '()))))))
   (let
      ([or-tmp
        (let
           ([<-tmp
             (#%< (const -1) (lref k))])
         (if
           (lref <-tmp)
           (#%< (lref k)
            (#%length (call (toplevel-ref false rtd-fields) (lref rtd))))
           (const false)))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-mutator)
        (const field index out of range)
        (#%cons (lref rtd) (#%cons (lref k) (const '()))))))
   (let
      ([or-tmp
        (call (toplevel-ref false record-field-mutable?) (lref rtd)
         (lref k))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-mutator)
        (const specified field is immutable)
        (#%cons (lref rtd) (#%cons (lref k) (const '()))))))
   (call (toplevel-ref false make-mutator) (lref rtd)
    (call (toplevel-ref false flat-field-offset) (lref rtd)
     (lref k))))))
 (define make-record-type (lambda (name rtd rcd)
  (let
     ([tup
       (#%make-tuple (const 4) (const #<undefined>))])
   (seq
    (#%tuple-set! (lref tup) (const 0) (const type:record-type))
    (#%tuple-set! (lref tup) (const 1) (lref name))
    (#%tuple-set! (lref tup) (const 2) (lref rtd))
    (#%tuple-set! (lref tup) (const 3) (lref rcd))
    (lref tup)))))
 (define record-type? (lambda (obj)
  (let
     ([and-tmp
       (#%tuple? (lref obj))])
   (if
     (lref and-tmp)
     (#%eq? (#%tuple-ref (lref obj) (const 0))
      (const type:record-type))
     (const false)))))
 (define record-type-rcd (lambda (obj)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-type?) (lref obj))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-type-rcd)
        (const not a record-type)
        (lref obj))))
   (#%tuple-ref (lref obj) (const 3)))))
 (define record-type-rtd (lambda (obj)
  (seq
   (let
      ([or-tmp
        (call (toplevel-ref false record-type?) (lref obj))])
    (if
      (lref or-tmp)
      (lref or-tmp)
      (call (toplevel-ref false assertion-violation) (const record-type-rtd)
        (call (toplevel-ref false wrong-type-argument-message) (const record-type)
         (lref obj)))))
   (#%tuple-ref (lref obj) (const 2)))))
 (define record? (lambda (obj)
  (let
     ([and-tmp
       (#%tuple? (lref obj))])
   (if
     (lref and-tmp)
     (call (toplevel-ref false record-type-descriptor?) (#%tuple-ref (lref obj)
       (const 0)))
     (const false)))))
 (define record-type-descriptor (lambda (obj)
  (call (toplevel-ref false record-rtd) (lref obj))))
 (define record-rtd (lambda (obj)
  (if
    (call (toplevel-ref false record?) (lref obj))
    (#%tuple-ref (lref obj) (const 0))
    (call (toplevel-ref false assertion-violation) (const record-rtd)
      (call (toplevel-ref false format) (const expected a record, but got ~r)
       (lref obj))
      (lref obj)))))
 (define &condition (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &condition)
       (const false)
       (const false)
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (const false)
        (const false))])
   (call (toplevel-ref false make-record-type) (const &condition)
    (lref rtd)
    (lref rcd)))))
 (define compound-condition-component (lambda (obj)
  (#%tuple-ref (lref obj) (const 1))))
 (define condition (lambda components
  (let
     ([tup
       (#%make-tuple (const 2) (const #<undefined>))])
   (seq
    (#%tuple-set! (lref tup) (const 0) (const type:condition))
    (#%tuple-set! (lref tup)
     (const 1)
     (call (module-ref (capy)::apply #t) #%append
      (call (toplevel-ref false map) (lambda (component)
        (call (toplevel-ref false simple-conditions) (lref component)))
       (lref components))))
    (lref tup)))))
 (define compound-condition? (lambda (obj)
  (let
     ([and-tmp
       (#%tuple? (lref obj))])
   (if
     (lref and-tmp)
     (#%eq? (#%tuple-ref (lref obj) (const 0)) (const type:condition))
     (const false)))))
 (define simple-condition? (lambda (obj)
  (let
     ([and-tmp
       (call (toplevel-ref false record?) (lref obj))])
   (if
     (lref and-tmp)
     (call (toplevel-ref false rtd-ancestor?) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
      (call (toplevel-ref false record-rtd) (lref obj)))
     (const false)))))
 (define condition? (lambda (obj)
  (let
     ([or-tmp
       (call (toplevel-ref false simple-condition?) (lref obj))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (call (toplevel-ref false compound-condition?) (lref obj))))))
 (define simple-conditions (lambda (c)
  (if
    (call (toplevel-ref false simple-condition?) (lref c))
    (#%cons (lref c) (const '()))
    (if
       (call (toplevel-ref false compound-condition?) (lref c))
       (call (toplevel-ref false compound-condition-component) (lref c))
       (const false)))))
 (define condition-predicate (lambda (rtd)
  (lambda (obj)
   (if
     (call (toplevel-ref false simple-condition?) (lref obj))
     (call (toplevel-ref false rtd-ancestor?) (lref rtd)
      (call (toplevel-ref false record-rtd) (lref obj)))
     (if
        (call (toplevel-ref false compound-condition?) (lref obj))
        (call (toplevel-ref false any1) (lambda (c)
          (call (toplevel-ref false rtd-ancestor?) (lref rtd)
           (call (toplevel-ref false record-rtd) (lref c))))
         (call (toplevel-ref false compound-condition-component) (lref obj)))
        (const false))))))
 (define condition-accessor (lambda (rtd proc)
  (fix
    ([wrong-type (lambda (rtd obj)
      (call (toplevel-ref false assertion-violation) (const condition accessor)
       (call (toplevel-ref false format) (const expected condition of a subtype of ~s, but got ~r)
        (lref rtd)
        (lref obj))
       (lref rtd)
       (lref obj)))])
   (seq
    (let
       ([or-tmp
         (call (toplevel-ref false rtd-ancestor?) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
          (lref rtd))])
     (if
       (lref or-tmp)
       (lref or-tmp)
       (call (toplevel-ref false assertion-violation) (const condition-accessor)
         (call (toplevel-ref false format) (const expected record-type-descriptor of a subtype of &condition, but got ~s)
          (lref rtd))
         (lref rtd)
         (lref proc))))
    (lambda (obj)
     (if
       (call (toplevel-ref false simple-condition?) (lref obj))
       (seq
        (let
           ([or-tmp
             (call (toplevel-ref false rtd-ancestor?) (lref rtd)
              (call (toplevel-ref false record-rtd) (lref obj)))])
         (if
           (lref or-tmp)
           (lref or-tmp)
           (call (lref wrong-type) (lref rtd) (lref obj))))
        (call (lref proc) (lref obj)))
       (if
          (call (toplevel-ref false compound-condition?) (lref obj))
          (let
             ([res
               (call (toplevel-ref false any1) (lambda (c)
                 (let
                    ([and-tmp
                      (call (toplevel-ref false rtd-ancestor?) (lref rtd)
                       (call (toplevel-ref false record-rtd) (lref c)))])
                  (if  (lref and-tmp)  (lref c)  (const false))))
                (call (toplevel-ref false compound-condition-component) (lref obj)))])
           (if
             (lref res)
             (call (lref proc) (lref res))
             (call (lref wrong-type) (lref rtd) (lref obj))))
          (call (lref wrong-type) (lref rtd) (lref obj)))))))))
 (define &message (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &message)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (const false)
       (const false)
       (const false)
       (const #((immutable message))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &message)
    (lref rtd)
    (lref rcd)))))
 (define make-message-condition (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &message))))
 (define message-condition? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &message))))
 (define condition-message (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &message))
  (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &message))
   (const 0))))
 (define &source (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &source)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (const false)
       (const false)
       (const false)
       (const #((immutable file) (immutable line) (immutable column))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &source)
    (lref rtd)
    (lref rcd)))))
 (define make-source-condition (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &source))))
 (define source-condition? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))))
 (define condition-source-file (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
  (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
   (const 0))))
 (define condition-source-line (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
  (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
   (const 1))))
 (define condition-source-column (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
  (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &source))
   (const 2))))
 (define make-condition-uid (lambda () (const false)))
 (define &warning (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &warning)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &warning)
    (lref rtd)
    (lref rcd)))))
 (define make-warning (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &warning))))
 (define warning? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &warning))))
 (define &serious (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &serious)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &serious)
    (lref rtd)
    (lref rcd)))))
 (define make-serious-condition (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &serious))))
 (define serious-condition? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &serious))))
 (define &error (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &error)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &serious))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &serious))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &error)
    (lref rtd)
    (lref rcd)))))
 (define make-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &error))))
 (define error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &error))))
 (define &violation (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &violation)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &serious))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &serious))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &violation)
    (lref rtd)
    (lref rcd)))))
 (define make-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))))
 (define violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))))
 (define &assertion (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &assertion)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &assertion)
    (lref rtd)
    (lref rcd)))))
 (define make-assertion-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &assertion))))
 (define assertion-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &assertion))))
 (define &irritants (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &irritants)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable irritants))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &irritants)
    (lref rtd)
    (lref rcd)))))
 (define &irritants-irritants (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &irritants))
  (const 0)))
 (define make-irritants-condition (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &irritants))))
 (define irritants-condition? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &irritants))))
 (define condition-irritants (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &irritants))
  (toplevel-ref false &irritants-irritants)))
 (define &who (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &who)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &condition))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable who))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &condition))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &who)
    (lref rtd)
    (lref rcd)))))
 (define &who-who (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &who))
  (const 0)))
 (define make-who-condition (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &who))))
 (define who-condition? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &who))))
 (define condition-who (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &who))
  (toplevel-ref false &who-who)))
 (define &non-continuable (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &non-continuable)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &non-continuable)
    (lref rtd)
    (lref rcd)))))
 (define make-non-continuable-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &non-continuable))))
 (define non-continuable-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &non-continuable))))
 (define &implementation-restriction (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &implementation-restriction)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &implementation-restriction)
    (lref rtd)
    (lref rcd)))))
 (define make-implementation-restriction-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &implementation-restriction))))
 (define implementation-restriction-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &implementation-restriction))))
 (define &lexical (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &lexical)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &lexical)
    (lref rtd)
    (lref rcd)))))
 (define make-lexical-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &lexical))))
 (define lexical-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &lexical))))
 (define &syntax (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &syntax)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable form) (immutable subform))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &syntax)
    (lref rtd)
    (lref rcd)))))
 (define &syntax-form (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &syntax))
  (const 0)))
 (define &syntax-subform (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &syntax))
  (const 1)))
 (define make-syntax-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &syntax))))
 (define syntax-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &syntax))))
 (define syntax-violation-form (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &syntax))
  (toplevel-ref false &syntax-form)))
 (define syntax-violation-subform (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &syntax))
  (toplevel-ref false &syntax-subform)))
 (define &undefined (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &undefined)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &violation))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &violation))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &undefined)
    (lref rtd)
    (lref rcd)))))
 (define make-undefined-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &undefined))))
 (define undefined-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &undefined))))
 (define &i/o (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &error))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &error))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))))
 (define i/o-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))))
 (define &i/o-read (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-read)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-read)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-read-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-read))))
 (define i/o-read-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-read))))
 (define &i/o-write (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-write)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-write)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-write-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-write))))
 (define i/o-write-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-write))))
 (define &i/o-invalid-position (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-invalid-position)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable position))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-invalid-position)
    (lref rtd)
    (lref rcd)))))
 (define &i/o-invalid-position-position (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-invalid-position))
  (const 0)))
 (define make-i/o-invalid-position-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-invalid-position))))
 (define i/o-invalid-position-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-invalid-position))))
 (define i/o-error-position (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-invalid-position))
  (toplevel-ref false &i/o-invalid-position-position)))
 (define &i/o-filename (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-filename)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable filename))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-filename)
    (lref rtd)
    (lref rcd)))))
 (define &i/o-filename-filename (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))
  (const 0)))
 (define make-i/o-filename-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-filename))))
 (define i/o-filename-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))))
 (define i/o-error-filename (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))
  (toplevel-ref false &i/o-filename-filename)))
 (define &i/o-file-protection (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-file-protection)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-filename))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-file-protection)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-file-protection-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-file-protection))))
 (define i/o-file-protection-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-file-protection))))
 (define &i/o-file-is-read-only (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-file-is-read-only)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-file-protection))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-file-protection))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-file-is-read-only)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-file-is-read-only-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-file-is-read-only))))
 (define i/o-file-is-read-only-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-file-is-read-only))))
 (define &i/o-file-already-exists (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-file-already-exists)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-filename))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-file-already-exists)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-file-already-exists-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-file-already-exists))))
 (define i/o-file-already-exists-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-file-already-exists))))
 (define &i/o-file-does-not-exist (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-file-does-not-exist)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-filename))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-filename))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-file-does-not-exist)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-file-does-not-exist-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-file-does-not-exist))))
 (define i/o-file-does-not-exist-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-file-does-not-exist))))
 (define &i/o-port (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-port)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable port))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-port)
    (lref rtd)
    (lref rcd)))))
 (define &i/o-port-port (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-port))
  (const 0)))
 (define make-i/o-port-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-port))))
 (define i/o-port-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-port))))
 (define i/o-error-port (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-port))
  (toplevel-ref false &i/o-port-port)))
 (define &i/o-decoding (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-decoding)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-port))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-port))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-decoding)
    (lref rtd)
    (lref rcd)))))
 (define make-i/o-decoding-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-decoding))))
 (define i/o-decoding-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-decoding))))
 (define &i/o-encoding (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &i/o-encoding)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-port))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #((immutable char))))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-port))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &i/o-encoding)
    (lref rtd)
    (lref rcd)))))
 (define &i/o-encoding-char (call (toplevel-ref false record-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-encoding))
  (const 0)))
 (define make-i/o-encoding-error (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &i/o-encoding))))
 (define i/o-encoding-error? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-encoding))))
 (define i/o-encoding-error-char (call (toplevel-ref false condition-accessor) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &i/o-encoding))
  (toplevel-ref false &i/o-encoding-char)))
 (define &no-infinities (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &no-infinities)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &implementation-restriction))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &implementation-restriction))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &no-infinities)
    (lref rtd)
    (lref rcd)))))
 (define make-no-infinities-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &no-infinities))))
 (define no-infinities-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &no-infinities))))
 (define &no-nans (let
    ([rtd
      (call (toplevel-ref false make-record-type-descriptor) (const &no-nans)
       (call (toplevel-ref false record-type-rtd) (toplevel-ref false &implementation-restriction))
       (call (toplevel-ref false make-condition-uid))
       (const false)
       (const false)
       (const #()))])
  (let
     ([rcd
       (call (toplevel-ref false make-record-constructor-descriptor) (lref rtd)
        (call (toplevel-ref false record-type-rcd) (toplevel-ref false &implementation-restriction))
        (const false))])
   (call (toplevel-ref false make-record-type) (const &no-nans)
    (lref rtd)
    (lref rcd)))))
 (define make-no-nans-violation (call (toplevel-ref false record-constructor) (call (toplevel-ref false record-type-rcd) (toplevel-ref false &no-nans))))
 (define no-nans-violation? (call (toplevel-ref false condition-predicate) (call (toplevel-ref false record-type-rtd) (toplevel-ref false &no-nans))))
 (define current-exception-handler (let
    ([f
      (call (toplevel-ref false make-thread-local-fluid) (const false))])
  (lambda args
   (if
     (#%null? (lref args))
     (call (toplevel-ref false fluid-ref) (lref f))
     (let
         ([old
           (call (toplevel-ref false fluid-ref) (lref f))])
       (seq
        (call (toplevel-ref false fluid-set!) (lref f)
         (#%car (lref args)))
        (lref old)))))))
 (define parent-exception-handler (let
    ([f
      (call (toplevel-ref false make-thread-local-fluid) (const false))])
  (lambda args
   (if
     (#%null? (lref args))
     (call (toplevel-ref false fluid-ref) (lref f))
     (let
         ([old
           (call (toplevel-ref false fluid-ref) (lref f))])
       (seq
        (call (toplevel-ref false fluid-set!) (lref f)
         (#%car (lref args)))
        (lref old)))))))
 (define *here* (let
    ([f
      (call (toplevel-ref false make-thread-local-fluid) (#%cons (const false)
        (const '())))])
  (lambda args
   (if
     (#%null? (lref args))
     (call (toplevel-ref false fluid-ref) (lref f))
     (let
         ([old
           (call (toplevel-ref false fluid-ref) (lref f))])
       (seq
        (call (toplevel-ref false fluid-set!) (lref f)
         (#%car (lref args)))
        (lref old)))))))
 (define reroot! (lambda (there)
  (fix
    ([reroot-loop (lambda (there)
      (if
        (#%eq? (lref there) (call (toplevel-ref false *here*)))
        (const false)
        (seq
          (call (lref reroot-loop) (#%cdr (lref there)))
          (let
             ([old-pair
               (#%car (lref there))])
           (let
              ([before
                (#%car (lref old-pair))]
              [after
                (#%cdr (lref old-pair))])
            (seq
             (#%set-car! (call (toplevel-ref false *here*))
              (#%cons (lref after) (lref before)))
             (#%set-cdr! (call (toplevel-ref false *here*))
              (lref there))
             (#%set-car! (lref there) (const false))
             (#%set-cdr! (lref there) (const '()))
             (call (toplevel-ref false *here*) (lref there))
             (call (lref before))))))))])
   (call (lref reroot-loop) (lref there)))))
 (define dynamic-wind (lambda (before thunk after)
  (let
     ([here
       (call (toplevel-ref false *here*))])
   (let
      ([there
        (#%cons (const false) (const '()))])
    (seq
     (call (lref before))
     (#%set-car! (call (toplevel-ref false *here*))
      (#%cons (lref after) (lref before)))
     (#%set-cdr! (call (toplevel-ref false *here*)) (lref there))
     (call (toplevel-ref false *here*) (lref there))
     (receive ( . results)
       (call (lref thunk))
       (seq
         (call (toplevel-ref false reroot!) (lref here))
         (call (module-ref (capy)::apply #t) #%values
          (lref results)))))))))
 (define call-with-current-continuation (lambda (proc)
  (let
     ([here
       (call (toplevel-ref false *here*))])
   (call (toplevel-ref false .call/cc-unsafe) (lambda (cont)
     (call (lref proc) (lambda results
       (seq
        (call (toplevel-ref false reroot!) (lref here))
        (call (module-ref (capy)::apply #t) (lref cont)
         (lref results))))))))))
 (define call/cc (toplevel-ref false call-with-current-continuation))
 (define unhandled-exception-error (lambda (val)
  (call (toplevel-ref false .return-error) (lref val))))
 (define *basic-exception-handlers* (#%cons (toplevel-ref false unhandled-exception-error)
  (const '())))
 (define *current-exception-handlers* (let
    ([f
      (call (toplevel-ref false make-thread-local-fluid) (toplevel-ref false *basic-exception-handlers*))])
  (lambda args
   (if
     (#%null? (lref args))
     (call (toplevel-ref false fluid-ref) (lref f))
     (let
         ([old
           (call (toplevel-ref false fluid-ref) (lref f))])
       (seq
        (call (toplevel-ref false fluid-set!) (lref f)
         (#%car (lref args)))
        (lref old)))))))
 (define raise (lambda (obj)
  (call (toplevel-ref false .raise) (lref obj))))
 (define raise-continuable (lambda (obj)
  (let
     ([handlers
       (call (toplevel-ref false *current-exception-handlers*))])
   (call (toplevel-ref false with-exception-handler) (#%car (#%cdr (lref handlers)))
    (lambda () (call (#%car (lref handlers)) (lref obj)))))))
 (define with-exception-handler (lambda (handler thunk)
  (let
     ([previous-handlers
       (call (toplevel-ref false *current-exception-handlers*))])
   (let
      ([new-handlers
        (if
          (lref handler)
          (#%cons (lref handler) (lref previous-handlers))
          (lref previous-handlers))])
    (call (toplevel-ref false dynamic-wind) (lambda ()
      (call (toplevel-ref false *current-exception-handlers*) (lref new-handlers)))
     (lambda ()
      (call (toplevel-ref false .with-handler) (lref handler)
       (lref thunk)))
     (lambda ()
      (call (toplevel-ref false *current-exception-handlers*) (lref previous-handlers))))))))
 (define assertion-violation (lambda (who message irritants)
  (seq
   (call (toplevel-ref false print-stacktrace))
   (if
     (let
         ([or-tmp
           (#%not (lref who))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (let
             ([or-tmp
               (#%string? (lref who))])
           (if
             (lref or-tmp)
             (lref or-tmp)
             (call (module-ref (capy)::symbol? #t) (lref who))))))
     (if
       (#%string? (lref message))
       (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
         (call (toplevel-ref false filter) #%values
          (#%cons (call (toplevel-ref false make-assertion-violation))
           (#%cons (let
               ([and-tmp
                 (lref who)])
             (if
               (lref and-tmp)
               (call (toplevel-ref false make-who-condition) (lref who))
               (const false)))
            (#%cons (call (toplevel-ref false make-message-condition) (lref message))
             (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
              (const '()))))))))
       (call (toplevel-ref false raise) (const false)))
     (call (toplevel-ref false raise) (const false))))))
 (define syntax-violation (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
        (call (toplevel-ref false filter) #%values
         (#%cons (call (toplevel-ref false make-syntax-violation))
          (#%cons (let
              ([and-tmp
                (lref who)])
            (if
              (lref and-tmp)
              (call (toplevel-ref false make-who-condition) (lref who))
              (const false)))
           (#%cons (call (toplevel-ref false make-message-condition) (lref message))
            (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
             (const '()))))))))
      (const false))
    (const false))))
 (define error (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
        (call (toplevel-ref false filter) #%values
         (#%cons (call (toplevel-ref false make-error))
          (#%cons (let
              ([and-tmp
                (lref who)])
            (if
              (lref and-tmp)
              (call (toplevel-ref false make-who-condition) (lref who))
              (const false)))
           (#%cons (call (toplevel-ref false make-message-condition) (lref message))
            (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
             (const '()))))))))
      (const false))
    (const false))))
 (define implementation-restriction-violation (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
        (call (toplevel-ref false filter) #%values
         (#%cons (call (toplevel-ref false make-implementation-restriction-violation))
          (#%cons (let
              ([and-tmp
                (lref who)])
            (if
              (lref and-tmp)
              (call (toplevel-ref false make-who-condition) (lref who))
              (const false)))
           (#%cons (call (toplevel-ref false make-message-condition) (lref message))
            (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
             (const '()))))))))
      (const false))
    (const false))))
 (define raise-i/o-error (lambda (who message irritants)
  (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
    (call (toplevel-ref false filter) #%values
     (#%cons (call (toplevel-ref false make-i/o-error))
      (#%cons (let
          ([and-tmp
            (lref who)])
        (if
          (lref and-tmp)
          (call (toplevel-ref false make-who-condition) (lref who))
          (const false)))
       (#%cons (call (toplevel-ref false make-message-condition) (lref message))
        (#%cons (let
            ([and-tmp
              (#%pair? (lref irritants))])
          (if
            (lref and-tmp)
            (call (toplevel-ref false make-irritants-condition) (lref irritants))
            (const false)))
         (const '()))))))))))
 (define .make-i/o-error (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
       (call (toplevel-ref false filter) #%values
        (#%cons (call (toplevel-ref false make-i/o-error))
         (#%cons (let
             ([and-tmp
               (lref who)])
           (if
             (lref and-tmp)
             (call (toplevel-ref false make-who-condition) (lref who))
             (const false)))
          (#%cons (call (toplevel-ref false make-message-condition) (lref message))
           (#%cons (let
               ([and-tmp
                 (#%pair? (lref irritants))])
             (if
               (lref and-tmp)
               (call (toplevel-ref false make-irritants-condition) (lref irritants))
               (const false)))
            (const '())))))))
      (const false))
    (const false))))
 (define .make-assertion-violation (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
       (call (toplevel-ref false filter) #%values
        (#%cons (call (toplevel-ref false make-assertion-violation))
         (#%cons (let
             ([and-tmp
               (lref who)])
           (if
             (lref and-tmp)
             (call (toplevel-ref false make-who-condition) (lref who))
             (const false)))
          (#%cons (call (toplevel-ref false make-message-condition) (lref message))
           (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
            (const '())))))))
      (const false))
    (const false))))
 (define .make-error (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
       (call (toplevel-ref false filter) #%values
        (#%cons (call (toplevel-ref false make-error))
         (#%cons (let
             ([and-tmp
               (lref who)])
           (if
             (lref and-tmp)
             (call (toplevel-ref false make-who-condition) (lref who))
             (const false)))
          (#%cons (call (toplevel-ref false make-message-condition) (lref message))
           (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
            (const '())))))))
      (const false))
    (const false))))
 (define .make-implementation-restriction-violation (lambda (who message irritants)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (if
      (#%string? (lref message))
      (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
       (call (toplevel-ref false filter) #%values
        (#%cons (call (toplevel-ref false make-implementation-restriction-violation))
         (#%cons (let
             ([and-tmp
               (lref who)])
           (if
             (lref and-tmp)
             (call (toplevel-ref false make-who-condition) (lref who))
             (const false)))
          (#%cons (call (toplevel-ref false make-message-condition) (lref message))
           (#%cons (call (toplevel-ref false make-irritants-condition) (lref irritants))
            (const '())))))))
      (const false))
    (const false))))
 (define lexical-violation (lambda (who message)
  (call (toplevel-ref false raise) (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
    (call (toplevel-ref false filter) #%values
     (#%cons (call (toplevel-ref false make-lexical-violation))
      (#%cons (let
          ([and-tmp
            (lref who)])
        (if
          (lref and-tmp)
          (call (toplevel-ref false make-who-condition) (lref who))
          (const false)))
       (#%cons (let
           ([and-tmp
             (#%pair? (lref message))])
         (if
           (lref and-tmp)
           (call (toplevel-ref false make-message-condition) (#%car (lref message)))
           (const false)))
        (const '())))))))))
 (define .make-lexical-violation (lambda (who message)
  (if
    (let
        ([or-tmp
          (#%not (lref who))])
      (if
        (lref or-tmp)
        (lref or-tmp)
        (let
            ([or-tmp
              (#%string? (lref who))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (module-ref (capy)::symbol? #t) (lref who))))))
    (call (module-ref (capy)::apply #t) (toplevel-ref false condition)
     (call (toplevel-ref false filter) #%values
      (#%cons (call (toplevel-ref false make-lexical-violation))
       (#%cons (let
           ([and-tmp
             (lref who)])
         (if
           (lref and-tmp)
           (call (toplevel-ref false make-who-condition) (lref who))
           (const false)))
        (#%cons (let
            ([and-tmp
              (#%pair? (lref message))])
          (if
            (lref and-tmp)
            (call (toplevel-ref false make-message-condition) (#%car (lref message)))
            (const false)))
         (const '()))))))
    (const false))))
 (define filter (lambda (pred lst)
  (fix
    ([loop (lambda (lst acc)
      (if
        (#%null? (lref lst))
        (call (toplevel-ref false reverse) (lref acc))
        (if
           (call (lref pred) (#%car (lref lst)))
           (call (lref loop) (#%cdr (lref lst))
            (#%cons (#%car (lref lst)) (lref acc)))
           (call (lref loop) (#%cdr (lref lst)) (lref acc)))))])
   (call (lref loop) (lref lst) (const '())))))
 (define load-in-vicinity (lambda (filename directory)
  (let
     ([thunk
       (call (toplevel-ref false load-thunk-in-vicinity) (lref filename)
        (lref directory))])
   (call (lref thunk)))))
 (define load (lambda (filename)
  (let
     ([thunk
       (call (toplevel-ref false load-thunk-in-vicinity) (lref filename))])
   (call (lref thunk)))))
 (define module-search (lambda (fn m v)
  (let
     ([or-tmp
       (call (lref fn) (lref m) (lref v))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (fix
        ([loop (lambda (pos)
          (if
            (#%null? (lref pos))
            (const false)
            (let
                ([or-tmp
                  (call (lref fn) (#%car (lref pos)) (lref v))])
              (if
                (lref or-tmp)
                (lref or-tmp)
                (call (lref loop) (#%cdr (lref pos)))))))])
       (call (lref loop) (call (toplevel-ref false module-uses) (lref m))))))))
 (define core-hash-map (lambda (proc ht)
  (call (toplevel-ref false map) (lref proc)
   (call (toplevel-ref false core-hash->list) (lref ht)))))
 (define core-hash-for-each (lambda (proc ht)
  (call (toplevel-ref false for-each) (lref proc)
   (call (toplevel-ref false core-hash->list) (lref ht)))))
 (define core-hash-copy (lambda (ht)
  (let
     ([new
       (call (toplevel-ref false make-core-hash))])
   (seq
    (call (toplevel-ref false core-hash-for-each) (lambda (pair)
      (call (toplevel-ref false core-hash-put!) (lref new)
       (#%car (lref pair))
       (#%cdr (lref pair))))
     (lref ht))
    (lref new)))))
 (define module-for-each (lambda (proc module)
  (call (toplevel-ref false for-each) (lambda (kv)
    (call (lref proc) (#%car (lref kv)) (#%cdr (lref kv))))
   (call (toplevel-ref false core-hash->list) (call (toplevel-ref false module-obarray) (lref module))))))
 (define module-map (lambda (proc module)
  (call (toplevel-ref false map) (lref proc)
   (call (toplevel-ref false core-hash->list) (call (toplevel-ref false module-obarray) (lref module))))))
 (define module-ref-submodule (lambda (module name)
  (call (toplevel-ref false core-hash-ref) (call (toplevel-ref false module-submodules) (lref module))
   (lref name))))
 (define module-define-submodule! (lambda (module name submodule)
  (call (toplevel-ref false core-hash-put!) (call (toplevel-ref false module-submodules) (lref module))
   (lref name)
   (lref submodule))))
 (define save-module-excursion (lambda (thunk)
  (let
     ([inner-module
       (#%current-module)]
     [outer-module
       (const false)])
   (let
      ([&inner-module
        (call (module-ref (capy)::make-variable #t) (lref inner-module))]
      [&outer-module
        (call (module-ref (capy)::make-variable #t) (lref outer-module))])
    (call (toplevel-ref false dynamic-wind) (lambda ()
      (seq
       (call (module-ref (capy)::variable-set! #t) (lref &outer-module)
        (#%current-module))
       (#%current-module (call (module-ref (capy)::variable-ref #t) (lref &inner-module)))
       (call (module-ref (capy)::variable-set! #t) (lref &inner-module)
        (const false))))
     (lref thunk)
     (lambda ()
      (seq
       (call (module-ref (capy)::variable-set! #t) (lref &inner-module)
        (#%current-module))
       (#%current-module (call (module-ref (capy)::variable-ref #t) (lref &outer-module)))
       (call (module-ref (capy)::variable-set! #t) (lref &outer-module)
        (const false)))))))))
 (define module-ref (lambda (module name rest)
  (let
     ([var
       (call (toplevel-ref false module-variable) (lref module)
        (lref name))])
   (if
     (let
         ([and-tmp
           (lref var)])
       (if
         (lref and-tmp)
         (call (module-ref (capy)::variable-bound? #t) (lref var))
         (const false)))
     (call (module-ref (capy)::variable-ref #t) (lref var))
     (if
        (#%null? (lref rest))
        (call (toplevel-ref false assertion-violation) (const module-ref)
         (const unbound variable)
         (lref module)
         (lref name))
        (#%car (lref rest)))))))
 (define module-set! (lambda (module name value)
  (let
     ([var
       (call (toplevel-ref false module-variable) (lref module)
        (lref name))])
   (if
     (lref var)
     (call (module-ref (capy)::variable-set! #t) (lref var)
      (lref value))
     (call (toplevel-ref false assertion-violation) (const module-set!)
       (const unbound variable)
       (lref module)
       (lref name))))))
 (define module-defined? (lambda (module name)
  (let
     ([var
       (call (toplevel-ref false module-variable) (lref module)
        (lref name))])
   (let
      ([and-tmp
        (lref var)])
    (if
      (lref and-tmp)
      (call (module-ref (capy)::variable-bound? #t) (lref var))
      (const false))))))
 (define module-use! (lambda (module interface)
  (if
    (#%not (let
         ([or-tmp
           (#%eq? (lref module) (lref interface))])
       (if
         (lref or-tmp)
         (lref or-tmp)
         (call (module-ref (capy)::memq #t) (lref interface)
           (call (toplevel-ref false module-uses) (lref module))))))
    (seq
     (call (toplevel-ref false set-module-uses!) (lref module)
      (#%append (call (toplevel-ref false module-uses) (lref module))
       (#%cons (lref interface) (const '()))))
     (call (toplevel-ref false core-hash-clear!) (call (toplevel-ref false module-import-obarray) (lref module))))
    (const #<undefined>))))
 (define module-use-interfaces! (lambda (module interfaces)
  (let
     ([cur
       (call (toplevel-ref false module-uses) (lref module))])
   (let
      ([new
        (fix
          ([loop (lambda (in out)
            (if
              (#%null? (lref in))
              (call (toplevel-ref false reverse) (lref out))
              (call (lref loop) (#%cdr (lref in))
                (let
                   ([iface
                     (#%car (lref in))])
                 (if
                   (let
                       ([or-tmp
                         (call (module-ref (capy)::memq #t) (lref iface)
                          (lref cur))])
                     (if
                       (lref or-tmp)
                       (lref or-tmp)
                       (call (module-ref (capy)::memq #t) (lref iface)
                         (lref out))))
                   (lref out)
                   (#%cons (lref iface) (lref out)))))))])
         (call (lref loop) (lref interfaces) (const '())))])
    (seq
     (call (toplevel-ref false set-module-uses!) (lref module)
      (#%append (lref cur) (lref new)))
     (call (toplevel-ref false core-hash-clear!) (call (toplevel-ref false module-import-obarray) (lref module))))))))
 (define module-define! (lambda (module name value)
  (let
     ([variable
       (call (toplevel-ref false module-local-variable) (lref module)
        (lref name))])
   (if
     (lref variable)
     (call (module-ref (capy)::variable-set! #t) (lref variable)
      (lref value))
     (let
         ([variable
           (call (module-ref (capy)::make-variable #t) (lref value))])
       (call (toplevel-ref false module-add!) (lref module)
        (lref name)
        (lref variable)))))))
 (define nested-ref (lambda (root names)
  (if
    (#%null? (lref names))
    (lref root)
    (fix
       ([loop (lambda (cur head tail)
         (if
           (#%null? (lref tail))
           (call (toplevel-ref false module-ref) (lref cur)
            (lref head)
            (const false))
           (let
               ([cur
                 (call (toplevel-ref false module-ref-submodule) (lref cur)
                  (lref head))])
             (let
                ([and-tmp
                  (lref cur)])
              (if
                (lref and-tmp)
                (call (lref loop) (lref cur)
                 (#%car (lref tail))
                 (#%cdr (lref tail)))
                (const false))))))])
      (call (lref loop) (lref root)
       (#%car (lref names))
       (#%cdr (lref names)))))))
 (define nested-set! (lambda (root names val)
  (fix
    ([loop (lambda (cur head tail)
      (if
        (#%null? (lref tail))
        (call (toplevel-ref false module-set!) (lref cur)
         (lref head)
         (lref val))
        (let
            ([cur
              (call (toplevel-ref false module-ref-submodule) (lref cur)
               (lref head))])
          (if
            (#%not (lref cur))
            (call (toplevel-ref false assertion-violation) (const nested-set!)
             (const failed to resolve module)
             (lref names))
            (call (lref loop) (lref cur)
              (#%car (lref tail))
              (#%cdr (lref tail)))))))])
   (call (lref loop) (lref root)
    (#%car (lref names))
    (#%cdr (lref names))))))
 (define nested-remove! (lambda (root names)
  (fix
    ([loop (lambda (cur head tail)
      (if
        (#%null? (lref tail))
        (call (toplevel-ref false module-remove!) (lref cur)
         (lref head))
        (let
            ([cur
              (call (toplevel-ref false module-ref-submodule) (lref cur)
               (lref head))])
          (if
            (#%not (lref cur))
            (call (toplevel-ref false assertion-violation) (const nested-remove!)
             (const failed to resolve module)
             (lref names))
            (call (lref loop) (lref cur)
              (#%car (lref tail))
              (#%cdr (lref tail)))))))])
   (call (lref loop) (lref root)
    (#%car (lref names))
    (#%cdr (lref names))))))
 (define nested-ref-module (lambda (root names)
  (fix
    ([loop (lambda (cur names)
      (if
        (#%null? (lref names))
        (lref cur)
        (let
            ([cur
              (call (toplevel-ref false module-ref-submodule) (lref cur)
               (#%car (lref names)))])
          (let
             ([and-tmp
               (lref cur)])
           (if
             (lref and-tmp)
             (call (lref loop) (lref cur) (#%cdr (lref names)))
             (const false))))))])
   (call (lref loop) (lref root) (lref names)))))
 (define nested-define-module! (lambda (root names module)
  (if
    (#%null? (lref names))
    (call (toplevel-ref false assertion-violation) (const nested-define-module!)
     (const can't redefine root module)
     (lref module))
    (fix
       ([loop (lambda (cur head tail)
         (if
           (#%null? (lref tail))
           (call (toplevel-ref false module-define-submodule!) (lref cur)
            (lref head)
            (lref module))
           (let
               ([cur
                 (let
                    ([or-tmp
                      (call (toplevel-ref false module-ref-submodule) (lref cur)
                       (lref head))])
                  (if
                    (lref or-tmp)
                    (lref or-tmp)
                    (let
                        ([m
                          (call (toplevel-ref false make-module))])
                      (seq
                       (call (toplevel-ref false set-module-kind!) (lref m)
                        (const directory))
                       (call (toplevel-ref false set-module-name!) (lref m)
                        (#%append (call (toplevel-ref false module-name) (lref cur))
                         (#%cons (lref head) (const '()))))
                       (call (toplevel-ref false module-define-submodule!) (lref cur)
                        (lref head)
                        (lref m))
                       (lref m)))))])
             (call (lref loop) (lref cur)
              (#%car (lref tail))
              (#%cdr (lref tail))))))])
      (call (lref loop) (lref root)
       (#%car (lref names))
       (#%cdr (lref names)))))))
 (define local-ref (lambda (names)
  (call (toplevel-ref false nested-ref) (#%current-module)
   (lref names))))
 (define local-set! (lambda (names val)
  (call (toplevel-ref false nested-set!) (#%current-module)
   (lref names)
   (lref val))))
 (define local-define (lambda (names val)
  (call (toplevel-ref false nested-define!) (#%current-module)
   (lref names)
   (lref val))))
 (define local-remove (lambda (names)
  (call (toplevel-ref false nested-remove!) (#%current-module)
   (lref names))))
 (define local-ref-module (lambda (names)
  (call (toplevel-ref false nested-ref-module) (#%current-module)
   (lref names))))
 (define local-define-module (lambda (names mod)
  (call (toplevel-ref false nested-define-module!) (#%current-module)
   (lref names)
   (lref mod))))
 (define module-name (lambda (mod)
  (let
     ([or-tmp
       (call (toplevel-ref false raw-module-name) (lref mod))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (let
         ([name
           (#%cons (call (toplevel-ref false gensym)) (const '()))])
       (seq
        (call (toplevel-ref false set-module-name!) (lref mod)
         (lref name))
        (call (toplevel-ref false nested-define-module!) (call (toplevel-ref false resolve-module) (const '())
          (const false)
          (const true))
         (lref name)
         (lref mod))
        (call (toplevel-ref false raw-module-name) (lref mod))))))))
 (define make-modules-in (lambda (module name)
  (let
     ([or-tmp
       (call (toplevel-ref false nested-ref-module) (lref module)
        (lref name))])
   (if
     (lref or-tmp)
     (lref or-tmp)
     (let
         ([m
           (call (toplevel-ref false make-module))])
       (seq
        (call (toplevel-ref false set-module-kind!) (lref m)
         (const directory))
        (call (toplevel-ref false set-module-name!) (lref m)
         (#%append (call (toplevel-ref false module-name) (lref module))
          (lref name)))
        (call (toplevel-ref false nested-define-module!) (lref module)
         (lref name)
         (lref m))
        (lref m)))))))
 (define beautify-user-module! (lambda (module)
  (seq
   (let
      ([interface
        (call (toplevel-ref false module-public-interface) (lref module))])
    (if
      (let
          ([or-tmp
            (#%not (lref interface))])
        (if
          (lref or-tmp)
          (lref or-tmp)
          (#%eq? (lref interface) (lref module))))
      (let
         ([interface
           (call (toplevel-ref false make-module))])
       (seq
        (call (toplevel-ref false set-module-name!) (lref interface)
         (call (toplevel-ref false module-name) (lref module)))
        (call (toplevel-ref false set-module-kind!) (lref interface)
         (const interface))
        (call (toplevel-ref false set-module-public-interface!) (lref module)
         (lref interface))))
      (const #<undefined>)))
   (if
     (let
         ([and-tmp
           (#%not (call (module-ref (capy)::memq #t) (toplevel-ref false the-scm-module)
             (call (toplevel-ref false module-uses) (lref module))))])
       (if
         (lref and-tmp)
         (#%not (#%eq? (lref module)
           (toplevel-ref false the-root-module)))
         (const false)))
     (call (toplevel-ref false module-use!) (lref module)
      (toplevel-ref false the-scm-module))
     (const #<undefined>)))))
 (define make-fresh-user-module (lambda ()
  (let
     ([m
       (call (toplevel-ref false make-module))])
   (seq
    (call (toplevel-ref false beautify-user-module!) (lref m))
    (call (toplevel-ref false set-module-declarative!) (lref m)
     (const false))
    (lref m)))))
 (define resolve-module (let
    ([root
      (toplevel-ref false *resolve-module-root*)])
  (lambda (name autoload ensure)
   (let
      ([already
        (call (toplevel-ref false nested-ref-module) (lref root)
         (lref name))])
    (if
      (let
          ([and-tmp
            (lref already)])
        (if
          (lref and-tmp)
          (let
             ([or-tmp
               (#%not (lref autoload))])
           (if
             (lref or-tmp)
             (lref or-tmp)
             (call (toplevel-ref false module-public-interface) (lref already))))
          (const false)))
      (lref already)
      (if
         (lref autoload)
         (seq
          (call (toplevel-ref false try-module-autoload) (lref name))
          (call (toplevel-ref false resolve-module) (lref name)
           (const false)
           (lref ensure)))
         (let
             ([or-tmp
               (lref already)])
           (if
             (lref or-tmp)
             (lref or-tmp)
             (let
                 ([and-tmp
                   (lref ensure)])
               (if
                 (lref and-tmp)
                 (call (toplevel-ref false make-modules-in) (lref root)
                  (lref name))
                 (const false)))))))))))
 (define ->bool (lambda (x) (#%not (#%not (lref x)))))
 (define autoloads-in-progress (const '()))
 (define autoloads-done (const ((capy . capy))))
 (define autoload-done-or-in-progress? (lambda (p m)
  (let
     ([n
       (#%cons (lref p) (lref m))])
   (call (toplevel-ref false ->bool) (let
       ([or-tmp
         (call (toplevel-ref false member) (lref n)
          (toplevel-ref false autoloads-done))])
     (if
       (lref or-tmp)
       (lref or-tmp)
       (call (toplevel-ref false member) (lref n)
         (toplevel-ref false autoloads-in-progress))))))))
 (define autoload-done! (lambda (p m)
  (let
     ([n
       (#%cons (lref p) (lref m))])
   (seq
    (toplevel-set autoloads-in-progress (call (toplevel-ref false delete!) (lref n)
     (toplevel-ref false autoloads-in-progress)))
    (let
       ([or-tmp
         (call (toplevel-ref false member) (lref n)
          (toplevel-ref false autoloads-done))])
     (if
       (lref or-tmp)
       (lref or-tmp)
       (toplevel-set autoloads-done (#%cons (lref n)
         (toplevel-ref false autoloads-done)))))))))
 (define autoload-in-progress! (lambda (p m)
  (let
     ([n
       (#%cons (lref p) (lref m))])
   (seq
    (toplevel-set autoloads-done (call (toplevel-ref false delete!) (lref n)
     (toplevel-ref false autoloads-done)))
    (toplevel-set autoloads-in-progress (#%cons (lref n)
     (toplevel-ref false autoloads-in-progress)))))))
 (define set-autoloaded! (lambda (p m done?)
  (if
    (lref done?)
    (call (toplevel-ref false autoload-done!) (lref p) (lref m))
    (let
        ([n
          (#%cons (lref p) (lref m))])
      (seq
       (toplevel-set autoloads-done (call (toplevel-ref false delete!) (lref n)
        (toplevel-ref false autoloads-done)))
       (toplevel-set autoloads-in-progress (call (toplevel-ref false delete!) (lref n)
        (toplevel-ref false autoloads-in-progress))))))))
 (define try-module-autoload (lambda (module-name)
  (let
     ([reverse-name
       (call (toplevel-ref false reverse) (lref module-name))])
   (let
      ([name
        (#%symbol->string (#%car (lref reverse-name)))])
    (let
       ([dir-hint-module-name
         (call (toplevel-ref false reverse) (#%cdr (lref reverse-name)))])
     (let
        ([dir-hint
          (call (module-ref (capy)::apply #t) (toplevel-ref false string-append)
           (call (toplevel-ref false map) (lambda (elt)
             (call (toplevel-ref false string-append) (#%symbol->string (lref elt))
              (const /)))
            (lref dir-hint-module-name)))])
      (seq
       (call (toplevel-ref false resolve-module) (lref dir-hint-module-name)
        (const false)
        (const true))
       (let
          ([and-tmp
            (#%not (call (toplevel-ref false autoload-done-or-in-progress?) (lref dir-hint)
              (lref name)))])
        (if
          (lref and-tmp)
          (let
             ([didit
               (const false)])
           (let
              ([&didit
                (call (module-ref (capy)::make-variable #t) (lref didit))])
            (seq
             (call (toplevel-ref false dynamic-wind) (lambda ()
               (call (toplevel-ref false autoload-in-progress!) (lref dir-hint)
                (lref name)))
              (lambda ()
               (call (toplevel-ref false save-module-excursion) (lambda ()
                 (seq
                  (#%current-module (call (toplevel-ref false make-fresh-user-module)))
                  (call (toplevel-ref false call/cc) (lambda (return)
                    (call (toplevel-ref false with-exception-handler) (lambda (_x)
                      (seq
                       (call (toplevel-ref false print) (const FAIL!))
                       (call (toplevel-ref false print) (const Autoload of )
                        (lref module-name)
                        (const  failed: )
                        (call (toplevel-ref false condition-message) (lref _x))
                        (const  irritants: )
                        (call (toplevel-ref false condition-irritants) (lref _x)))
                       (call (lref return) (const false))))
                     (lambda ()
                      (seq
                       (call (toplevel-ref false load) (call (toplevel-ref false string-append) (lref dir-hint)
                         (lref name)))
                       (call (module-ref (capy)::variable-set! #t) (lref &didit)
                        (const true)))))))))))
              (lambda ()
               (call (toplevel-ref false set-autoloaded!) (lref dir-hint)
                (lref name)
                (call (module-ref (capy)::variable-ref #t) (lref &didit)))))
             (call (module-ref (capy)::variable-ref #t) (lref &didit)))))
          (const false))))))))))
 (define identity (lambda (x) (lref x)))
 (define resolve-interface (lambda (name select hide prefix)
  (let
     ([mod
       (call (toplevel-ref false resolve-module) (lref name)
        (const true)
        (const false))])
   (let
      ([public-i
        (let
           ([and-tmp
             (lref mod)])
         (if
           (lref and-tmp)
           (call (toplevel-ref false module-public-interface) (lref mod))
           (const false)))])
    (let
       ([renamer
         (if
           (lref prefix)
           (lambda (symbol)
            (call (toplevel-ref false symbol-append) (lref prefix)
             (lref symbol)))
           (toplevel-ref false identity))])
     (seq
      (if
        (#%not (lref public-i))
        (call (toplevel-ref false assertion-violation) (const resolve-interface)
         (const no code for module)
         (lref name))
        (const #<undefined>))
      (if
        (let
            ([and-tmp
              (#%not (lref select))])
          (if
            (lref and-tmp)
            (let
               ([and-tmp
                 (#%null? (lref hide))])
             (if
               (lref and-tmp)
               (#%eq? (lref renamer) (toplevel-ref false identity))
               (const false)))
            (const false)))
        (lref public-i)
        (let
            ([custom-i
              (call (toplevel-ref false make-module))])
          (fix
            ([maybe-export! (lambda (src dst var)
              (if
                (#%not (call (module-ref (capy)::memq #t) (lref src)
                   (lref hide)))
                (let
                   ([name
                     (call (lref renamer) (lref dst))])
                 (seq
                  (if
                    (call (toplevel-ref false core-hash-ref) (call (toplevel-ref false module-replacements) (lref public-i))
                      (lref src))
                    (call (toplevel-ref false core-hash-put!) (call (toplevel-ref false module-replacements) (lref custom-i))
                     (lref name)
                     (const true))
                    (const #<undefined>))
                  (call (toplevel-ref false module-add!) (lref custom-i)
                   (lref name)
                   (lref var))))
                (const #<undefined>)))])
           (seq
            (call (toplevel-ref false set-module-kind!) (lref custom-i)
             (const custom-interface))
            (call (toplevel-ref false set-module-name!) (lref custom-i)
             (lref name))
            (call (toplevel-ref false for-each) (lambda (binding)
              (if
                (#%not (call (toplevel-ref false module-local-variable) (lref public-i)
                   (lref binding)))
                (call (toplevel-ref false assertion-violation) (const false)
                 (const no binding to hide in module)
                 (lref name)
                 (lref binding))
                (const #<undefined>)))
             (lref hide))
            (let
               ([cond-test-tmp
                 (lref select)])
             (if
               (lref cond-test-tmp)
               (call (toplevel-ref false for-each) (lambda (bspec)
                 (let
                    ([direct?
                      (call (module-ref (capy)::symbol? #t) (lref bspec))])
                  (let
                     ([orig
                       (if
                         (lref direct?)
                         (lref bspec)
                         (#%car (lref bspec)))])
                   (let
                      ([seen
                        (if
                          (lref direct?)
                          (lref bspec)
                          (#%cdr (lref bspec)))])
                    (let
                       ([var
                         (call (toplevel-ref false module-local-variable) (lref public-i)
                          (lref orig))])
                     (seq
                      (if
                        (#%not (lref var))
                        (call (toplevel-ref false assertion-violation) (const unbound-variable)
                         (const no binding to select in module)
                         (lref orig)
                         (lref name))
                        (const #<undefined>))
                      (call (lref maybe-export!) (lref orig)
                       (lref seen)
                       (lref var))))))))
                (lref select))
               (call (toplevel-ref false module-for-each) (lambda (sym var)
                  (call (lref maybe-export!) (lref sym)
                   (lref sym)
                   (lref var)))
                 (lref public-i))))
            (lref custom-i)))))))))))
 (define define-module* (lambda (name)
  (let
     ([module
       (call (toplevel-ref false resolve-module) (lref name)
        (const false)
        (const true))])
   (seq
    (call (toplevel-ref false beautify-user-module!) (lref module))
    (lref module)))))
 (define module-export! (lambda (m names replace?)
  (let
     ([replace?
       (if
         (#%null? (lref replace?))
         (const false)
         (#%car (lref replace?)))]
     [public-i
       (call (toplevel-ref false module-public-interface) (lref m))])
   (call (toplevel-ref false for-each) (lambda (name)
     (let
        ([internal-name
          (if
            (#%pair? (lref name))
            (#%car (lref name))
            (lref name))])
      (let
         ([external-name
           (if
             (#%pair? (lref name))
             (#%cdr (lref name))
             (lref name))])
       (let
          ([var
            (call (toplevel-ref false module-ensure-local-variable!) (lref m)
             (lref internal-name))])
        (seq
         (if
           (lref replace?)
           (call (toplevel-ref false core-hash-put!) (call (toplevel-ref false module-replacements) (lref public-i))
            (lref external-name)
            (const true))
           (const #<undefined>))
         (call (toplevel-ref false module-add!) (lref public-i)
          (lref external-name)
          (lref var)))))))
    (lref names)))))
 (define module-replace! (lambda (m names)
  (call (toplevel-ref false module-export!) (lref m)
   (lref names)
   (const true))))
 (define module-export-all! (lambda (mod)
  (fix
    ([fresh-interface! (lambda ()
      (let
         ([iface
           (call (toplevel-ref false make-module))])
       (seq
        (call (toplevel-ref false set-module-name!) (lref iface)
         (call (toplevel-ref false module-name) (lref mod)))
        (call (toplevel-ref false set-module-version!) (lref iface)
         (call (toplevel-ref false module-version) (lref mod)))
        (call (toplevel-ref false set-module-kind!) (lref iface)
         (const interface))
        (call (toplevel-ref false set-module-public-interface!) (lref mod)
         (lref iface))
        (lref iface))))])
   (let
      ([iface
        (let
           ([or-tmp
             (call (toplevel-ref false module-public-interface) (lref mod))])
         (if
           (lref or-tmp)
           (lref or-tmp)
           (call (lref fresh-interface!))))])
    (call (toplevel-ref false set-module-obarray!) (lref iface)
     (call (toplevel-ref false module-obarray) (lref mod)))))))
 (define process-use-modules (lambda (module-iface-args)
  (let
     ([interfaces
       (call (toplevel-ref false map) (lambda (mif-args)
         (let
            ([or-tmp
              (call (module-ref (capy)::apply #t) (toplevel-ref false resolve-interface)
               (lref mif-args))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (call (toplevel-ref false assertion-violation) (const use)
              (const failed to resolve module)
              (lref mif-args)))))
        (lref module-iface-args))])
   (call (toplevel-ref false module-use-interfaces!) (#%current-module)
    (lref interfaces)))))
 (define call-with-values (lambda (producer consumer)
  (receive ( . results)
    (call (lref producer))
    (call (module-ref (capy)::apply #t) (lref consumer)
      (lref results)))))
 (define cadr (lambda (x) (#%car (#%cdr (lref x)))))
 (define cddr (lambda (x) (#%cdr (#%cdr (lref x)))))
 (define cdar (lambda (x) (#%cdr (#%car (lref x)))))
 (define caar (lambda (x) (#%car (#%car (lref x)))))
 (define caddr (lambda (x) (#%car (#%cdr (#%cdr (lref x))))))
 (define cdddr (lambda (x) (#%cdr (#%cdr (#%cdr (lref x))))))
 (define lookup-bound (lambda (module name public?)
  (let
     ([mod
       (call (toplevel-ref false resolve-module) (lref module)
        (const false)
        (const false))])
   (seq
    (if
      (#%not (lref mod))
      (call (toplevel-ref false assertion-violation) (const lookup-bound)
       (const module not found)
       (lref module))
      (const #<undefined>))
    (let
       ([iface
         (if
           (lref public?)
           (call (toplevel-ref false module-public-interface) (lref mod))
           (lref mod))])
     (let
        ([var
          (call (toplevel-ref false module-variable) (lref iface)
           (lref name))])
      (seq
       (if
         (let
             ([or-tmp
               (#%not (lref var))])
           (if
             (lref or-tmp)
             (lref or-tmp)
             (#%not (call (module-ref (capy)::variable-bound? #t) (lref var)))))
         (call (toplevel-ref false assertion-violation) (const lookup-bound)
          (const unbound variable)
          (lref module)
          (lref name))
         (const #<undefined>))
       (lref var))))))))
 (define module-re-export! (lambda (m names replace?)
  (let
     ([replace?
       (if
         (#%null? (lref replace?))
         (const false)
         (#%car (lref replace?)))])
   (let
      ([public-i
        (call (toplevel-ref false module-public-interface) (lref m))])
    (call (toplevel-ref false for-each) (lambda (name)
      (let
         ([internal-name
           (if
             (#%pair? (lref name))
             (#%car (lref name))
             (lref name))])
       (let
          ([external-name
            (if
              (#%pair? (lref name))
              (#%cdr (lref name))
              (lref name))])
        (let
           ([var
             (call (toplevel-ref false module-variable) (lref m)
              (toplevel-ref false interna-name))])
         (let
            ([cond-test-tmp
              (#%not (lref var))])
          (if
            (lref cond-test-tmp)
            (call (toplevel-ref false assertion-violation) (const unbound-variable)
             (const undefined variable)
             (lref internal-name))
            (let
                ([cond-test-tmp
                  (#%eq? (lref var)
                   (call (toplevel-ref false module-local-variable) (lref m)
                    (lref internal-name)))])
              (if
                (lref cond-test-tmp)
                (call (toplevel-ref false assertion-violation) (const export)
                 (const re-exporting local variable)
                 (lref internal-name))
                (seq
                  (if
                    (lref replace?)
                    (call (toplevel-ref false core-hash-put!) (call (toplevel-ref false module-replacements) (lref public-i))
                     (lref external-name)
                     (const true))
                    (const #<undefined>))
                  (call (toplevel-ref false module-add!) (lref public-i)
                   (lref external-name)
                   (lref var)))))))))))
     (lref names))))))
 (define r6rs:bytevector-copy! (lambda (source source-start target target-start count)
  (if
    (#%>= (lref source-start) (lref target-start))
    (fix
      ([loop (lambda (i)
        (if
          (#%< (lref i) (lref count))
          (seq
           (call (module-ref (capy)::bytevector-u8-set! #t) (lref target)
            (#%+ (lref target-start) (lref i))
            (call (module-ref (capy)::bytevector-u8-ref #t) (lref source)
             (#%+ (lref source-start) (lref i))))
           (call (lref loop) (#%+ (lref i) (const 1))))
          (const #<undefined>)))])
     (call (lref loop) (const 0)))
    (fix
       ([loop (lambda (i)
         (if
           (#%>= (lref i) (const 0))
           (seq
            (call (module-ref (capy)::bytevector-u8-set! #t) (lref target)
             (#%+ (lref target-start) (lref i))
             (call (module-ref (capy)::bytevector-u8-ref #t) (lref source)
              (#%+ (lref source-start) (lref i))))
            (call (lref loop) (#%- (lref i) (const 1))))
           (const #<undefined>)))])
      (call (lref loop) (#%- (lref count) (const 1)))))))
 (define bytevector-copy (lambda (b rest)
  (let
     ([n
       (call (module-ref (capy)::bytevector-length #t) (lref b))])
   (let
      ([start
        (if  (#%null? (lref rest))  (const 0)  (#%car (lref rest)))])
    (let
       ([end
         (if
           (let
               ([or-tmp
                 (#%null? (lref rest))])
             (if
               (lref or-tmp)
               (lref or-tmp)
               (#%null? (#%cdr (lref rest)))))
           (lref n)
           (#%car (#%cdr (lref rest))))])
     (let
        ([k
          (#%- (lref end) (lref start))])
      (let
         ([b2
           (call (toplevel-ref false make-bytevector) (lref k))])
       (seq
        (call (toplevel-ref false r6rs:bytevector-copy!) (lref b)
         (lref start)
         (lref b2)
         (const 0)
         (lref k))
        (lref b2)))))))))
 (define assert (lambda (cond rest)
  (if
    (#%not (lref cond))
    (call (module-ref (capy)::apply #t) (toplevel-ref false assertion-violation)
     (const assert)
     (const assertion failed)
     (lref rest))
    (const true))))
 (define procedure-property (lambda (proc key)
  (let
     ([p
       (call (toplevel-ref false assq) (lref key)
        (call (toplevel-ref false procedure-properties) (lref proc)))])
   (let
      ([and-tmp
        (lref p)])
    (if  (lref and-tmp)  (#%cdr (lref p))  (const false))))))
 (define procedure-name (lambda (proc)
  (call (toplevel-ref false procedure-property) (lref proc)
   (const name))))
 (define procedure-documentation (lambda (proc)
  (call (toplevel-ref false procedure-property) (lref proc)
   (const documentation))))
 (define procedure-sourcev (lambda (proc)
  (let
     ([src
       (call (toplevel-ref false procedure-property) (lref proc)
        (const source))])
   (let
      ([cond-test-tmp
        (#%vector? (lref src))])
    (if
      (lref cond-test-tmp)
      (lref src)
      (let
          ([cond-test-tmp
            (#%list? (lref src))])
        (if
          (lref cond-test-tmp)
          (let
             ([file
               (call (toplevel-ref false assq) (const filename)
                (lref src))]
             [line
               (call (toplevel-ref false assq) (const line)
                (lref src))]
             [col
               (call (toplevel-ref false assq) (const column)
                (lref src))])
           (let
              ([vec
                (#%make-vector (const 3) (const #<undefined>))])
            (seq
             (#%vector-set! (lref vec) (const 0) (lref file))
             (#%vector-set! (lref vec) (const 1) (lref line))
             (#%vector-set! (lref vec) (const 2) (lref col))
             (lref vec))))
          (const false))))))))
 (define call-without-interrupts (lambda (thunk) (call (lref thunk))))
 (call (toplevel-ref false load) (const boot/expand.scm))
 (call (toplevel-ref false load) (const boot/interpreter.scm))
 (call (toplevel-ref false load) (const boot/psyntax.scm))
 (call (toplevel-ref false load) (const boot/sys.scm))
 (call (toplevel-ref false load) (const boot/osdep.scm))
 (call (toplevel-ref false load) (const boot/iosys.scm))
 (call (toplevel-ref false load) (const boot/portio.scm))
 (call (toplevel-ref false load) (const boot/bytevectorio.scm))
 (call (toplevel-ref false load) (const boot/fileio.scm))
 (call (toplevel-ref false load) (const boot/conio.scm))
 (call (toplevel-ref false load) (const boot/stringio.scm))
 (call (toplevel-ref false load) (const boot/stdio.scm))
 (call (toplevel-ref false load) (const boot/print.scm))
 (call (toplevel-ref false initialize-io-system))
 (call (toplevel-ref false load) (const boot/reader.scm))
 (call (toplevel-ref false load) (const boot/set.scm))
 (call (toplevel-ref false load) (const boot/eval.scm)))
