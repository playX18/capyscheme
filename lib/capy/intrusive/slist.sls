(library (capy intrusive slist)
  (export define-instrusive-slist)
  (import (scheme base)
          (rnrs syntax-case))

  (define-syntax define-intrusive-slist
    (lambda (x)
      (syntax-case x ()
        ((_ prefix get-next set-next!)
        (with-syntax
            ((make-list       (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-make"))))
              (push-front!     (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-push-front!"))))
              (pop-front!      (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-pop-front!"))))
              (front           (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-front"))))
              (empty?          (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-empty?"))))
              (iterate         (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-iterate"))))
              (iter-type       (datum->syntax #'prefix 
                                (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-head")))))
          #'(begin
              ;; 1. The List Header
              ;; In Boost, the slist object holds the 'head' pointer. 
              ;; We define a simple record to act as this container.
              (define-record-type iter-type
                (make-list-internal head)
                list-head?
                (head list-head-ref list-head-set!))

              ;; 2. Constructor
              (define (make-list)
                (make-list-internal #f))

              ;; 3. Empty Check
              (define (empty? lst)
                (not (list-head-ref lst)))

              ;; 4. Front Accessor
              (define (front lst)
                (list-head-ref lst))

              ;; 5. Push Front (O(1))
              ;; We set the NEW node's next to the CURRENT head.
              ;; Then update the list head to point to the NEW node.
              (define (push-front! lst node)
                (set-next! node (list-head-ref lst))
                (list-head-set! lst node))

              ;; 6. Pop Front (O(1))
              (define (pop-front! lst)
                (let ((first-node (list-head-ref lst)))
                  (when first-node
                    (let ((second-node (get-next first-node)))
                      (set-next! first-node #f) ;; Clean up the detached node
                      (list-head-set! lst second-node)))
                  first-node))
              
              ;; 7. Iterator (for-each style)
              (define (iterate lst proc)
                (let loop ((current (list-head-ref lst)))
                  (when current
                    (let ((next-node (get-next current))) ;; Save next in case proc mutates/removes
                      (proc current)
                      (loop next-node))))))))))))