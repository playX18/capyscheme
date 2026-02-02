(library (capy intrusive dlist)
  (export define-intrusive-dlist)
  (import (scheme base)
          (rnrs syntax-case))

  (define-syntax define-intrusive-dlist
    (lambda (x)
      (syntax-case x ()
        ((_ prefix get-next set-next! get-prev set-prev!)
        (with-syntax
            ((make-list       (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-make"))))
             (push-front!     (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-push-front!"))))
             (push-back!      (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-push-back!"))))
             (pop-front!      (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-pop-front!"))))
             (pop-back!       (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-pop-back!"))))
             (remove!         (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-remove!"))))
             (front           (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-front"))))
             (back            (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-back"))))
             (empty?          (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-empty?"))))
             (iterate         (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-iterate"))))
             (list-type       (datum->syntax #'prefix (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-container"))))
             )
          #'(begin
              ;; 1. The List Container (Head and Tail)
              (define-record-type list-type
                (make-list-internal head tail)
                list-container?
                (head list-head list-head-set!)
                (tail list-tail list-tail-set!))

              (define (make-list)
                (make-list-internal #f #f))

              (define (empty? lst)
                (not (list-head lst)))

              (define (front lst) (list-head lst))
              (define (back lst)  (list-tail lst))

              ;; 2. Push Front
              (define (push-front! lst node)
                (let ((old-head (list-head lst)))
                  (set-next! node old-head)
                  (set-prev! node #f)
                  (if old-head
                      (set-prev! old-head node)
                      (list-tail-set! lst node)) ;; If list was empty, new node is also tail
                  (list-head-set! lst node)))

              ;; 3. Push Back
              (define (push-back! lst node)
                (let ((old-tail (list-tail lst)))
                  (set-prev! node old-tail)
                  (set-next! node #f)
                  (if old-tail
                      (set-next! old-tail node)
                      (list-head-set! lst node)) ;; If list was empty, new node is also head
                  (list-tail-set! lst node)))

              ;; 4. Remove (Arbitrary Node)
              ;; This is the "killer feature" of dlist. 
              ;; We can remove a node just by having the node and the list.
              (define (remove! lst node)
                (let ((prev-node (get-prev node))
                      (next-node (get-next node)))
                  
                  ;; Unlink from Previous
                  (if prev-node
                      (set-next! prev-node next-node)
                      (list-head-set! lst next-node)) ;; If no prev, we are removing Head
                  
                  ;; Unlink from Next
                  (if next-node
                      (set-prev! next-node prev-node)
                      (list-tail-set! lst prev-node)) ;; If no next, we are removing Tail
                  
                  ;; Clean up the removed node pointers
                  (set-next! node #f)
                  (set-prev! node #f)))

              ;; 5. Pop Front
              (define (pop-front! lst)
                (let ((h (list-head lst)))
                  (when h (remove! lst h))
                  h))

              ;; 6. Pop Back
              (define (pop-back! lst)
                (let ((t (list-tail lst)))
                  (when t (remove! lst t))
                  t))

              ;; 7. Iterator
              (define (iterate lst proc)
                (let loop ((current (list-head lst)))
                  (when current
                    (let ((next-node (get-next current)))
                      (proc current)
                      (loop next-node))))))))))))