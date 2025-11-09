;; Perform GC so that heap is as small as possible
(collect-garbage)
;; Dump the heap to a file
(dump-heap "capy.heap" (variable-ref (lookup-bound '(boot cli) 'enter #t)))