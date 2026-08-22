(library (capy graph)
  (export
    make-graph
    make-digraph
    make-undirected-graph
    graph?
    graph-directed?
    graph-add-vertex!
    graph-add-edge!
    graph-vertex-count
    graph-edge-count
    graph-vertex-weight
    graph-vertex-payload
    graph-edge-weight
    graph-edge-source
    graph-edge-target
    graph-edge-endpoints
    graph-contains-edge?
    graph-find-edge
    graph-for-each-successor
    graph-for-each-edge
    graph-for-each-vertex
    graph-for-each-edge-all
    graph-successors
    graph-neighbors
    graph-edges
    tarjan-scc
    toposort
    cyclic-directed?
    connected-components
    min-spanning-tree
    dijkstra
    condensation
    graph-dfs-preorder
    graph-bfs
    graph-copy)
  (import
    (rnrs)
    (rnrs hashtables)
    (capy)
    (capy binary-heap)
    (srfi :214))

  (define-record-type (<graph> %make-graph graph?)
    (fields
      (immutable directed? graph-directed?)
      (immutable node-weights graph-node-weights)
      (immutable edge-from graph-edge-from)
      (immutable edge-to graph-edge-to)
      (immutable edge-weights graph-edge-weights)
      (immutable adjacency graph-adjacency)))

  (define (make-graph)
    (%make-graph #t
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)))

  (define make-digraph make-graph)

  (define (make-undirected-graph)
    (%make-graph #f
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)
      (make-flexvector 0)))

  (define (graph-add-vertex! g weight)
    (flexvector-add-back! (graph-node-weights g) weight)
    (flexvector-add-back! (graph-adjacency g) #f)
    (- (flexvector-length (graph-node-weights g)) 1))

  ;; Append edge index E to vertex V's adjacency, allocating the node's
  ;; flexvector on first use.
  (define (graph-push-adj! g v e)
    (define adj (graph-adjacency g))
    (define node-adj (flexvector-ref adj v))
    (if node-adj
      (flexvector-add-back! node-adj e)
      (flexvector-set! adj v (flexvector e))))

  (define (graph-add-edge! g from to . weight-opt)
    (let ([e (flexvector-length (graph-edge-from g))])
      (flexvector-add-back! (graph-edge-from g) from)
      (flexvector-add-back! (graph-edge-to g) to)
      (flexvector-add-back! (graph-edge-weights g)
        (if (pair? weight-opt) (car weight-opt) #f))
      (graph-push-adj! g from e)
      (unless (graph-directed? g)
        (graph-push-adj! g to e))
      e))


  (define (graph-vertex-count g)
    (flexvector-length (graph-node-weights g)))

  (define (graph-edge-count g)
    (flexvector-length (graph-edge-from g)))

  (define (graph-vertex-weight g v)
    (flexvector-ref (graph-node-weights g) v))

  (define graph-vertex-payload graph-vertex-weight)

  (define (graph-edge-weight g e)
    (flexvector-ref (graph-edge-weights g) e))

  (define (graph-edge-source g e)
    (flexvector-ref (graph-edge-from g) e))

  (define (graph-edge-target g e)
    (flexvector-ref (graph-edge-to g) e))

  (define (graph-edge-endpoints g e)
    (values (graph-edge-source g e) (graph-edge-target g e)))

  (define (graph-contains-edge? g a b)
    (let ([found? #f])
      (graph-for-each-edge g a
        (lambda (e t)
          (when (and (not found?) (= t b))
            (set! found? #t))))
      found?))

  (define (graph-find-edge g a b)
    (let ([result #f])
      (graph-for-each-edge g a
        (lambda (e t)
          (when (and (not result) (= t b))
            (set! result e))))
      result))

  ;; The successor reached by following edge E from vertex V.  For
  ;; directed edges E only appears in FROM's adjacency (so V = from);
  ;; undirected edges appear in both endpoints' lists, so the neighbor is
  ;; the endpoint other than V.
  (define (graph-successor-of g v e)
    (let ([from (flexvector-ref (graph-edge-from g) e)]
          [to (flexvector-ref (graph-edge-to g) e)])
      (if (= v from) to from)))

  (define (graph-for-each-successor g v proc)
    (define adj (flexvector-ref (graph-adjacency g) v))
    (when adj
      (if (graph-directed? g)
        (let ([to (graph-edge-to g)]
              [n (flexvector-length adj)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (proc (flexvector-ref to (flexvector-ref adj i)))))
        (let ([n (flexvector-length adj)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (proc (graph-successor-of g v (flexvector-ref adj i))))))))

  (define (graph-for-each-edge g v proc)
    (define adj (flexvector-ref (graph-adjacency g) v))
    (when adj
      (if (graph-directed? g)
        (let ([n (flexvector-length adj)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (let ([e (flexvector-ref adj i)])
              (proc e (flexvector-ref (graph-edge-to g) e)))))
        (let ([n (flexvector-length adj)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (let ([e (flexvector-ref adj i)])
              (proc e (graph-successor-of g v e))))))))

  (define (graph-for-each-vertex g proc)
    (let ([n (graph-vertex-count g)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (proc i))))

  (define (graph-for-each-edge-all g proc)
    (let ([m (graph-edge-count g)])
      (do ([i 0 (+ i 1)]) ((= i m))
        (proc i))))

  (define (graph-successors g v)
    (let ([out '()])
      (graph-for-each-successor g v (lambda (t) (set! out (cons t out))))
      (reverse out)))

  (define (graph-neighbors g v)
    (let ([out (make-flexvector 0)])
      (graph-for-each-successor g v
        (lambda (t) (flexvector-add-back! out t)))
      (flexvector->vector out)))

  (define (graph-edges g v)
    (let ([out (make-flexvector 0)])
      (graph-for-each-edge g v
        (lambda (e t) (flexvector-add-back! out e)))
      (flexvector->vector out)))

  (define (merge-sort-fixnums lst)
    (define (merge a b)
      (cond
        [(null? a) b]
        [(null? b) a]
        [(< (car a) (car b))
          (cons (car a) (merge (cdr a) b))]
        [else (cons (car b) (merge a (cdr b)))]))
    (define (split lst)
      (let loop ([fast lst] [slow lst] [acc '()])
        (if (or (null? fast) (null? (cdr fast)))
          (values (reverse acc) slow)
          (loop (cddr fast) (cdr slow) (cons (car slow) acc)))))
    (if (or (null? lst) (null? (cdr lst)))
      lst
      (receive (left right) (split lst)
        (merge (merge-sort-fixnums left) (merge-sort-fixnums right)))))

  ;; Merge sort with an explicit comparator (used for edge lists).
  (define (merge-sort-with cmp lst)
    (define (merge a b)
      (cond
        [(null? a) b]
        [(null? b) a]
        [(cmp (car a) (car b))
          (cons (car a) (merge (cdr a) b))]
        [else (cons (car b) (merge a (cdr b)))]))
    (define (split lst)
      (let loop ([fast lst] [slow lst] [acc '()])
        (if (or (null? fast) (null? (cdr fast)))
          (values (reverse acc) slow)
          (loop (cddr fast) (cdr slow) (cons (car slow) acc)))))
    (if (or (null? lst) (null? (cdr lst)))
      lst
      (receive (left right) (split lst)
        (merge (merge-sort-with cmp left) (merge-sort-with cmp right)))))

  (define (iota n)
    (let loop ([i (- n 1)] [acc '()])
      (if (< i 0) acc (loop (- i 1) (cons i acc)))))

  ;; Tarjan SCC
  (define (tarjan-scc g)
    (define n (graph-vertex-count g))
    (define index (make-vector n #f))
    (define lowlink (make-vector n #f))
    (define onstack (make-vector n #f))
    (define stack '())
    (define sccs '())
    (define idx 0)
    (define (visit v)
      (vector-set! index v idx)
      (vector-set! lowlink v idx)
      (set! idx (+ idx 1))
      (set! stack (cons v stack))
      (vector-set! onstack v #t)
      (let ([adj (flexvector-ref (graph-adjacency g) v)]
            [to (graph-edge-to g)])
        (when adj
          (let ([m (flexvector-length adj)]
                [directed? (graph-directed? g)])
            (do ([i 0 (+ i 1)]) ((= i m))
              (let* ([e (flexvector-ref adj i)]
                     [w (if directed? (flexvector-ref to e) (graph-successor-of g v e))])
                (cond
                  [(not (vector-ref index w))
                   (visit w)
                   (vector-set! lowlink v (min (vector-ref lowlink v) (vector-ref lowlink w)))]
                  [(vector-ref onstack w)
                   (vector-set! lowlink v (min (vector-ref lowlink v) (vector-ref index w)))]))))))
      (when (= (vector-ref lowlink v) (vector-ref index v))
        (let loop ([scc '()])
          (let ([w (car stack)])
            (set! stack (cdr stack))
            (vector-set! onstack w #f)
            (let ([scc (cons w scc)])
              (if (= w v)
                (set! sccs (cons scc sccs))
                (loop scc)))))))
    (do ([v 0 (+ v 1)]) ((= v n))
      (when (not (vector-ref index v))
        (visit v)))
    (map merge-sort-fixnums (reverse sccs)))

  ;; Topological sort (Kahn's algorithm).  Returns the vertex order, or #f
  ;; when the graph contains a cycle.
  (define (toposort g)
    (define n (graph-vertex-count g))
    (define indeg (make-vector n 0))
    (define ready '())
    (let ([from (graph-edge-from g)]
          [to (graph-edge-to g)]
          [m (graph-edge-count g)])
      (do ([i 0 (+ i 1)]) ((= i m))
        (let ([t (flexvector-ref to i)])
          (vector-set! indeg t (+ (vector-ref indeg t) 1)))))
    (let loop ([v 0])
      (when (< v n)
        (when (= (vector-ref indeg v) 0)
          (set! ready (cons v ready)))
        (loop (+ v 1))))
    (let loop ([ready ready] [order '()] [count 0])
      (if (null? ready)
        (if (= count n) (reverse order) #f)
        (let ([v (car ready)]
              [rest (cdr ready)])
          (graph-for-each-successor g v
            (lambda (t)
              (let ([d (- (vector-ref indeg t) 1)])
                (vector-set! indeg t d)
                (when (= d 0)
                  (set! rest (cons t rest))))))
          (loop rest (cons v order) (+ count 1))))))

  (define (cyclic-directed? g)
    (not (toposort g)))

  ;; Union-find over fixnum vertices.
  (define (make-union-find n)
    (define parent (make-vector n))
    (let loop ([i 0])
      (when (< i n)
        (vector-set! parent i i)
        (loop (+ i 1))))
    (define (find x)
      (let loop ([x x])
        (if (= (vector-ref parent x) x)
          x
          (loop (vector-ref parent x)))))
    (define (union a b)
      (let ([ra (find a)]
            [rb (find b)])
        (unless (= ra rb)
          (vector-set! parent rb ra))))
    (values find union))

  ;; Connected components of an undirected graph (edges are treated
  ;; symmetrically).  Returns a list of vertex lists in deterministic
  ;; discovery order.
  (define (connected-components g)
    (define n (graph-vertex-count g))
    (let-values ([(find union) (make-union-find n)])
      (let ([roots (make-eqv-hashtable)])
        (let ([m (graph-edge-count g)])
          (do ([i 0 (+ i 1)]) ((= i m))
            (union (graph-edge-source g i) (graph-edge-target g i))))
        (let loop ([v 0])
          (when (< v n)
            (let ([r (find v)])
              (hashtable-set! roots r (cons v (hashtable-ref roots r '()))))
            (loop (+ v 1))))
        (let ([out '()])
          (for-each
            (lambda (r)
              (set! out (cons (reverse (hashtable-ref roots r '())) out)))
            (vector->list (hashtable-keys roots)))
          out))))

  ;; Kruskal's minimum spanning tree.  Returns the list of edge indices
  ;; forming an MST (deterministic tie-break by edge index).  Unweighted
  ;; edges (weight #f) sort after any numeric weight.
  (define (min-spanning-tree g)
    (define n (graph-vertex-count g))
    (define m (graph-edge-count g))
    ;; Edge comparator: (weight, index); #f weight sorts last.
    (define (edge< a b)
      (let ([wa (graph-edge-weight g a)]
            [wb (graph-edge-weight g b)])
        (cond
          [(and wa wb) (or (< wa wb) (and (= wa wb) (< a b)))]
          [(and (not wa) (not wb)) (< a b)]
          [wa #t]            ; numeric < #f
          [else #f])))       ; #f > numeric
    (define sorted (merge-sort-with edge< (iota m)))
    (if (<= m 0)
      '()
      (receive (find union) (make-union-find n)
        (let loop ([es sorted] [out '()] [count 0])
          (if (or (null? es) (= count (- n 1)))
            (reverse out)
            (let ([e (car es)])
              (let ([a (graph-edge-source g e)]
                    [b (graph-edge-target g e)])
                (if (not (= (find a) (find b)))
                  (begin
                    (union a b)
                    (loop (cdr es) (cons e out) (+ count 1)))
                  (loop (cdr es) out count)))))))))

  ;; Dijkstra's shortest paths from SOURCE.  Returns (values dist pred):
  ;; dist is a vector of distances (#f for unreachable), pred a vector of
  ;; predecessors (#f for the source/unreachable).  Unweighted edges
  ;; (weight #f) count as 1.
  (define (dijkstra g source)
    (define n (graph-vertex-count g))
    (define dist (make-vector n #f))
    (define pred (make-vector n #f))
    (define heap (make-heap (lambda (a b) (< (car a) (car b))) 16))
    (vector-set! dist source 0)
    (heap-push! heap (cons 0 source))
    (let loop ()
      (unless (heap-empty? heap)
        (let* ([entry (heap-pop! heap)]
               [d (car entry)]
               [v (cdr entry)])
          (when (= d (vector-ref dist v))
            (graph-for-each-edge g v
              (lambda (e t)
                (let* ([w (graph-edge-weight g e)]
                       [w (if w w 1)]
                       [nd (+ d w)])
                  (when (or (not (vector-ref dist t))
                            (< nd (vector-ref dist t)))
                    (vector-set! dist t nd)
                    (vector-set! pred t v)
                    (heap-push! heap (cons nd t)))))))
          (loop))))
    (values dist pred))

  ;; Condensation: one vertex per SCC, an edge per original edge between
  ;; distinct SCCs (deduplicated).  Returns (values g2 node->scc) where
  ;; node->scc maps each original vertex to its condensed vertex.
  (define (condensation g)
    (define sccs (tarjan-scc g))
    (define n (graph-vertex-count g))
    (define node->scc (make-vector n))
    (define g2 (make-graph))
    (define n2 0)
    (define seen (make-eqv-hashtable))
    (for-each (lambda (scc) (graph-add-vertex! g2 scc)) sccs)
    (set! n2 (graph-vertex-count g2))
    (let loop ([i 0])
      (when (< i (length sccs))
        (for-each (lambda (v) (vector-set! node->scc v i)) (list-ref sccs i))
        (loop (+ i 1))))
    (let ([m (graph-edge-count g)])
      (do ([i 0 (+ i 1)]) ((= i m))
        (let ([a (vector-ref node->scc (graph-edge-source g i))]
              [b (vector-ref node->scc (graph-edge-target g i))])
          (unless (= a b)
            (let ([key (+ (* a n2) b)])
              (unless (hashtable-ref seen key #f)
                (hashtable-set! seen key #t)
                (graph-add-edge! g2 a b)))))))
    (values g2 node->scc))

  ;; Depth-first preorder from ROOT (recursive; only visits reachable
  ;; vertices).
  (define (graph-dfs-preorder g root)
    (define seen (make-vector (graph-vertex-count g) #f))
    (define out '())
    (define (visit v)
      (vector-set! seen v #t)
      (set! out (cons v out))
      (graph-for-each-successor g v
        (lambda (t)
          (unless (vector-ref seen t)
            (visit t)))))
    (visit root)
    (reverse out))

  ;; Breadth-first order from ROOT.
  (define (graph-bfs g root)
    (define seen (make-vector (graph-vertex-count g) #f))
    (vector-set! seen root #t)
    (let loop ([queue (list root)] [out '()])
      (if (null? queue)
        (reverse out)
        (let* ([v (car queue)]
               [rest (cdr queue)]
               [new (let ([acc '()])
                      (graph-for-each-successor g v
                        (lambda (t)
                          (unless (vector-ref seen t)
                            (vector-set! seen t #t)
                            (set! acc (cons t acc)))))
                      (reverse acc))])
          (loop (append rest new) (cons v out))))))

  (define (graph-copy g)
    (%make-graph (graph-directed? g)
      (flexvector-copy (graph-node-weights g))
      (flexvector-copy (graph-edge-from g))
      (flexvector-copy (graph-edge-to g))
      (flexvector-copy (graph-edge-weights g))
      (let ([adj (graph-adjacency g)]
            [n (graph-vertex-count g)])
        (let ([out (make-flexvector 0)])
          (let loop ([v 0])
            (when (< v n)
              (let ([a (flexvector-ref adj v)])
                (flexvector-add-back! out (and a (flexvector-copy a))))
              (loop (+ v 1))))
          out)))))
