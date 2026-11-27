;; Text rendering of the CFG
(library (core compiler cfg pretty)
  (export render-program render-value)
  (import (core) (capy) (core compiler cfg ir))

  (define (join strings sep)
    (if (null? strings)
      ""
      (apply string-append
        (car strings)
        (map (lambda (s) (string-append sep s)) (cdr strings)))))

  (define (num->str n) (number->string n))

  (define (render-value v)
    (cond
      [(eq? v #f) "#f"]
      [(eq? v #t) "#t"]
      [(symbol? v) (symbol->string v)]
      [(number? v) (number->string v)]
      [(string? v) v]
      [else (call-with-string-output-port (lambda (port) (display v port)))]))

  (define (render-program program)
    (apply string-append
      (map (lambda (p) (string-append (render-procedure p) "\n"))
        (program-procedures program))))

  (define (string-trim-both s)
    (define (ws? c) (char-whitespace? c))
    (let* ([n (string-length s)]
           [start (let loop ([i 0]) (if (and (< i n) (ws? (string-ref s i))) (loop (+ i 1)) i))]
           [end (let loop ([i n]) (if (and (> i start) (ws? (string-ref s (- i 1)))) (loop (- i 1)) i))])
      (substring s start end)))

  (define (render-procedure p)
    (let ([name (render-procedure-name p)]
          [params (render-uvars (cfg-proc-params p))]
          [blocks (list-sort (lambda (a b) (< (block-id a) (block-id b))) (cfg-proc-blocks p))])
      (string-append
        "fn " (render-code-id (cfg-proc-code p)) " " (string-trim-both name) "(" params ") {\n"
        (apply string-append (map (lambda (b) (string-append (render-block b) "\n")) blocks))
        "}\n")))

  (define (render-block b)
    (string-append
      "block" (num->str (block-id b)) ":\n"
      (apply string-append
        (map (lambda (i) (string-append "    " (render-instruction i) "\n"))
          (block-instructions b)))
      "    " (render-terminator (block-terminator b)) "\n"))

  (define (render-instruction i)
    (cond
      [(insn-assign? i)
       (string-append (render-uvar (insn-assign-dst i)) " = " (render-operand (insn-assign-src i)))]
      [(insn-const? i)
       (string-append (render-uvar (insn-const-dst i)) " = const "
         (render-value (insn-const-value i)))]
      [(insn-make-closure? i)
       (string-append (render-uvar (insn-make-closure-dst i)) " = make_closure "
         (render-code-id (insn-make-closure-code i)) " "
         (num->str (insn-make-closure-free-count i))
         (let ([env (insn-make-closure-env i)])
           (if env (string-append " env " (render-operand env)) "")))]
      [(insn-make-case-closure? i)
       (string-append (render-uvar (insn-make-case-closure-dst i)) " = make_case_closure "
         (render-code-id (insn-make-case-closure-code i)) " ["
         (join (map render-operand (insn-make-case-closure-clauses i)) ", ") "]")]
      [(insn-make-env? i)
       (string-append (render-uvar (insn-make-env-dst i)) " = make_env "
         (num->str (insn-make-env-size i)))]
      [(insn-env-ref? i)
       (string-append (render-uvar (insn-env-ref-dst i)) " = env_ref "
         (render-operand (insn-env-ref-env i)) "[" (num->str (insn-env-ref-index i)) "]")]
      [(insn-env-set? i)
       (string-append "env_set " (render-operand (insn-env-set-env i)) "["
         (num->str (insn-env-set-index i)) "], " (render-operand (insn-env-set-value i)))]
      [(insn-closure-ref? i)
       (string-append (render-uvar (insn-closure-ref-dst i)) " = closure_ref "
         (render-operand (insn-closure-ref-closure i)) "["
         (num->str (insn-closure-ref-index i)) "]")]
      [(insn-closure-set? i)
       (string-append "closure_set " (render-operand (insn-closure-set-closure i)) "["
         (num->str (insn-closure-set-index i)) "], "
         (render-operand (insn-closure-set-value i)))]
      [(insn-cache-ref? i)
       (string-append (render-uvar (insn-cache-ref-dst i)) " = cache_ref "
         (render-operand (insn-cache-ref-cache-key i)))]
      [(insn-cache-set? i)
       (string-append (render-uvar (insn-cache-set-dst i)) " = cache_set "
         (render-operand (insn-cache-set-cache-key i)) ", "
         (render-operand (insn-cache-set-value i)))]
      [(insn-prim-call? i)
       (string-append (render-uvar (insn-prim-call-dst i)) " = "
         (symbol->string (insn-prim-call-prim i))
         "(" (render-call-args-list (insn-prim-call-args i)) ")")]
      [(insn-rest-to-list? i)
       (string-append (render-uvar (insn-rest-to-list-dst i)) " = rest_to_list "
         (render-uvar (insn-rest-to-list-rest i)))]
      [(insn-rest-ref? i)
       (string-append (render-uvar (insn-rest-ref-dst i)) " = rest_ref "
         (render-uvar (insn-rest-ref-rest i)) ", " (num->str (insn-rest-ref-index i)))]
      [(insn-rest-length? i)
       (string-append (render-uvar (insn-rest-length-dst i)) " = rest_length "
         (render-uvar (insn-rest-length-rest i)) ", " (num->str (insn-rest-length-skip i)))]
      [(insn-rest-predicate? i)
       (string-append (render-uvar (insn-rest-predicate-dst i)) " = rest_"
         (render-rest-predicate (insn-rest-predicate-predicate i)) " "
         (render-uvar (insn-rest-predicate-rest i)) ", "
         (num->str (insn-rest-predicate-skip i)))]
      [(insn-receive? i)
       (string-append (render-uvar (insn-receive-dst i)) " = receive "
         (num->str (insn-receive-index i)))]
      [(insn-receive-rest? i)
       (string-append (render-uvar (insn-receive-rest-dst i)) " = receive_rest "
         (num->str (insn-receive-rest-from i)))]
      [else (error 'render-instruction "unknown instruction" i)]))

  (define (hint->string h) (symbol->string h))

  (define (render-terminator t)
    (cond
      [(term-call? t)
       (let ([ret (term-call-ret t)])
         (string-append "call " (render-operand (term-call-callee t)) "("
           (render-call-args-list (term-call-args t)) ") -> block"
           (num->str (return-point-block ret)) " (" (num->str (return-point-nvals ret))
           (if (return-point-variadic? ret) "+" "") " values)"))]
      [(term-return? t)
       (string-append "return (" (render-call-args-list (term-return-args t)) ")")]
      [(term-tail-call? t)
       (string-append "tail_call " (render-operand (term-tail-call-callee t)) "("
         (render-call-args-list (term-tail-call-args t)) ")")]
      [(term-raise? t)
       (string-append "raise " (symbol->string (term-raise-kind t)) "("
         (render-call-args-list (term-raise-args t)) ")")]
      [(term-jump? t)
       (string-append "jump block" (num->str (term-jump-target t)))]
      [(term-branch? t)
       (string-append "brif " (render-operand (term-branch-test t)) ", "
         (render-branch-target (term-branch-consequent t)) ", "
         (render-branch-target (term-branch-alternative t)) " ; "
         (hint->string (car (term-branch-hints t))) ", "
         (hint->string (cadr (term-branch-hints t))))]
      [(term-branch-prim? t)
       (string-append "brif_prim " (symbol->string (term-branch-prim-prim t)) "("
         (join (map render-operand (term-branch-prim-args t)) ", ") "), "
         (render-branch-target (term-branch-prim-consequent t)) ", "
         (render-branch-target (term-branch-prim-alternative t)) " ; "
         (hint->string (car (term-branch-prim-hints t))) ", "
         (hint->string (cadr (term-branch-prim-hints t))))]
      [else
       (string-append "switch " (render-switch-kind (term-switch-kind t)) " "
         (render-operand (term-switch-scrutinee t)) ", ["
         (join
           (map (lambda (c)
                  (string-append (render-switch-case-value (switch-case-value c)) " => "
                    (render-branch-target (switch-case-target c))))
             (term-switch-cases t))
           ", ")
         "], default " (render-branch-target (term-switch-default t)))]))

  (define (render-branch-target t)
    (if (target-local? t)
      (if (null? (target-local-edge-assigns t))
        (string-append "block" (num->str (target-local-block t)))
        (string-append "block" (num->str (target-local-block t)) "[+"
          (num->str (length (target-local-edge-assigns t))) "]"))
      (string-append "return (" (render-call-args-list (target-return-args t)) ")")))

  (define (render-rest-predicate p)
    (case p
      [(Null) "null"]
      [(Pair) "pair"]
      [else "list"]))

  (define (render-switch-kind kind)
    (cond
      [(pair? kind) "eq?"]   ; (SymbolEq . mask)
      [else
       (case kind
         [(Eq) "eq?"]
         [(Fixnum) "fx=?"]
         [(Numeric) "="]
         [(Char) "char=?"]
         [else "eq?"])]))   ; CharEq

  (define (render-switch-case-value v)
    (if (case-integer? v)
      (num->str (case-integer-value v))
      (render-value (case-symbol-value v))))

  (define (render-call-args-list args) (join (map render-operand args) ", "))

  (define (render-operand o)
    (if (operand-constant? o)
      (render-value (operand-constant-value o))
      (render-uvar (operand-local-uvar o))))

  (define (render-uvar id) (string-append "u" (num->str id)))
  (define (render-uvars vars) (join (map render-uvar vars) ", "))

  (define (render-code-id code)
    (string-append (if (graph-code-id? code) "gf" "cg") (num->str (code-id-number code))))

  (define (render-procedure-name p)
    (let ([entry (assv (cfg-proc-binding p) (cfg-proc-sources p))])
      (if entry
        (render-value (cdr entry))
        (render-value (cfg-proc-name p)))))
)
