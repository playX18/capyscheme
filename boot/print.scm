(define procedure-printer 
    (make-parameter 
        (lambda (proc port slashify)
           
            (print 
                (string-append "#<procedure" (let ([name (procedure-name proc)])
                    (if name (string-append " " (symbol->string name)) "")) " " (number->string (hash proc) 16) ">")
                port 
                #f))))

(define (print x p slashify)
    (define write-char io/write-char)    
    (define quoters '(quote quasiquote unquote unquote-splicing
                    syntax quasisyntax unsyntax unsyntax-splicing))

    (define quoter-strings '((quote . "'")
                            (quasiquote . "`")
                            (unquote . ",")
                            (unquote-splicing . ",@")
                            (syntax . "#'")
                            (quasisyntax . "#`")
                            (unsyntax . "#,")
                            (unsyntax-splicing . "#,@")))
    
    (define funny-characters (list #\" #\\))

    (define ctrl-B (integer->char 2))
    (define ctrl-C (integer->char 3))
    (define ctrl-F (integer->char 6))

    (define (print-in-string-without-hexifying? c)
        (let ((sv (char->integer c)))
            (or (<= 32 sv 126)
                (and (<= 128 sv)
                     (not (= sv #x00ab))    ; left double angle quote
                     (not (= sv #x00bb))    ; right double angle quote
                     (not (memq (char-general-category c)
                               '(Zs Zl Zp Cc Cf Cs Co Cn)))))))
    (define (print-as-char-without-hexifying? c)
        (let ((sv (char->integer c)))
            (or (<= 32 sv 126)
                (and (<= 128 sv)
                (not (memq (char-general-category c)
                          '(Mn Mc Me Zs Zl Zp Cc Cf Cs Co Cn)))))))
    (define (print x p slashify level)
        (cond 
            [(and (not slashify)
                  (zero? level))
                (printstr "..." p)]
            [(not (pair? x)) 
                (patom x p slashify level)]
            [(and (memq (car x) quoters)
                  (pair? (cdr x))
                  (null? (cadr x)))
                (print-quoted x p slashify level)]
            [(and (not slashify)
                  (eqv? 0 (print-length)))
                (printstr "(...)" p)]
            [else 
                (write-char (integer->char 40) p)
                (print (car x) p slashify (- level 1))
                (print-cdr (cdr x) p slashify
                           (- level 1)
                           (- (or (print-length) 0) 1))]))
    
    (define (print-cdr x p slashify level length)
        (cond 
            [(null? x)
                (write-char (integer->char 41) p)]
            [(and (not slashify)
                  (zero? length))
                (printstr " ...)" p)]
            [(pair? x)
                (write-char #\space p)
                (print (car x) p slashify level)
                (print-cdr (cdr x) p slashify level
                           (- length 1))]
            [else 
                (printstr " . " p)
                (print x p slashify level)
                (write-char (integer->char 41) p)]))
    
    (define (printsym s p) (printstr s p))
    (define (printstr s p)
        (define (loop x p i n)
            (if (< i n)
                (begin (write-char (string-ref x i) p)
                       (loop x p (+ i 1) n))))
        (loop s p 0 (string-length s)))

    (define (print-slashed-symbol x p)
        (let* ([s (symbol->string x)]
               [n (string-length s)])
            ;; TODO: R7RS
            (printstr s p)))

    (define (print-slashed-string s p)
        (define (loop i n)
            (if (< i n)
                (let* ([c (string-ref s i)]
                       [sv (char->integer c)])
                    (cond 
                        [(<= 32 sv 126)
                            (if (or (char=? c #\\)
                                    (char=? c #\"))
                                (write-char #\\ p))
                            (write-char c p)]
                        [(and (<= 128 sv)
                              (memq (transcoder-codec (port-transcoder p))
                                    '(utf-8 utf-16))
                              (print-in-string-without-hexifying? c))
                            (write-char c p)]
                        [else
                            (write-char #\\ p)
                            (case sv
                                [(7) (write-char #\a p)]
                                [(8) (write-char #\b p)]
                                [(9) (write-char #\t p)]
                                [(10) (write-char #\n p)]
                                ;[(11) (write-char #\v p)]
                                ;[(12) (write-char #\f p)]
                                [(13) (write-char #\r p)]
                                [else
                                    (let ([hexstring (number->string sv 16)])
                                        (write-char #\x p)
                                        (print-slashed-string hexstring p)
                                        (write-char #\; p))])])
                    (loop (+ 1 i) n))))
        (loop 0 (string-length s)))
    (define (print-slashed-bytevector s p)
        (define (loop x p i n)
            (if (< i n)
                (let ((c (integer->char (bytevector-ref x i))))
                    (if (memq c funny-characters)
                        (write-char #\\ p))
                    (write-char c p)
                    (loop x p (+ 1 i) n))))
        (loop s p 0 (bytevector-length s)))

    (define (patom x p slashify level)
        (cond 
            [(eq? x '()) (printstr "()" p)]
            [(not x) (printstr "#f" p)]
            [(eq? x #t) (printstr "#t" p)]
            [(symbol? x)
                (if slashify
                    (print-slashed-symbol x p)
                    (printsym (symbol->string x) p))]
            [(number? x) (printnumber x p slashify)]
            [(char? x)
                (if slashify
                    (printcharacter x p)
                    (write-char x p))]
            [(string? x)
                (if slashify
                    (begin 
                        (write-char #\" p)
                        (print-slashed-string x p)
                        (write-char #\" p))
                    (printstr x p))]
            [(eof-object? x) (printeof p)]
            [(bytevector? x) (print-bytevector x p slashify)]
            [(port? x) (print-port x p slashify)]
            [(procedure? x) (print-procedure x p slashify)]
            [(tuple? x) ((tuple-printer) x p slashify)]
            [(vector? x)
                (cond 
                    [(= 2 (vector-length x))
                        (write-char #\# p)
                        (write-char (integer->char 40) p)
                        (print (vector-ref x 0) p slashify level)
                        (write-char #\space p)
                        (print (vector-ref x 1) p slashify level)
                        (write-char (integer->char 41) p)]
                    [else
                        (write-char #\# p)
                        (print (vector->list x) p slashify (- level 1))])]
            [(syntax? x)
                (display "#<syntax " p)
                (display (syntax-expression x))
                (display " at " p)
                (display (syntax-sourcev x) p)]
            [(module? x)
                (display "#<module " p)
                (display (module-name x) p)
                (display ">" p)]
            [else (printstr "<TODO>" p)]))
    
    (define (printnumber n p slashify)
        (printstr (number->string n) p))
    (define (printcharacter c p)
        (write-char #\# p)
        (write-char #\\ p)
        (let ((k (char->integer c)))
            (cond 
                ((<= k **space**)
                    (cond 
                        ((= k **space**)  (printstr "space" p))
                        ((= k **newline**) (printstr "newline" p))
                        ((= k **linefeed**) (printstr "linefeed" p))
                        ((= k **return**) (printstr "return" p))
                        ((= k **tab**) (printstr "tab" p))
                        ((= k **alarm**) (printstr "alarm" p))
                        ((= k **backspace**) (printstr "backspace" p))
                        ((= k **vtab**) (printstr "vtab" p))
                        ((= k **page**) (printstr "page" p))
                        (else
                            (let ((r7rs?
                                (or (io/port-allows-r7rs-weirdness? p)
                                (not (io/port-allows-r6rs-weirdness? p)))))
                                (cond 
                                    ((= k **nul**)
                                        (printstr (if r7rs? "null" "nul") p))
                                    ((= k **esc**)
                                        (printstr (if r7rs? "escape" "esc") p))
                                    (else
                                        (printstr "x" p)
                                        (printstr (number->string k 16) p)))))))
                ((< k **delete**) (write-char c p))
                ((= k **delete**) (printstr "delete" p))
                ((and (memq (transcoder-codec (port-transcoder p))
                        '(utf-8 utf-16))
                    (print-as-char-without-hexifying? c))
                    (write-char c p))
                (else
                    (printstr "x" p)
                    (printstr (number->string k 16) p)))))
        (define (printeof p) (printstr "#<eof>" p))
        (define (print-quoted x p slashify level)
            (printstr (cdr (assq (car x) quoter-strings)) p)
            (print (cadr x) p slashify (- level 1)))
        (define (print-port x p slashify)
            (printstr (string-append "#<" (cond [(input-port? x) "input-port"]
                                                [(output-port? x) "output-port"]
                                                [else "port"]) " " (port-name x) ">") p))
        (define (print-bytevector x p slashify)
            (write-char #\# p)
            (cond 
                ((io/port-allows-r7rs-weirdness? p) #t)
                ((io/port-allows-r6rs-weirdness? p)
                    (write-char #\v p))
                (else #t))
            (write-char #\u p)
            (write-char #\8 p)
            (print (bytevector->list x) p slashify (- level 1)))
        (define (print-procedure x p slashify)
            ((procedure-printer) x p slashify))

    (print x p slashify (+ (or (print-level) -2) 1)))

(define print-length (make-parameter #f))
(define print-level (make-parameter #f))

(define (display-simple x . rest)
    "Simple implementation of display"
    (let [(p (if (null? rest) (current-output-port) (car rest)))]
        (print x p #f)
        (io/discretionary-flush p)))

(define (write-simple x . rest)
    "Simple implementation of write"
    (let [(p (if (null? rest) (current-output-port) (car rest)))]
        (print x p #t)
        (io/discretionary-flush p)))

(define (print-with-shared-structure obj outport write)
    (define (lookup key state)
        (core-hash-ref state key #f))
    (define (present? key state)
        (core-hash-contains? state key))
    (define (update-state key val state)
        (core-hash-put! state key val)
        state)
    
    (define (interesting? obj)
        (or (pair? obj)
            (and (vector? obj) (not (zero? (vector-length obj))))
            (and (string? obj) (not (zero? (string-length obj))))
            (bytevector? obj)
            (record? obj)
            (port? obj)
            (core-hashtable? obj)))
    
    (define (write-obj obj state outport)
        (define (write-interesting state)
            (cond 
                [(pair? obj)
                    (display "(" outport)
                    (let write-cdr ([obj (cdr obj)]
                                    [state (write-obj (car obj) state outport)])
                        (cond 
                            [(and (pair? obj)
                                  (not (lookup obj state)))
                                (display " " outport)
                                (write-cdr (cdr obj)
                                           (write-obj (car obj) state outport))]
                            [(null? obj)
                                (display ")" outport)
                                state]
                            [else 
                                (display " . " outport)
                                (let ([state (write-obj obj state outport)])
                                    (display ")" outport)
                                    state)]))]
                [(vector? obj)
                    (let ([len (vector-length obj)])
                    (display "#(" outport)
                    (let write-vec ([i 1]
                                    [state (write-obj (vector-ref obj 0) state outport)])
                        (cond 
                            [(= i len) 
                                (display ")" outport)
                                state]
                            [else 
                                (display " " outport)
                                (write-vec (+ i 1)
                                           (write-obj (vector-ref obj i) state outport))])))]
                [else (write obj outport) state]))
        
        (cond 
            [(interesting? obj)
                (let ([val (lookup obj state)])
                
                (cond 
                    [(not val) (write-interesting state)]
                    [(number? val)
                        (display "#" outport)
                        (write val outport)
                        (display "#" outport)]
                    [else 
                        (let* ([n (+ 1 (lookup 'counter state))]
                               [state (update-state 'counter n state)])
                            (display "#" outport)
                            (write n outport)
                            (display "=" outport)
                            (write-interesting (update-state obj n state)))]))]
            [else (write obj outport) state]))

    (define (scan obj state)
        (cond 
            [(not (interesting? obj)) state]
            [(present? obj state) 
                (update-state obj #t state)]
            [else 
                (let ([state (update-state obj #f state)])
                    (cond
                        [(pair? obj) (scan (car obj) (scan (cdr obj) state))]
                        [(vector? obj)
                            (let ([len (vector-length obj)])
                                (do ((i 0 (+ 1 i))
                                     (state state (scan (vector-ref obj i) state)))
                                    ((= i len) state)))]
                        [else state]))]))
    (let* ([state (make-core-hash-eq)]
           [state (scan obj state)]
           [state (update-state 'counter 0 state)])
        (write-obj obj state outport)
        (if #f #f)))

(define write-shared
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print-with-shared-structure x p write-simple))))

;;; An R7RS-conforming write procedure.

(define write
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (if (object-is-circular? x)
          (print-with-shared-structure x p write-simple)
          (write-simple x p)))))

;;; An R7RS-conforming display procedure.

(define display
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (if (object-is-circular? x)
          (print-with-shared-structure x p display-simple)
          (display-simple x p)))))


;;; Is the given object circular?
;;;
;;; First see if a depth-first traversal completes in bounded time.
;;; If not, perform a more expensive traversal that keeps track of
;;; all possibly circular objects in scope.
;;;
;;; See implementation of the equal? procedure for a more complicated
;;; example of this technique.

(define circularity:bound-on-recursion 100000)

(define (object-is-circular? x)

  ; Fast traversal with bounded recursion.
  ; Returns an exact integer n.
  ; If n > 0, then x is not circular and the traversal performed
  ; bound - n recursive calls.
  ; If n <= 0, then the bound was exceeded before the traversal
  ; could determine whether x is circular.

  (define (small? x bound)
    (cond ((<= bound 0)
           bound)
          ((pair? x)
           (let ((result (small? (car x) (- bound 1))))
             (if (> result 0)
                 (small? (cdr x) result)
                 result)))
          ((vector? x)
           (let ((nx (vector-length x)))
             (let loop ((i 0)
                        (bound (- bound 1)))
               (if (< i nx)
                   (let ((result (small? (vector-ref x i) bound)))
                     (if (> result 0)
                         (loop (+ i 1) result)
                         result))
                   bound))))
          (else bound)))

  ; Returns #t iff x contains circular structure or contains
  ; any of the objects present within the given hashtable.

  (define (circular? x table)
    (cond ((core-hash-contains? table x)
           #t)
          ((pair? x)
           (core-hash-put! table x #t)
           (cond ((circular? (car x) table)
                  #t)
                 ((circular? (cdr x) table)
                  #t)
                 (else
                  (core-hash-remove! table x)
                  #f)))
          ((vector? x)
           (core-hash-put! table x #t)
           (let ((nx (vector-length x)))
             (let loop ((i 0))
               (if (< i nx)
                   (if (circular? (vector-ref x i) table)
                       #t
                       (loop (+ i 1)))
                   (begin (core-hash-remove! table x)
                          #f)))))
          (else #f)))

  (cond ((< 0 (small? x circularity:bound-on-recursion))
         #f)
        (else
         (circular? x (make-core-hash-eq)))))

(tuple-printer 
    (lambda (obj port quote?)
        (cond 
            [(record? obj)
                (let ([p (rtd-printer (record-rtd obj))])
                    (if p 
                        (p obj port quote?)
                        (begin 
                            (display "#<record " port)
                            (display (record-type-name (record-rtd obj)) port)
                            (display ">" port))))]
            [(eq? (tuple-ref obj 0) 'type:record-type-descriptor)                
                (display "#<record-type-descriptor " port)
                (display (record-type-name obj) port)
                (display ">" port)]
            [(eq? (tuple-ref obj 0) 'type:record-type)
                (display "#<record-type " port)
                (display (record-type-name (record-type-rtd obj)) port)
                (display ">" port)]
            [(eq? (tuple-ref obj 0) 'type:record-constructor-descriptor)
                (display "#<record-constructor-descriptor " port)
                (display (record-type-name (rcd-rtd obj)) port)
                (display ">" port)]
            [else (display "#<unknown>" port)])))
(define (newline . port) (display "\n" (if (null? port) (current-output-port) (car port))))

(define (displayln x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
        (display x p)
        (newline p)))
(define (writeln x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
        (write x p)
        (newline p)))