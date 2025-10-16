
(define <reader> 
    (let* ([rtd (make-record-type-descriptor '<reader> #f #f #f #f '#(
        (immutable port) 
        (immutable file)
        (mutable line) (mutable column)
        (mutable saved-line) (mutable saved-column)
        (mutable fold-case?)
        (mutable mode)
        (mutable tolerant?)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type '<reader> rtd rcd)))


(define reader? (record-predicate (record-type-rtd <reader>)))
(define %make-reader (record-constructor (record-type-rcd <reader>)))
(define reader-port (record-accessor (record-type-rtd <reader>) 0))
(define reader-file (record-accessor (record-type-rtd <reader>) 1))
(define reader-line (record-accessor (record-type-rtd <reader>) 2))
(define reader-column (record-accessor (record-type-rtd <reader>) 3))
(define reader-saved-line (record-accessor (record-type-rtd <reader>) 4))
(define reader-saved-column (record-accessor (record-type-rtd <reader>) 5))
(define reader-fold-case? (record-accessor (record-type-rtd <reader>) 6))
(define reader-mode (record-accessor (record-type-rtd <reader>) 7))
(define reader-tolerant? (record-accessor (record-type-rtd <reader>) 8))

(define reader-line-set! (record-mutator (record-type-rtd <reader>) 2))
(define reader-column-set! (record-mutator (record-type-rtd <reader>) 3))
(define reader-saved-line-set! (record-mutator (record-type-rtd <reader>) 4))
(define reader-saved-column-set! (record-mutator (record-type-rtd <reader>) 5))
(define reader-fold-case-set! (record-mutator (record-type-rtd <reader>) 6))
(define reader-mode-set! (record-mutator (record-type-rtd <reader>) 7))
(define reader-tolerant-set! (record-mutator (record-type-rtd <reader>) 8))

(define <annotation> 
    (let* ([rtd (make-record-type-descriptor '<annotation> #f #f #f #f '#(
        (immutable expression)
        (immutable source)
        (immutable stripped)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type '<annotation> rtd rcd)))

(define annotation? (record-predicate (record-type-rtd <annotation>)))
(define %make-annotation (record-constructor (record-type-rcd <annotation>)))
(define annotation-expression (record-accessor (record-type-rtd <annotation>) 0))
(define annotation-source (record-accessor (record-type-rtd <annotation>) 1))
(define annotation-stripped (record-accessor (record-type-rtd <annotation>) 2))

(define (annotation-source->condition x)
    (if (vector? x)
        (apply make-source-condition (vector->list x))
        (condition)))

(define (reader-source r)
    (vector (reader-file r) (reader-saved-line r) (reader-saved-column r)))
(define (annotate source stripped datum)
    (datum->syntax #f datum source))

(define (make-reader port file)
    (%make-reader port file 1 0 1 0 #f 'rnrs #f))

(define (lexical-condition reader msg irritants)
    (condition 
        (make-lexical-violation)
        (make-message-condition msg)
        (make-source-condition (reader-file reader) (reader-saved-line reader) (reader-saved-column reader))
        (make-irritants-condition irritants)))

(define (reader-error reader msg . irritants)
    (raise 
        (lexical-condition reader msg irritants)))

(define (reader-warning reader msg . irritants)
    (if (reader-tolerant? reader)
        (raise-continuable 
            (condition 
                (make-warning)
                (lexical-condition reader msg irritants)))
        (reader-error reader msg irritants)))
(define (assert-mode p msg modes)
  (unless (memq (reader-mode p) modes)
    (reader-warning p (string-append msg " is not allowed in this mode")
                    (reader-mode p))))
(define (eof-warning reader)
    (reader-warning reader "unexpected end of file"))

(define read-datum #f)
(define read-annotated #f)
(define get-token #f)

(let ([rnrs:lookahead-char lookahead-char]
      [rnrs:get-char get-char]
      [rnrs:call-with-string-output-port call-with-string-output-port]
      [rnrs:put-char put-char])

    (define (reader-mark reader)
        (reader-saved-line-set! reader (reader-line reader))
        (reader-saved-column-set! reader (reader-column reader)))

    (define (lookahead-char reader) (rnrs:lookahead-char (reader-port reader)))
    (define (get-char reader) 
        (let ([c (rnrs:get-char (reader-port reader))])
            (if (eqv? c #\newline)
                (begin 
                    (reader-line-set! reader (+ 1 (reader-line reader)))
                    (reader-column-set! reader -1)))
            (reader-column-set! reader (+ 1 (reader-column reader)))
            c))
    
    (define (unicode-scalar-value? sv)
        (and (fixnum? sv) 
             (<= 0 sv #x10FFFF)
             (not (<= #xD800 sv #xDFFF))))
    
    (define (char-delimiter? reader c)
        ;; Treats the eof-object as a delimiter
        (or (eof-object? c)
            (char-whitespace? c)
            (case (reader-mode reader)
                ((r6rs)
                (memv c '(#\( #\) #\[ #\] #\" #\; #\#)))
                ((r7rs)
                (memv c '(#\( #\) #\" #\; #\|)))
                (else
                (memv c '(#\( #\) #\[ #\] #\" #\; #\# #\|))))))
    (define (get-line reader)
        (rnrs:call-with-string-output-port
            (lambda (out)
                (do ((c (get-char reader) (get-char reader)))
                    ((or (eqv? c #\linefeed) (eof-object? c)))
                    (rnrs:put-char out c)))))
    (define (get-whitespace reader char)
        (rnrs:call-with-string-output-port
            (lambda (out)
                (let lp ((char char))
                    (rnrs:put-char out char)
                    (let ((char (lookahead-char reader)))
                        (if (and (char? char) (char-whitespace? char))
                        (lp (get-char reader))))))))

    ;; Get an inline hex escape (escaped character inside an identifier).
    (define (get-inline-hex-escape p)
        (reader-mark p)
        (let lp ((digits '()))
            (let ((c (get-char p)))
                (cond 
                    ((eof-object? c)
                        (eof-warning p)
                        #\xFFFD)
                    ((or (char<=? #\0 c #\9)
                         (char<=? #\a c #\f)
                         (char<=? #\A c #\F))
                        (lp (cons c digits)))
                    ((and (char=? c #\;) (pair? digits))
                        (let ((sv (string->number (list->string (reverse digits)) 16)))
                            (cond 
                                ((unicode-scalar-value? sv)
                                    (integer->char sv))
                                (else
                                    (reader-warning p "Inline hex escape outside valid range" sv)
                                    #\xFFFD))))
                    (else
                        (reader-warning p "Invalid inline hex escape" c)
                        #\xFFFD)))))
(define (get-identifier p initial-char pipe-quoted?)
  (let lp ((chars (if initial-char (list initial-char) '())))
    (let ((c (lookahead-char p)))
      (cond
        ((and (char? c)
              (or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                            #\+ #\- #\. #\@))
                  (and (> (char->integer c) 127)
                       (memq (char-general-category c) ;XXX: could be done faster
                             '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co Nd Mc Me)))
                  (and (memv (reader-mode p) '(rnrs r7rs))
                       (memv c '(#\x200C #\x200D)))))

         (lp (cons (get-char p) chars)))
        ((and pipe-quoted? (char? c) (not (memv c '(#\| #\\))))
         (lp (cons (get-char p) chars)))
        ((or (char-delimiter? p c) (and pipe-quoted? (eqv? c #\|)))
         (if (eqv? c #\|)
           (get-char p))

         (let ((id (list->string (reverse chars))))
           (if (reader-fold-case? p)
               (values 'identifier (string->symbol (string-foldcase id)))
               (values 'identifier (string->symbol id)))))
        ((char=? c #\\)           ;\xUUUU;
         (get-char p)             ;consume #\\
         (let ((c (get-char p)))  ;should be #\x
           (cond ((eqv? c #\x)
                  (lp (cons (get-inline-hex-escape p) chars)))
                 
                 (else
                  (if (eof-object? c)
                      (eof-warning p)
                      (reader-warning p "Invalid character following \\"))
                  (lp chars)))))
        (else
         (reader-warning p "Invalid character in identifier" c)
         (get-char p)
         (lp chars))))))
;; Get a number from the reader.
(define (get-number p initial-chars)
  (let lp ((chars initial-chars))
    (let ((c (lookahead-char p)))
      (cond ((and (not (eqv? c #\#)) (char-delimiter? p c))
             ;; TODO: some standard numbers are not supported
             ;; everywhere, should use a number lexer.
             (let ((str (list->string (reverse chars))))
               (cond (#t
                      (values 'value (string->number str)))
                     ((and (memq (reader-mode p) '(rnrs r7rs))
                           ;; TODO: This is incomplete.
                           (not (and (pair? initial-chars)
                                     (char<=? #\0 (car initial-chars) #\9))))
                      (values 'identifier (string->symbol str)))
                     (else
                      (reader-warning p "Invalid number syntax" str)
                      (values 'identifier (string->symbol str))))))
            (else
             (lp (cons (get-char p) chars)))))))
;; Get a string datum from the reader.
(define (get-string p)
  (let lp ((chars '()))
    (let ((c (lookahead-char p)))
      (cond ((eof-object? c)
             (eof-warning p)
             c)
            ((char=? c #\")
             (get-char p)
             (list->string (reverse chars)))
            ((char=? c #\\)           ;escapes
             (get-char p)             ;consume #\\
             (let ((c (lookahead-char p)))
               (cond ((eof-object? c)
                      (eof-warning p)
                      c)
                     ((or (memv c '(#\tab #\linefeed #\x85 #\x2028))
                          (eq? (char-general-category c) 'Zs))
                      ;; \<intraline whitespace>*<line ending>
                      ;; <intraline whitespace>*
                      (letrec ((skip-intraline-whitespace*
                                (lambda ()
                                  (let ((c (lookahead-char p)))
                                    (cond ((eof-object? c)
                                           (eof-warning p)
                                           c)
                                          ((or (char=? c '#\tab)
                                               (eq? (char-general-category c) 'Zs))
                                           (get-char p)
                                           (skip-intraline-whitespace*))))))
                               (skip-newline
                                (lambda ()
                                  (let ((c (get-char p)))
                                    ;; XXX: it appears that the port
                                    ;; transcoder is meant to
                                    ;; replace all these linefeeds
                                    ;; with #\linefeed.
                                    (cond ((eof-object? c) c)
                                          ((memv c '(#\linefeed #\x85 #\x2028)))
                                          ((char=? c #\return)
                                           (if (memv (lookahead-char p)
                                                       '(#\linefeed #\x85))
                                             (get-char p)))
                                          (else
                                           (reader-warning p "Expected a line ending" c)))))))
                        (skip-intraline-whitespace*)
                        (skip-newline)
                        (skip-intraline-whitespace*)
                        (lp chars)))
                     (else
                      (lp (cons
                           (case (get-char p)
                             ((#\") #\")
                             ((#\\) #\\)
                             ((#\a) #\alarm)
                             ((#\b) #\backspace)
                             ((#\t) #\tab)
                             ((#\n) #\linefeed)
                             ((#\v) (assert-mode p "\\v" '(rnrs r6rs)) #\vtab)
                             ((#\f) (assert-mode p "\\f" '(rnrs r6rs)) #\page)
                             ((#\r) #\return)
                             ((#\|) (assert-mode p "\\|" '(rnrs r7rs)) #\|)
                             ((#\x) (get-inline-hex-escape p))
                             (else
                              (reader-warning p "Invalid escape in string" c)
                              #\xFFFD))
                           chars))))))
            (else
             (lp (cons (get-char p) chars)))))))

;; Gets a nested comment from the reader.
(define (get-nested-comment reader)
  ;; The reader is immediately after "#|".
  (rnrs:call-with-string-output-port
   (lambda (out)
     (let lp ((levels 1) (c0 (get-char reader)))
       (let ((c1 (get-char reader)))
         (cond ((eof-object? c0)
                (eof-warning reader))
               ((and (eqv? c0 #\|) (eqv? c1 #\#))
                (if (not (eqv? levels 1)) (begin
                  (rnrs:put-char out c0)
                  (rnrs:put-char out c1)
                  (lp (- levels 1) (get-char reader)))))
               ((and (eqv? c0 #\#) (eqv? c1 #\|))
                (rnrs:put-char out c0)
                (rnrs:put-char out c1)
                (lp (+ levels 1) (get-char reader)))
               (else
                (rnrs:put-char out c0)
                (lp levels c1))))))))

;; Gets a #! !# comment from the reader.
(define (get-!-comment reader)
  ;; The reader is immediately after "#!".
  (rnrs:call-with-string-output-port
   (lambda (out)
     (let lp ((c0 (get-char reader)))
       (let ((c1 (get-char reader)))
         (cond ((eof-object? c0)
                (eof-warning reader))
               ((and (eqv? c0 #\!) (eqv? c1 #\#))
                #f)
               (else
                (rnrs:put-char out c0)
                (lp c1))))))))

;; Get a comment from the reader (including the terminating whitespace).
(define (get-comment reader)
  ;; The reader is immediately after #\;.
  (rnrs:call-with-string-output-port
   (lambda (out)
     (let lp ()
       (let ((c (get-char reader)))
         (if (not (eof-object? c)) (begin
           (rnrs:put-char out c)
           (cond ((memv c '(#\linefeed #\x85 #\x2028 #\x2029)))
                 ((char=? c #\return)
                  ;; Weird line ending. This lookahead is what forces
                  ;; the procedure to include the terminator.
                  (if (memv (lookahead-char reader) '(#\linefeed #\x85))
                    (rnrs:put-char out (get-char reader))))
                 (else
                  (lp))))))))))

;; Whitespace and comments can appear anywhere.
(define (atmosphere? type)
  (memq type '(directive whitespace comment inline-comment nested-comment)))

(define (get-lexeme p)
    (call-with-values 
        (lambda () (get-token p))
        (lambda (type lexeme)
            (if (atmosphere? type)
                (get-lexeme p)
                (values type lexeme)))))



(define (reader:get-token p)
  (assert (reader? p))
  (reader-mark p)
  (let ((c (get-char p)))
    (cond
      ((eof-object? c)
       (values 'eof c))
      ((char-whitespace? c)
       (values 'whitespace (get-whitespace p c)))
      ((char=? c #\;)                 ;a comment like this one
       (values 'comment (get-comment p)))
      ((char=? c #\#)                 ;the mighty octothorpe
       (let ((c (get-char p)))
         (case c
           ((#\() (values 'vector #f))
           ((#\') (values 'abbrev 'syntax))
           ((#\`) (values 'abbrev 'quasisyntax))
           ((#\,)
            (case (lookahead-char p)
              ((#\@)
               (get-char p)
               (values 'abbrev 'unsyntax-splicing))
              (else (values 'abbrev 'unsyntax))))
           ((#\v)                       ;r6rs
            (let* ((c1 (and (eqv? (lookahead-char p) #\u) (get-char p)))
                   (c2 (and (eqv? c1 #\u) (eqv? (lookahead-char p) #\8) (get-char p)))
                   (c3 (and (eqv? c2 #\8) (eqv? (lookahead-char p) #\() (get-char p))))
              (cond ((and (eqv? c1 #\u) (eqv? c2 #\8) (eqv? c3 #\())
                     (assert-mode p "#vu8(" '(rnrs r6rs))
                     (values 'bytevector #f))
                    (else
                     (reader-warning p "Expected #vu8(")
                     (get-token p)))))
           ((#\u #\U)                   ;r7rs
            (let* ((c1 (and (eqv? (lookahead-char p) #\8) (get-char p)))
                   (c2 (and (eqv? c1 #\8) (eqv? (lookahead-char p) #\() (get-char p))))
              (cond ((and (eqv? c1 #\8) (eqv? c2 #\())
                     (assert-mode p "#u8(" '(rnrs r7rs))
                     (values 'bytevector #f))
                    (else
                     (reader-warning p "Expected #u8(")
                     (get-token p)))))
           ((#\;)                     ;s-expr/datum comment
            (let lp ((atmosphere '()))
              (let-values (((type token) (get-token p)))
                (cond ((eq? type 'eof)
                       (eof-warning p)
                       (values 'inline-comment (cons (reverse atmosphere) p)))
                      ((atmosphere? type)
                       (lp (cons (cons type token) atmosphere)))
                      (else
                       (let-values ([(d _) (handle-lexeme p type token #f #t)])
                         (values 'inline-comment (cons (reverse atmosphere) d))))))))
           ((#\|)                     ;nested comment
            (values 'nested-comment (get-nested-comment p)))
           ((#\!)                     ;#!r6rs etc
            (let ((next-char (lookahead-char p)))
              (cond ((and (= (reader-saved-line p) 1) (memv next-char '(#\/ #\space)))
                     (let ((line (reader-saved-line p))
                           (column (reader-saved-column p)))
                       (values 'shebang `(,line ,column ,(get-line p)))))
                    ((and (char? next-char) (char-alphabetic? next-char))
                     (receive (type id) (get-token p)
                       (cond
                         ((eq? type 'identifier)
                          (case id
                            ((r6rs)          ;r6rs.pdf
                             (assert-mode p "#!r6rs" '(rnrs r6rs))
                             (reader-mode-set! p 'r6rs))
                            ((fold-case)     ;r6rs-app.pdf
                             (assert-mode p "#!fold-case" '(rnrs r6rs r7rs))
                             (reader-fold-case?-set! p #t))
                            ((no-fold-case)  ;r6rs-app.pdf
                             (assert-mode p "#!no-fold-case" '(rnrs r6rs r7rs))
                             (reader-fold-case?-set! p #f))
                            ((r7rs)          ;oddly missing in r7rs
                             (assert-mode p "#!r7rs" '(rnrs))
                             (reader-mode-set! p 'r7rs))
                            ((false)         ;r2rs
                             (assert-mode p "#!false" '(rnrs r2rs)))
                            ((true)          ;r2rs
                             (assert-mode p "#!true" '(rnrs r2rs)))
                            (else
                             (reader-warning p "Invalid directive" type id)))
                          (cond ((assq id '((false . #f) (true . #t)))
                                 => (lambda (x) (values 'value (cdr x))))
                                (else
                                 (values 'directive id))))
                         (else
                          (reader-warning p "Expected an identifier after #!")
                          (get-token p)))))
                    ((eq? (reader-mode p) 'rnrs)
                     ;; Guile compat.
                     (get-token p)
                     (values 'comment (get-!-comment p)))
                    (else
                     (reader-warning p "Expected an identifier after #!")
                     (get-token p)))))
           ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
            (get-number p (list c #\#)))
           ((#\t #\T)
            (unless (char-delimiter? p (lookahead-char p))
              (if (memq (reader-mode p) '(rnrs r7rs))
                  (let* ((c1 (and (memv (lookahead-char p) '(#\r #\R)) (get-char p)))
                         (c2 (and c1 (memv (lookahead-char p) '(#\u #\U)) (get-char p)))
                         (c3 (and c2 (memv (lookahead-char p) '(#\e #\E)) (get-char p))))
                    (unless (and c1 c2 c3 (char-delimiter? p (lookahead-char p)))
                      (reader-warning p "Expected #true")))
                  (reader-warning p "A delimiter is expected after #t")))
            (values 'value #t))
           ((#\f #\F)
            (unless (char-delimiter? p (lookahead-char p))
              (if (memq (reader-mode p) '(rnrs r7rs))
                  (let* ((c1 (and (memv (lookahead-char p) '(#\a #\A)) (get-char p)))
                         (c2 (and c1 (memv (lookahead-char p) '(#\l #\L)) (get-char p)))
                         (c3 (and c2 (memv (lookahead-char p) '(#\s #\S)) (get-char p)))
                         (c4 (and c3 (memv (lookahead-char p) '(#\e #\E)) (get-char p))))
                    (unless (and c1 c2 c3 c4 (char-delimiter? p (lookahead-char p)))
                      (reader-warning p "Expected #false" c1 c2 c3 c4)))
                  (reader-warning p "A delimiter is expected after #f")))
            (values 'value #f))
           ((#\\)
            (let lp ((char* '()))
              (let ((c (lookahead-char p)))
                (cond ((and (pair? char*) (char-delimiter? p c))
                       (let ((char* (reverse char*)))
                         (cond ((null? char*)
                                (reader-warning p "Empty character name")
                                (values 'value #\xFFFD))
                               ((null? (cdr char*)) (values 'value (car char*)))
                               ((char=? (car char*) #\x)
                                (cond ((for-all (lambda (c)
                                                  (or (char<=? #\0 c #\9)
                                                      (char<=? #\a c #\f)
                                                      (char<=? #\A c #\F)))
                                                (cdr char*))
                                       (let ((sv (string->number (list->string (cdr char*)) 16)))
                                         (cond ((unicode-scalar-value? sv)
                                                (values 'value (integer->char sv)))
                                               (else
                                                (reader-warning p "Hex-escaped character outside valid range" sv)
                                                (values 'value #\xFFFD)))))
                                      (else
                                       (reader-warning p "Invalid character in hex-escaped character"
                                                       (list->string (cdr char*)))
                                       (values 'value #\xFFFD))))
                               (else
                                (let ((char-name (list->string char*))
                                      (char-names '(("nul" #\nul r6rs)
                                                    ("null" #\nul r7rs)
                                                    ("alarm" #\alarm r6rs r7rs)
                                                    ("backspace" #\backspace r6rs r7rs)
                                                    ("tab" #\tab r6rs r7rs)
                                                    ("linefeed" #\linefeed r6rs)
                                                    ("newline" #\linefeed r5rs r6rs r7rs)
                                                    ("vtab" #\vtab r6rs)
                                                    ("page" #\page r6rs)
                                                    ("return" #\return r6rs r7rs)
                                                    ("esc" #\esc r6rs)
                                                    ("escape" #\esc r7rs)
                                                    ("space" #\space r5rs r6rs r7rs)
                                                    ("delete" #\delete r6rs r7rs))))
                                  (cond
                                    ((or (assoc char-name char-names)
                                         (and (reader-fold-case? p)
                                              (assoc (string-foldcase char-name)
                                                     char-names)))
                                     => (lambda (char-data)
                                          (assert-mode p char-name (cons 'rnrs (cddr char-data)))
                                          (values 'value (cadr char-data))))
                                    (else
                                     (reader-warning p "Invalid character name" char-name)
                                     (values 'value #\xFFFD))))))))
                      ((and (null? char*) (eof-object? c))
                       (eof-warning p)
                       (values 'value #\xFFFD))
                      (else
                       (lp (cons (get-char p) char*)))))))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (assert-mode p "#<n>=<datum> and #<n>#" '(rnrs r7rs))
            (let lp ((char* (list c)))
              (let ((next (lookahead-char p)))
                (cond
                  ((eof-object? next)
                   (eof-warning p)
                   (get-char p))
                  ((char<=? #\0 next #\9)
                   (lp (cons (get-char p) char*)))
                  ((char=? next #\=)
                   (get-char p)
                   (values 'label (string->number (list->string (reverse char*)) 10)))
                  ((char=? next #\#)
                   (get-char p)
                   (values 'reference (string->number (list->string (reverse char*)) 10)))
                  (else
                   (reader-warning p "Expected #<n>=<datum> or #<n>#" next)
                   (get-token p))))))
           (else
            (reader-warning p "Invalid #-syntax" c)
            (get-token p)))))
      ((char=? c #\")
       (values 'value (get-string p)))
      ((memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (get-number p (list c)))
      ((memv c '(#\- #\+))            ;peculiar identifier
       (cond ((and (char=? c #\-) (eqv? #\> (lookahead-char p))) ;->
              (get-identifier p c #f))
             ((char-delimiter? p (lookahead-char p))
              (values 'identifier (if (eqv? c #\-) '- '+)))
             (else
              (get-number p (list c)))))
      ((char=? c #\.)                 ;peculiar identifier
       (cond ((char-delimiter? p (lookahead-char p))
              (values 'dot #f))
             ((and (eq? (reader-mode p) 'r6rs)
                   (eqv? #\. (lookahead-char p)))
              (get-char p)            ;consume second dot
              (unless (eqv? #\. (get-char p)) ;consume third dot
                (reader-warning p "Expected the ... identifier"))
              (unless (char-delimiter? p (lookahead-char p))
                (reader-warning p "Expected the ... identifier"))
              (values 'identifier '...))
             (else
              (get-number p (list c)))))
      ((or (char<=? #\a c #\z) (char<=? #\A c #\Z) ;<constituent> and <special initial>
           (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
           (and (memv (reader-mode p) '(rnrs r7rs))
                (or (eqv? c #\@) (memv c '(#\x200C #\x200D))))
           (and (> (char->integer c) 127)
                (memq (char-general-category c)
                      '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))
       (get-identifier p c #f))
      ((char=? c #\@)
        (get-identifier p c #f))
      ((char=? c #\\)                 ;<inline hex escape>
       (let ((c (get-char p)))
         (cond ((eqv? c #\x)
                (get-identifier p (get-inline-hex-escape p) #f))
               (else
                (cond ((eof-object? c)
                       (eof-warning p))
                      (else
                       (reader-warning p "Invalid character following \\")))
                (get-token p)))))
      (else
       (case c
         ((#\() (values 'openp #f))
         ((#\)) (values 'closep #f))
         ((#\[) (values 'openb #f))
         ((#\]) (values 'closeb #f))
         ((#\') (values 'abbrev 'quote))
         ((#\`) (values 'abbrev 'quasiquote))
         ((#\,)
          (case (lookahead-char p)
            ((#\@)
             (get-char p)
             (values 'abbrev 'unquote-splicing))
            (else (values 'abbrev 'unquote))))
         ((#\|)
          (assert-mode p "Quoted identifiers" '(rnrs r7rs))
          (get-identifier p #f 'pipe))
         (else
          (reader-warning p "Invalid leading character" c)
          (get-token p)))))))

(define (get-compound-datum p src terminator type labels)
    (define vec #f)
    (define vec^ #f)
    (let lp ((head '()) (head^ '()) (prev #f) (prev^ #f) (len 0))
        (call-with-values 
            (lambda () (get-lexeme p))
            (lambda (lextype x)
                (case lextype 
                    [(closep closeb eof)
                        (unless (eq? lextype terminator)
                            (if (eof-object? x)
                                (eof-warning p)
                                (reader-error p "Mismatched parenthesis/brackes" lextype x terminator)))
                        (case type 
                            [(vector)
                                (let ([s (list->vector head)]
                                      [s^ (list->vector head^)])
                                    (set! vec s)
                                    (set! vec^ (annotate src s s^))
                                    (values vec vec^))]
                            [(list) 
                                (values head (annotate src head head^))]
                            [(bytevector)
                                (let ([s (u8-list->bytevector head)])
                                    (values s (annotate src s s)))]
                            [else (reader-error p "internal error in get-compound-datum" type)])]
                    [(dot)
                        (cond 
                            [(eq? type 'list)
                                (receive (lextype x) (get-lexeme p)
                                    (receive (d d^) (handle-lexeme p lextype x labels #t)
                                        (begin 
                                            (receive (termtype _) (get-lexeme p)
                                                (cond
                                                    [(eq? termtype terminator)]
                                                    [(eq? termtype 'eof) (eof-warning p)]
                                                    [else (reader-warning p "Improperly terminated dot list")]))
                                            (cond 
                                                [(pair? prev)
                                                    (set-cdr! prev d)
                                                    (set-cdr! prev^ d^)]
                                                [else (reader-warning p "unexpected dot")])
                                            (values head (annotate src head head^)))))]
                            [else
                                (reader-warning p "Dot used in non-list datum")
                                (lp head head^ prev prev^ len)])]
                    [else
                        (receive (d d^) (handle-lexeme p lextype x labels #t)
                            (cond 
                                [(and (eq? type 'bytevector)
                                      (not (and (fixnum? d) (<= 0 d 255))))
                                    (reader-warning p "Non-byte in bytevector" d)
                                    (lp head head^ prev prev^ len)]
                                [else 
                                    (let ([new-prev (cons d '())]
                                          [new-prev^ (cons d^ '())])
                                        (when (pair? prev)
                                            (set-cdr! prev new-prev)
                                            (set-cdr! prev^ new-prev^))
                                        
                                        (if (pair? head)
                                            (lp head head^ new-prev new-prev^ (+ 1 len))
                                            (lp new-prev new-prev^ new-prev new-prev^ (+ 1 len))))]))])))))

(define (handle-lexeme p lextype x labels allow-refs?)
    (let ([src (reader-source p)])
        (case lextype 
            [(openp)
                (get-compound-datum p src 'closep 'list labels)]
            [(openb)
                (get-compound-datum p src 'closeb 'list labels)]
            [(vector)
                (get-compound-datum p src 'closep 'vector labels)]
            [(bytevector)
                (get-compound-datum p src 'closep 'bytevector labels)]
            [(value eof identifier)
                (values x (annotate src x x))]
            [(abbrev)
                (receive (type lex) (get-lexeme p)
                    (cond 
                        [(eq? type 'eof)
                            (eof-warning p)
                            (values lex lex)]
                        [else 
                            (receive (d d^) (handle-lexeme p type lex labels #t)
                                (let ([s (list x d)])
                                    (values s (annotate src s (list x d^)))))]))]
            [else (reader-warning p "unexpected lexeme" lextype x)])))

(set! read-datum (lambda (reader)
    (let ([labels #f])
        (receive (type x) (get-lexeme reader)
            (receive (d _) (handle-lexeme reader type x labels #f)
            d)))))
(set! read-annotated (lambda (reader)
    (if (not (reader? reader))
        (assertion-violation 'read-annotated "not a reader" reader))
    (receive (type x) (get-lexeme reader)
        (receive (_ d^) (handle-lexeme reader type x #f #f)
            d^))))

(set! get-token reader:get-token))

(define (get-port-reader port fn) 
  (let ([r (io/port-reader port)])
    (if r 
      r
      (begin 
        (let ([reader (make-reader port (or fn (port-name port)))])
          (reader-mode-set! reader 'r6rs)
          (io/port-reader-set! port reader)
          reader)))))
      
(define (get-datum p)
  (read-datum (get-port-reader p #f)))

(define (read . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (get-datum p)))

(define (read-syntax . rest)
  "Read a datum with source annotations. Produces #<syntax> objects."
  (define port (if (null? rest) (current-input-port) (car rest)))
  (define exp (read-annotated (get-port-reader port #f)))
  (cond 
    [(and (syntax? exp) (eof-object? (syntax-expression exp)))
     (eof-object)]
    [else exp]))