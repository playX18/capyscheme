(define-library (scmline line-buffer)
    (import (scheme base)
            (rope)
            (rnrs records syntactic (6)))
    (export)
(begin 

(define-record-type
    (<line-buffer> %make-line-buffer line-buffer?)
    (fields
        (mutable buf line-buffer-buf set-line-buffer-buf!)
        (mutable pos line-buffer-pos set-line-buffer-pos!)
        (mutable can-grow? line-buffer-can-grow? set-line-buffer-can-grow?!)))

(define (dummy-change-listener action)
    (case action 
        [(insert) (lambda (index s) #f)]
        [(delete) (lambda (index s dir) #f)]
        [(start-killing) (lambda () #f)]
        [(stop-killing) (lambda () #f)]))

(define (make-line-buffer)
    (%make-line-buffer (make-rope "")
                       0
                       #f))

(define (must-truncate? lb)
    (and (not (line-buffer-can-grow? lb))
         (> (rope-length (line-buffer-buf lb))
            (line-buffer-pos lb))))

(define (line-buffer->string lb)
    (write-rope (line-buffer-buf lb) #f))

(define (line-buffer-at-end? lb)
    "Returns #t if the line buffer's position is at the end of the buffer."
    (= (line-buffer-pos lb)
       (rope-length (line-buffer-buf lb))))

(define (line-buffer-len lb)
    "Returns the length of the line buffer's content."
    (rope-length (line-buffer-buf lb)))

(define (line-buffer-empty? lb)
    "Returns #t if the line buffer is empty."
    (= (rope-length (line-buffer-buf lb)) 0))

(define (line-buffer-end-of-line lb)
    (cond 
        [(rope-find (line-buffer-buf lb) #\newline (line-buffer-pos lb))
         => (lambda (n) (+ n (line-buffer-pos lb)))]
        [else (rope-length (line-buffer-buf lb))]))

(define (line-buffer-start-of-line lb)
    (cond 
        [(rope-rfind (line-buffer-buf lb) #\newline 0 (line-buffer-pos lb))
         => (lambda (n) (+ n 1))]
        [else 0]))

(define (line-buffer-next-pos lb n)
    (if (line-buffer-at-end? lb)
        #f
        (+ n (line-buffer-pos lb))))

(define (line-buffer-previous-pos lb n)
    (if (= (line-buffer-pos lb) 0)
        #f
        (- (line-buffer-pos lb) n)))

(define (line-buffer-insert-str lb idx s cl)
    (cond 
        [(string? s) (line-buffer-insert-str lb idx (make-rope s) cl)]
        [(char? s) (line-buffer-insert-str lb idx (string s) cl)]
        [else 
            ((cl 'insert) idx s)
            (set-line-buffer-buf! lb
                                   (rope-insert (line-buffer-buf lb)
                                                idx
                                                s))]))

(define (line-buffer-length lb)
    (rope-length (line-buffer-buf lb)))

(define (line-buffer-update lb buf pos cl)
    (define end (line-buffer-length lb))
    (line-buffer-drain lb 0 end 'forward cl)
    (line-buffer-insert-str lb 0 buf cl)
    (set-line-buffer-pos! lb pos))

(define (line-buffer-drain lb start end dir cl)
    (define s (rope-substring (line-buffer-buf lb) start end))
    (define new-buf (rope-kill (line-buffer-buf lb) start end))
    ((cl 'delete) start s dir)
    (set-line-buffer-buf! lb new-buf)
    s)

(define (line-buffer-insert lb ch n cl)
    (define push? (line-buffer-at-end? lb))
    (define s (if (= n 1)
                  (string ch)
                  (make-string n ch)))
    (define index (line-buffer-pos lb))
    (line-buffer-insert-str lb index s cl)
    (set-line-buffer-pos! lb (+ (line-buffer-pos lb) n))
    push?)

(define (line-buffer-move-backward lb n)
    (cond 
        [(line-buffer-previous-pos lb n)
         => (lambda (new-pos)
              (set-line-buffer-pos! lb new-pos)
              #t)]
        [else #f]))

(define (line-buffer-move-forward lb n)
    (cond 
        [(line-buffer-next-pos lb n)
         => (lambda (new-pos)
              (set-line-buffer-pos! lb new-pos)
              #t)]
        [else #f]))

(define (line-buffer-move-buffer-start lb)
    (if (> (line-buffer-pos lb) 0)
        (begin
            (set-line-buffer-pos! lb 0)
            #t)
        #f))

(define (line-buffer-move-buffer-end lb)
    (if (line-buffer-at-end? lb)
        #f
        (begin
            (set-line-buffer-pos! lb (line-buffer-length lb))
            #t)))

(define (line-buffer-move-home lb)
    (define start (line-buffer-start-of-line lb))
    (if (> (line-buffer-pos lb) start)
        (begin
            (set-line-buffer-pos! lb start)
            #t)
        #f))
(define (line-buffer-move-end lb)
    (define end (line-buffer-end-of-line lb))
    (if (< (line-buffer-pos lb) end)
        (begin
            (set-line-buffer-pos! lb end)
            #t)
        #f))

(define (line-buffer-end-of-input? lb)
    (line-buffer-at-end? lb))

(define (line-buffer-delete lb n cl)
    (cond 
        [(line-buffer-next-pos lb n)
         => (lambda (pos)
            (define start (line-buffer-pos lb))
            (line-buffer-drain lb start pos 'forward cl))]
        [else #f]))

(define (line-buffer-backspace lb cl)
    (cond 
        [(line-buffer-previous-pos lb)
         => (lambda (pos)
            (define end (line-buffer-pos lb))
            (define s (line-buffer-drain lb pos end 'backward cl))
            (set-line-buffer-pos! lb pos)
            s)]
        [else #f]))

(define (line-buffer-kill-line lb cl)
    (cond 
        [(and (not (line-buffer-empty? lb))
              (< (line-buffer-pos lb)
                 (line-buffer-length lb)))
            (let* ([start (line-buffer-pos lb)]
                   [end (line-buffer-end-of-line lb)])
              (if (= start end)
                  (line-buffer-delete lb 1 cl)
                  (line-buffer-drain lb start end 'forward cl)))
            #t]
        [else #f]))

(define (line-buffer-drain-buffer lb cl)
    (cond 
        [(and (not (line-buffer-empty? lb))
              (< (line-buffer-pos) (line-buffer-length lb)))
            (let* ([start (line-buffer-pos lb)]
                   [end (line-buffer-length lb)])
              (line-buffer-drain lb start end 'forward cl))
            #t]
        [else #f]))

(define (line-buffer-discard-line lb cl)
    (cond 
        [(and (> (line-buffer-pos lb) 0)
              (not (line-buffer-empty? lb)))
            (let ([start (line-buffer-start-of-line lb)]
                  [end (line-buffer-pos lb)])
                (cond 
                    [(= start end)
                     (line-buffer-backspace lb cl)]
                    [else 
                        (line-buffer-drain lb start end 'backward cl)
                        (set-line-buffer-pos! lb start)
                        #t]))]
        [else #f]))

(define (line-buffer-discard-buffer lb cl)
    (cond 
        [(and (> (line-buffer-pos lb) 0)
              (not (line-buffer-empty? lb)))
            (let ([start 0]
                  [end (line-buffer-pos lb)])
                (line-buffer-drain lb start end 'backward cl)
                (set-line-buffer-pos! lb 0)
                #t)]
        [else #f]))

(define (string-repeat s n)
    (if (= n 0)
        ""
        (string-append s (string-repeat s (- n 1)))))

(define (line-buffer-yank lb s n cl)
    (cond 
        [(zero? (string-length s))
         #f]
        [else 
            (let* ([push (line-buffer-at-end? lb)]
                   [pos (line-buffer-pos lb)]
                   [shift (* n (string-length s))])
                (if (= n 1)
                    (line-buffer-insert-str lb pos s cl)
                    (line-buffer-insert-str lb pos (string-repeat s n) cl))
                (set-line-buffer-pos! lb (+ pos shift))
                push)]))

(define (line-buffer-transpose-chars lb cl)
    (cond 
        [(or (zero? (line-buffer-pos lb))
             (< (line-buffer-length lb) 2))
         #f]
        [else
            (if (line-buffer-at-end? lb)
                (line-buffer-move-backward lb 1))
            (let ([chars (line-buffer-delete lb 1 cl)])
                (format #t "deleted=~a~%" chars)
                (line-buffer-move-backward lb 1)
                (line-buffer-yank lb chars 1 cl)
                (line-buffer-move-forward lb 1)
                #t)]))

(define (word-char? ch)
    (or (and (char>=? ch #\a) (char<=? ch #\z))
        (and (char>=? ch #\A) (char<=? ch #\Z))
        (and (char>=? ch #\0) (char<=? ch #\9))
        (char=? ch #\_)))

(define (other-char? ch)
    (not (or (word-char? ch)
             (char=? ch #\space)
             (char=? ch #\newline)
             (char=? ch #\tab))))

(define (start-of-word? previous ch)
    (or (and (not (word-char? previous)) (word-char? ch))
        (and (not (other-char? previous)) (other-char? ch))))

(define (end-of-word? ch next)
    (or (and (word-char? ch) (not (word-char? next)))
        (and (other-char? ch) (not (other-char? next)))))


;;; New Implementation of line-buffer-prev-word-pos
(define (line-buffer-prev-word-pos lb start-pos n)
    "Moves the position backward by N words from START-POS, based on start-of-word?."
    (define buf (line-buffer-buf lb))
    (define len (rope-length buf))

    ;; Helper function to find the position of the N-th previous word boundary (i.e., the start of the word)
    (let find-prev-word ((current-pos start-pos) (words-to-find n))
        (cond
            ;; Stop if we've found N words
            [(zero? words-to-find) current-pos]

            ;; Stop if we hit the beginning of the buffer
            [(= current-pos 0) (if (= words-to-find n) #f 0)]

            [else
                ;; Start searching backward from the character *before* current-pos
                (let loop ((i (- current-pos 1)))
                    (cond
                        [(= i 0) ; Reached index 0
                            (let ([ch (rope-index buf 0)])
                                (if (or (word-char? ch) (other-char? ch))
                                    ;; If char at 0 is part of a word, it's a boundary
                                    (find-prev-word 0 (- words-to-find 1))
                                    ;; If it's a separator, the boundary is still 0 (or #f if n=1)
                                    (if (= words-to-find 1) #f (find-prev-word 0 (- words-to-find 1)))))]

                        [(< i 0) #f] ; Should not happen

                        [else
                            (let* ([previous (rope-index buf (- i 1))] ; The 'previous' char
                                   [ch (rope-index buf i)])  ; The 'current' char
                                (if (start-of-word? previous ch)
                                    ;; Found a word boundary. 'i' is the position *at* the start of the word
                                    (find-prev-word i (- words-to-find 1))
                                    ;; Not a boundary, keep searching left
                                    (loop (- i 1))))]))])))

(define (line-buffer-move-to-prev-word lb n)
    (cond 
        [(line-buffer-prev-word-pos lb (line-buffer-pos lb) n)
         => (lambda (new-pos)
              (set-line-buffer-pos! lb new-pos)
              #t)]
        [else #f]))

(define (line-buffer-next-word-pos lb start-pos n)
    "Moves the position forward by N words from START-POS, based on end-of-word?."
    (define buf (line-buffer-buf lb))
    (define len (rope-length buf))

    ;; Helper function to find the position of the N-th next word boundary (i.e., the end of the word)
    (let find-next-word ((current-pos start-pos) (words-to-find n))
        (cond
            ;; Stop if we've found N words
            [(zero? words-to-find) current-pos]

            ;; Stop if we hit or pass the end of the buffer
            [(>= current-pos len)
             (if (= words-to-find n) #f len)]

            [else
                ;; Start searching forward from current-pos
                (let loop ((i current-pos))
                    (cond
                        [(= i len) ; Reached the end of the buffer
                            (if (= words-to-find 1) len (find-next-word len (- words-to-find 1)))]

                        [(> i len) #f] ; Should not happen

                        [else
                            (let* ([ch (rope-index buf i)] ; The 'current' char
                                   [next (if (< (+ i 1) len)
                                             (rope-index buf (+ i 1))
                                             #\space)]) ; Treat end of buffer as followed by a space
                                (if (end-of-word? ch next)
                                    ;; Found a word boundary. 'i+1' is the position *after* the word
                                    (find-next-word (+ i 1) (- words-to-find 1))
                                    ;; Not a boundary, keep searching right
                                    (loop (+ i 1))))]))])))

(define (line-buffer-move-to-next-word lb n)
    (cond 
        [(line-buffer-next-word-pos lb (line-buffer-pos lb) n)
         => (lambda (new-pos)
              (set-line-buffer-pos! lb new-pos)
              #t)]
        [else #f]))

(define (line-buffer-delete-prev-word lb n cl)
    (cond 
        [(line-buffer-prev-word-pos lb (line-buffer-pos lb) n)
         => (lambda (pos)
                (define end (line-buffer-pos lb))
                (line-buffer-drain lb pos end 'backward cl)
                (set-line-buffer-pos! lb pos)
                #t)]
        [else #f]))

(define (line-buffer-delete-next-word lb n cl)
    (cond 
        [(line-buffer-next-word-pos lb (line-buffer-pos lb) n)
         => (lambda (pos)
                (define start (line-buffer-pos lb))
                (line-buffer-drain lb start pos 'forward cl)
                #t)]
        [else #f]))
))