


(import (libc termios)
        (scheme base)
        (core foreign)
        (core foreign-library)
        (core exceptions)
        (rnrs records syntactic (6))
        (rope))


(define (enable-raw-mode port)
    (define fd (port-fd port))
    (define raw (tcgetattr fd))

    (define iflag (logand (termios-input-flags raw)
                          (lognot (logior BRKINT ICRNL INPCK ISTRIP IXON))))
    (define oflag (logand (termios-output-flags raw) (lognot OPOST)))
    (define cflag (logior (termios-control-flags raw) CS8))
    (define lflag (logand (termios-local-flags raw)
                          (lognot (logior ECHO ICANON IEXTEN ISIG))))
    (define cc (termios-control-chars raw))
    ; modify termios inline
    (list-set! cc VMIN 1)
    (list-set! cc VTIME 0)
    (set-termios-control-chars! raw cc)
    (set-termios-input-flags! raw iflag)
    (set-termios-output-flags! raw oflag)
    (set-termios-control-flags! raw cflag)
    (set-termios-local-flags! raw lflag)
    (tcsetattr fd TCSAFLUSH raw))

(define (disable-raw-mode port orig)
    (define fd (port-fd port))
    (tcsetattr fd TCSAFLUSH orig))

(define (with-raw-mode port proc)
    (define fd (port-fd port))
    (define orig (tcgetattr fd))
    (dynamic-wind 
        (lambda () (enable-raw-mode port))
        (lambda () (proc))
        (lambda () 
            (disable-raw-mode port orig))))

;; This function retrieves the cursor position.
;; It returns two values: (values row col)
(define (get-cursor-position)
  ;; Assuming the read-response logic is encapsulated, we would call it
  ;; and parse the result. For a full example, let's combine it.
  (call-with-values
      (lambda ()
        (let ((out (current-output-port)) (in (current-input-port)))
          (format out "\x1b;[6n~!")
          ;; Skip the leading ESC and [
          (get-char in)
          (get-char in)
          (let loop ((acc '()) (char (get-char in)))
            (if (char=? char #\R)
                (let ((str (list->string (reverse acc))))
                  (apply values (map string->number (string-split str #\;))))
                (loop (cons char acc) (get-char in))))))
    (lambda (row col)
      (values row col))))

(define (get-columns in out)
    (receive (row start) (get-cursor-position)
        (format out "\x1b;[999C")
        (receive (row end) (get-cursor-position)
            (when (> end start)
                (format out "\x1b;[~aD" (- end start)))
            end)))
    
(define (clear-screen)
    (format #t "\x1b;[H\x1b;[2J~!"))

(define (beep)
    (format #t "\x07;~!"))


(define-record-type 
    (<line-buffer> make-line-buffer line-buffer?)
    (fields 
        (mutable buf line-buffer-buf set-line-buffer-buf!)
        (mutable pos line-buffer-pos set-line-buffer-pos!)))

(define (line-buffer-insert lb c)
    (define buf (line-buffer-buf lb))
    (define pos (line-buffer-pos lb))
    (define s (if (char? c) (string c) c))
    (set-line-buffer-buf! lb 
        (rope-insert buf pos s))
    (set-line-buffer-pos! lb (+ pos (string-length s))))

(define (line-buffer-delete lb)
    (define buf (line-buffer-buf lb))
    (define pos (line-buffer-pos lb))
    (when (> pos 0)
        (set-line-buffer-buf! lb 
            (rope-kill buf (- pos 1)))
        (set-line-buffer-pos! lb (- pos 1))))

(define (line-buffer-backspace lb)
    (define buf (line-buffer-buf lb))
    (define pos (line-buffer-pos lb))
    (when (> pos 0)
        (set-line-buffer-pos! lb (- pos 1))
        (set-line-buffer-buf! lb 
            (rope-kill buf (- pos 1)))))

(define (line-buffer-move-left lb)
    (define pos (line-buffer-pos lb))
    (when (> pos 0)
        (set-line-buffer-pos! lb (- pos 1))))

(define (line-buffer-move-right lb)
    (define buf (line-buffer-buf lb))
    (define pos (line-buffer-pos lb))
    (when (< pos (rope-length buf))
        (set-line-buffer-pos! lb (+ pos 1))))


(define (line-buffer-move-home lb)
    (set-line-buffer-pos! lb 0))
(define (line-buffer-move-end lb)
    (define buf (line-buffer-buf lb))
    (set-line-buffer-pos! lb (rope-length buf)))

(define (line-buffer-clear lb)
    (set-line-buffer-buf! lb (make-rope ""))
    (set-line-buffer-pos! lb 0))

(define (line-buffer-set! lb string)
    (set-line-buffer-buf! lb (make-rope string))
    (set-line-buffer-pos! lb (string-length string)))

(define (line-buffer-write lb port)
    (write-rope (line-buffer-buf lb) port))

(define-record-type 
    (<editor> %make-editor editor?)
    (fields 
        (immutable in editor-in)
        (immutable out editor-out)
        (immutable line-buffer editor-line-buffer)
        (mutable prompt editor-prompt set-editor-prompt!)
        (mutable history-index editor-history-index set-editor-history-index!)
        (mutable saved-line editor-saved-line set-editor-saved-line!)
        (mutable old-rows editor-old-rows set-editor-old-rows!)
        (mutable cursor-row-off editor-cursor-row-off set-editor-cursor-row-off!)
        (mutable completion-state editor-completion set-editor-completion!)
        (mutable ln editor-ln set-editor-ln!)))

(define-record-type 
    (<completion-state> completion-state completion-state?)
    (fields 
        (mutable buffer completion-state-buffer set-completion-state-buffer!)
        (mutable index completion-state-index set-completion-state-index!)
        (mutable cursor-pos completion-state-cursor-pos set-completion-state-cursor-pos!)))
    

(define (make-editor in out prompt)
    (%make-editor in out 
        (make-line-buffer (make-rope "") 0)
        prompt
        0
        #f
        0
        0
        #f
        #f))

(define key:ctrl-a 1)
(define key:ctrl-e 5)
(define key:ctrl-b 2)
(define key:ctrl-c 3)
(define key:ctrl-d 4)
(define key:ctrl-f 6)
(define key:ctrl-h 8)
(define key:tab 9)
(define key:ctrl-k 11)
(define key:ctrl-l 12)
(define key:enter 13)
(define key:ctrl-n 14)
(define key:ctrl-p 16)
(define key:ctrl-t 20)
(define key:ctrl-u 21)
(define key:ctrl-w 23)
(define key:esc 27)
(define key:backspace 127)

(define (editor-refresh-single-line editor)
    (define in (editor-in editor))
    (define out (editor-out editor))
    (define lb (editor-line-buffer editor))
    (define prompt (editor-prompt editor))
    (define cursor-pos (+ (string-length prompt) (line-buffer-pos lb)))

    
    ;; Move cursor to beginning
    (put-char out #\return)
    ;(format out "\x1b;[H")
    (format out "~a" prompt)
    (line-buffer-write lb out)
    ;; Clear to end of the line
    (format out "\x1b;[0K")
    ;; position cursor
    (put-char out #\return)
    (format out "\x1b;[~aC" cursor-pos)
    (flush-output-port out))

(define (editor-refresh-multi-line editor)
    (define in (editor-in editor))
    (define out (editor-out editor))
    (define lb (editor-line-buffer editor))
    (define prompt (editor-prompt editor))
    (define cursor-pos (+ (string-length prompt) (line-buffer-pos lb)))
    (define plen (string-length prompt))
    (define cols (get-columns in out))
    (define blen (rope-length (line-buffer-buf lb)))
    (define clen (+ plen blen))

    (define content-rows 
        (if (zero? clen)
            1
            (quotient (- (+ clen cols) 1) cols)))

    (define phantom-line (and (= (line-buffer-pos lb) blen)
                              (> cursor-pos 0)
                              (= (modulo cursor-pos cols) 0)))
    (define total-rows (if phantom-line 
                           (+ content-rows 1) 
                           content-rows))
    
    (define cursor-row (cond 
                         [phantom-line 0]
                         [(zero? cursor-pos) 0]
                         [(+ 1 (remainder (- cursor-pos 1) cols))]))
    (define cursor-col (cond 
                         [phantom-line 0]
                         [(zero? cursor-pos) plen]
                         [(remainder (- cursor-pos 1) cols)]))

    
    (put-char out #\return)
    (if (> (editor-cursor-row-off editor) 0)    
        (format out "\x1b;[~aA" (editor-cursor-row-off editor)))
    
    ;; clear everything
    (let ([rows-to-clear (max (editor-old-rows editor) total-rows)])
        (do ((i 0 (+ i 1)))
            ((= i rows-to-clear))
            (when (> i 0)
                (put-char out #\return)
                (put-char out #\newline))
            (format out "\x1b;[2K"))
        (when (> rows-to-clear 0)
            (format out "\x1b;[~aA" (- rows-to-clear 1))))
    (put-char out #\return)

    (format out "~a" prompt)
    (write-rope (line-buffer-buf lb) out)
    ;(when (and (= content-rows 0) (not (editor-completion editor)))
    ;    #f)

    (when phantom-line 
        (put-char out #\return)
        (put-char out #\newline))
    
    (let ([current-row (- total-rows 1)])
        (if (< cursor-row current-row)
            (format out "\x1b;[~aB" (- current-row cursor-row))
            (format out "\x1b;[~aA" (- cursor-row current-row)))
        (set-editor-cursor-row-off! editor (- total-rows cursor-row)))
    (put-char out #\return)
    (format out "\x1b;[~aC" cursor-col)
    (set-editor-old-rows! editor total-rows)
    (flush-output-port out))
    

(define (editor-refresh-line editor)
    (editor-refresh-single-line editor))

(define (editor-handle-completion editor)
    (define callback (ln-completion-callback (editor-ln editor)))

    (cond 
        [(procedure? callback) 
            (let* ([line-for-completion (if (editor-completion editor)
                                           (write-rope (completion-state-buffer (editor-completion editor)) #f)
                                           (write-rope (line-buffer-buf (editor-line-buffer editor)) #f))]
                   [cursor-pos (if (editor-completion editor)
                                   (completion-state-cursor-pos (editor-completion editor))
                                   (line-buffer-pos (editor-line-buffer editor)))]
                   [completions (callback line-for-completion cursor-pos)])
                (cond 
                    [(null? completions)
                        (beep)
                        (set-editor-completion! editor #f)
                        #f]
                    [(editor-completion editor)
                        => 
                        (lambda (comp-state)
                            (set-completion-state-index! comp-state 
                                (remainder (+ (completion-state-index comp-state) 1) (length completions)))
                            (cond 
                                [(< (completion-state-index comp-state) (length completions))
                                    (line-buffer-set! (editor-line-buffer editor)
                                        (list-ref completions (completion-state-index comp-state)))
                                    (editor-refresh-line editor)])
                            #t)]
                    [else 
                        ; first tab: start completion mode
                        (set-editor-completion! editor
                            (completion-state (line-buffer-buf (editor-line-buffer editor)) 0 cursor-pos))
                        (line-buffer-set! (editor-line-buffer editor) (list-ref completions 0))
                        (editor-refresh-line editor)
                        #t]))]
        [else #f]))
(define (editor-accept-completion editor)
    (set-editor-completion! editor #f))

(define (editor-handle-escape-sequence editor)
    (define in (editor-in editor))
    (define c1 (read-char in))
    (define c2 (read-char in))

    (cond 
        [(eof-object? c1) #f]
        [(eof-object? c2) #f]
        [(char=? c1 #\[)
            (cond 
                [(char=? c2 #\D) ; left arrow
                    (line-buffer-move-left (editor-line-buffer editor))
                    (editor-refresh-line editor)
                    #f]
                [(char=? c2 #\C) ; right arrow
                    (line-buffer-move-right (editor-line-buffer editor))
                    (editor-refresh-line editor)
                    #f]
                [(or (char=? c2 #\F) (char=? c2 #\E)) ; end
                    (line-buffer-move-end (editor-line-buffer editor))
                    (editor-refresh-line editor)
                    #f]
                [(or (char=? c2 #\H) (char=? c2 #\S)) ; home
                    (line-buffer-move-home (editor-line-buffer editor))
                    (editor-refresh-line editor)
                    #f]
                [else #f])]
        [else #f]))

(define (editor-process-key editor c)
    (when (and (editor-completion editor)
               (not (eq? (char->integer c) key:tab)))
        (editor-accept-completion editor))

    (cond 
        [(eq? (char->integer c) key:enter) (editor-line-buffer editor)]
        [(eq? (char->integer c) key:ctrl-c) 'interrupted]
        [(eq? (char->integer c) key:tab)
            (editor-handle-completion editor)
            #f]
        [(eq? (char->integer c) key:backspace)
            (line-buffer-backspace (editor-line-buffer editor))
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:ctrl-b)
            (line-buffer-move-left (editor-line-buffer editor))
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:ctrl-f)
            (line-buffer-move-right (editor-line-buffer editor))
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:ctrl-a)
            (line-buffer-move-home (editor-line-buffer editor))
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:ctrl-e)
            (line-buffer-move-end (editor-line-buffer editor))
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:ctrl-l)
            (clear-screen)
            (editor-refresh-line editor)
            #f]
        [(eq? (char->integer c) key:esc)
            (editor-handle-escape-sequence editor)
            #f]
        [else 
            (line-buffer-insert (editor-line-buffer editor) c)
            (editor-refresh-line editor)
            #f]))

(define-record-type 
    (<linenoise-state> %make-linenoise-state linenoise-state?)
    (fields 
        (mutable active ln-active set-ln-active!)
        (immutable editor ln-editor)
        (mutable history ln-history set-ln-history!)
        (mutable completion-callback ln-completion-callback set-ln-completion-callback!)
        (mutable hints-callback ln-hints-callback set-ln-hints-callback!)
        (mutable validator ln-validator set-ln-validator!)))

(define (make-linenoise-state in out prompt)
    (define ln (%make-linenoise-state 
        #f
        (make-editor in out prompt)
        '()
        #f 
        #f
        #f))
    (set-editor-ln! (ln-editor ln) ln)
    ln)

(define (linenoise-edit-feed ln)
    (define editor (ln-editor ln))
    (define in (editor-in editor))
    (define out (editor-out editor))
    (cond 
        [(not (ln-active ln)) #f]
        [else 
            (let ([c (read-char in)])
                (let ([result (editor-process-key editor c)])
                    (cond 
                        [(eq? result 'interrupted)
                            (set-ln-active! ln #f)
                            'interrupted]
                        [(line-buffer? result)
                            (let ([validator (ln-validator ln)])
                                (cond 
                                    [(and validator)
                                        (let ([res (validator (line-buffer-buf result))])
                                            (cond 
                                                [res 
                                                    (put-char out #\return)
                                                    (put-char out #\newline)
                                                    (set-ln-active! ln #f)
                                                    (set-line-buffer-buf! (editor-line-buffer editor) (make-rope ""))
                                                    (set-line-buffer-pos! (editor-line-buffer editor) 0)
                                                    res]
                                                [else #f]))]
                                    [(not validator)
                                        (put-char out #\return)
                                        (put-char out #\newline)
                                        (set-ln-active! ln #f)
                                        (write-rope (line-buffer-buf result) #f)]
                                    [else #f]))]
                        [else result])))]))

(define (linenoise-edit-start ln)
    (define editor (ln-editor ln))
    (define out (editor-out editor))
    (set-ln-active! ln #t)
    (set-editor-history-index! editor 0)
    (set-editor-saved-line! editor #f)

    (editor-refresh-line editor))


(define (linenoise-edit-stop ln)
    (when (ln-active ln)
        (set-ln-active! ln #f)))

(define (linenoise-hide ln)
    (define out (editor-out (ln-editor ln)))
    (put-char #\return)
    (format out "\x1b;[0K"))

(define (linenoise-show ln)
    (define editor (ln-editor ln))
    (editor-refresh-line editor))

(define (linenoise ln)
    (with-raw-mode 
        (editor-in (ln-editor ln))
        (lambda ()
            (linenoise-edit-start ln)
            (let lp ()
                (let ([res (linenoise-edit-feed ln)])
                    (cond 
                        [(string? res) res]
                        [(eq? res 'interrupted)
                            (linenoise-edit-stop ln)
                            'interrupted]
                        [(eq? res #f)
                            (lp)]
                        [else 
                            (linenoise-edit-stop ln)
                            res]))))))



(define ln (make-linenoise-state 
            (current-input-port) 
            (current-output-port) 
            ">>> "))

(set-ln-validator! ln 
    (lambda (x)
        (define input (write-rope x #f))
        (with-exception-handler 
            (lambda (ex) (format #t "invalid input ~a: ~a ~a ~%~!" input (condition-who ex) (condition-message ex)) #f)
            (lambda () (read-syntax (open-input-string input))))))
(set-ln-completion-callback! ln 
    (lambda (input cursor-pos)
        (define words (string-split input #\space))
        (define chars-before-cursor (substring input 0 cursor-pos))
        (define words-before-cursor (string-split chars-before-cursor #\space))
        (define current-word (if (null? words-before-cursor) 
                                ""
                                (car (reverse words-before-cursor))))
        (if (string-prefix? "he" current-word)
            (list "hello" "help" "here")
            '())))

(let loop ()
    (define line (linenoise ln))
    (cond 
        [(eq? line 'interrupted)
            (format #t "Input interrupted.~%")
            #f]
        [else
            (when line
                (receive vals (eval line)
                    (cond 
                        [(null? vals)
                            #f]
                        [(= (length vals) 1)
                            (format #t "=> ~a~%~!" (car vals))]
                        [else 
                            (for-each 
                                (lambda (v)
                                    (format #t "=> ~a~%~!" v))
                                vals)])))
            (loop)]))