(define-library (scmline tty)
    (import 
            (scmline layout)
            (scmline line-buffer)
            (scheme base)
            (rnrs records syntactic (6))
            (libc termios))
    (export 
        with-raw-mode
        get-window-size
        port-move-cursor
        port-refresh-line
        port-clear-screen
        port-calculate-position
    )
(begin 


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

(define (get-window-size fd)
    (with-exception-handler 
        (lambda (exn)
            (format #t "Warning: could not get terminal size: ~a~%" (condition-who exn))
            (values 80 24))
        (lambda ()
            (define ws (get-winsize fd))
            (values (winsize-cols ws) (winsize-rows ws)))))

(define vals (receive x (get-window-size (port-fd (current-output-port)))
                x))
(define wcols (list-ref vals 0))
(define wrows (list-ref vals 1))

(format #t "Terminal size: ~a cols, ~a rows~%~!" wcols wrows)

(define-record-type 
    (<posix-raw-reader> make-posix-raw-reader posix-raw-reader?)
    (fields 
        (immutable in reader-in)))

(define (reader-next-char r)
    (read-char (reader-in r)))

(define (port-move-cursor port old new)
    "Moves cursor from OLD to NEW position on PORT."
    (define old-col (position-col old))
    (define new-col (position-col new))
    (define old-row (position-row old))
    (define new-row (position-row new))
    (cond 
        [(> new-row old-row) ; move down
            (let ([row-shift (- new-row old-row)])
                (cond 
                    [(= row-shift 1)
                        (format port "\x1b;[B")]
                    [else
                        (format port "\x1b;[~aB" row-shift)]))]
        [(< new-row old-row) ; move up
            (let ([row-shift (- old-row new-row)])
                (cond 
                    [(= row-shift 1)
                        (format port "\x1b;[A")]
                    [else
                        (format port "\x1b;[~aA" row-shift)]))])
    
    (cond 
        [(> new-col old-col) ; move right 
            (let ([col-shift (- new-col old-col)])
                (cond 
                    [(= col-shift 1)
                        (format port "\x1b;[C")]
                    [else
                        (format port "\x1b;[~aC" col-shift)]))]
        [(< new-col old-col) ; move left
            (let ([col-shift (- old-col new-col)])
                (cond 
                    [(= col-shift 1)
                        (format port "\x1b;[D")]
                    [else
                        (format port "\x1b;[~aD" col-shift)]))])
    
    (flush-output-port port))

(define (port-refresh-line port 
                           prompt 
                           line 
                           hint 
                           old-layout 
                           new-layout)
    (define default-prompt (layout-default-prompt? new-layout))
    (define cursor (layout-cursor new-layout))
    (define end-pos (layout-end new-layout))

    (when old-layout
        (port-clear-old-rows port old-layout))

    (format port prompt)
    (format port (line-buffer->string line))
    (when hint 
        (format port hint))
    
    (if (and (zero? (position-col end-pos))
             (> (position-row end-pos) 0))
        (put-char port #\newline))

    (let ([new-cursor-row-movement (- (position-row end-pos) (position-row cursor))])
        (if (> new-cursor-row-movement 0)
            (format port "\x1b;[~aA" new-cursor-row-movement)))
    (if (> (position-col cursor) 0)
        (format port "\r\x1b;[~aC" (position-col cursor))
        (put-char port #\return))
    (flush-output-port port))

(define (port-clear-screen port)
    (format port "\x1b;[H\x1b;[J"))

(define (port-calculate-position port s orig)
    (let loop ([i 0] [pos orig] [esc-seq 0])
        (cond 
            [(< i (string-length s))
                (let ([c (string-ref s i)])
                    (cond 
                        [(char=? c #\newline)
                            (format #t "Newline at col ~a~%~!" (position-col pos))
                            (set-position-row! pos (+ (position-row pos) 1))
                            (set-position-col! pos 0)
                            (loop (+ i 1) pos esc-seq)]
                        [else 
                            (receive (cw new-esc-seq)
                                (if (char=? c #\tab)
                                    (values (- 4 (remainder (position-col pos) 4))
                                            esc-seq)
                                    (width (string c) esc-seq))
                                (begin 
                                    (set-position-col! pos (+ cw (position-col pos)))
                                    (when (> (position-col pos) wcols)
                                            (set-position-row! pos (+ 1 (position-row pos)))
                                            (set-position-col! pos cw))
                                    (loop (+ i 1) pos new-esc-seq)))]))]
            [else 
                (when (= (position-col pos) wcols)
                    (set-position-col! pos 0)
                    (set-position-row! pos (+ 1 (position-row pos))))
                pos])))

(define (width s esc-seq)
    (cond 
        [(= esc-seq 1)
            (if (string=? s "[")
                (values 0 2) ;; CSI
                (values 0 0))] ;; two-character sequence
        [(= esc-seq 2)
            (let* (
                [c (string-ref s 0)]
                [new (if (or (char=? c #\;) 
                             (and (char>=? c #\0) (char<=? c #\9)))
                        esc-seq 
                        0)])
                (values 0 new))]
        [(string=? s "\x1b;")
            (values 0 1)]
        [(string=? s "\n")
            (values 0 esc-seq)]
        [else (values (string-length s) esc-seq)]))

(define (port-clear-old-rows port old-layout)
    (define current-row (position-row (layout-cursor old-layout)))
    (define old-rows (position-row (layout-end old-layout)))
    (define cursor-row-movement (if (< old-rows current-row)
                                      0
                                      (- old-rows current-row)))
    
    (if (> cursor-row-movement 0)
        (format port "\x1b;[~aB" cursor-row-movement))
    
    (let loop ([i 0])
        (cond 
            [(< i old-rows)
                (put-char port #\return)
                (format port "\x1b;[K\x1b;[A")
                (loop (+ i 1))]
            [else #f]))
    (put-char port #\return)
    (format port "\x1b;[K"))

(define (port-clear-rows port layout)
    (port-clear-old-rows port layout)
    (flush-output-port port))

(define (port-clear-to-eol port)
    (format port "\x1b;[K~!"))

))