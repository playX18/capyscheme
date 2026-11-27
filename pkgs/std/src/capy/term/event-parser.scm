(library (capy term event-parser)
  (export parse-event)
  (import (rnrs) (rnrs arithmetic bitwise))

  (define (focus-gained-event)
    '(focus-gained))

  (define (focus-lost-event)
    '(focus-lost))

  (define (key-event code modifiers kind state)
    (list 'key code modifiers kind state))

  (define (key-event? event)
    (and (pair? event) (eq? (car event) 'key)))

  (define (key-event-code event) (cadr event))
  (define (key-event-modifiers event) (caddr event))
  (define (key-event-kind event) (cadddr event))
  (define (key-event-state event) (car (cddddr event)))

  (define (mouse-event kind column row modifiers)
    (list 'mouse kind column row modifiers))

  (define (paste-event text)
    (list 'paste text))

  (define (complete event consumed)
    (values 'complete event consumed))

  (define (incomplete)
    (values 'incomplete #f 0))

  (define (invalid consumed)
    (values 'invalid #f consumed))

  (define (u8 bytes index)
    (bytevector-u8-ref bytes index))

  (define (len bytes)
    (bytevector-length bytes))

  (define (ascii-upper? ch)
    (and (char<=? #\A ch) (char<=? ch #\Z)))

  (define (key code modifiers . maybe-kind-state)
    (let ([kind (if (pair? maybe-kind-state) (car maybe-kind-state) 'press)]
          [state (if (and (pair? maybe-kind-state) (pair? (cdr maybe-kind-state)))
                  (cadr maybe-kind-state)
                  '())])
      (key-event code modifiers kind state)))

  (define (char-key ch)
    (key `(char ,ch) (if (ascii-upper? ch) '(shift) '())))

  (define (add-modifier modifier modifiers)
    (if (memq modifier modifiers) modifiers (append modifiers (list modifier))))

  (define (add-alt event)
    (if (key-event? event)
      (key-event (key-event-code event)
        (add-modifier 'alt (key-event-modifiers event))
        (key-event-kind event)
        (key-event-state event))
      event))

  (define (sub-bytevector bytes start end)
    (let* ([n (- end start)]
           [out (make-bytevector n)])
      (bytevector-copy! bytes start out 0 n)
      out))

  (define (ascii-slice bytes start end)
    (utf8->string (sub-bytevector bytes start end)))

  (define (string-split s delimiter)
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [parts '()])
        (cond
          [(= i n) (reverse (cons (substring s start i) parts))]
          [(char=? (string-ref s i) delimiter)
            (loop (+ i 1) (+ i 1) (cons (substring s start i) parts))]
          [else (loop (+ i 1) start parts)]))))

  (define (string-empty? s) (= (string-length s) 0))

  (define (string->nat s)
    (and (not (string-empty? s))
      (let ([n (string->number s)])
        (and (integer? n) (>= n 0) n))))

  (define (parse-modifier-kind field)
    (let* ([parts (string-split field #\:)]
           [mask (and (pair? parts) (string->nat (car parts)))]
           [kind-code (and (pair? parts) (pair? (cdr parts)) (string->nat (cadr parts)))])
      (and mask (cons mask (or kind-code 1)))))

  (define (parse-modifiers mask)
    (let ([bits (max 0 (- mask 1))])
      (let loop ([entries '((1 shift) (2 alt) (4 control) (8 super) (16 hyper) (32 meta))]
                 [out '()])
        (cond
          [(null? entries) (reverse out)]
          [(not (= 0 (bitwise-and bits (caar entries))))
            (loop (cdr entries) (cons (cadar entries) out))]
          [else (loop (cdr entries) out)]))))

  (define (parse-state-from-modifiers mask)
    (let ([bits (max 0 (- mask 1))])
      (append (if (= 0 (bitwise-and bits 64)) '() '(caps-lock))
        (if (= 0 (bitwise-and bits 128)) '() '(num-lock)))))

  (define (parse-key-kind code)
    (case code
      [(2) 'repeat]
      [(3) 'release]
      [else 'press]))

  (define (parse-modifiers-kind-state parts)
    (if (and (pair? parts) (parse-modifier-kind (car parts)))
      (let* ([parsed (parse-modifier-kind (car parts))]
             [mask (car parsed)]
             [kind-code (cdr parsed)])
        (values (parse-modifiers mask)
          (parse-key-kind kind-code)
          (parse-state-from-modifiers mask)))
      (values '() 'press '())))

  (define (functional-code codepoint)
    (cond
      [(and (<= 57399 codepoint) (<= codepoint 57408))
        (cons `(char ,(integer->char (+ (char->integer #\0) (- codepoint 57399)))) '(keypad))]
      [(= codepoint 57409) (cons '(char #\.) '(keypad))]
      [(= codepoint 57410) (cons '(char #\/) '(keypad))]
      [(= codepoint 57411) (cons '(char #\*) '(keypad))]
      [(= codepoint 57412) (cons '(char #\-) '(keypad))]
      [(= codepoint 57413) (cons '(char #\+) '(keypad))]
      [(= codepoint 57414) (cons 'enter '(keypad))]
      [(= codepoint 57415) (cons '(char #\=) '(keypad))]
      [(= codepoint 57416) (cons '(char #\,) '(keypad))]
      [(= codepoint 57417) (cons 'left '(keypad))]
      [(= codepoint 57418) (cons 'right '(keypad))]
      [(= codepoint 57419) (cons 'up '(keypad))]
      [(= codepoint 57420) (cons 'down '(keypad))]
      [(= codepoint 57421) (cons 'page-up '(keypad))]
      [(= codepoint 57422) (cons 'page-down '(keypad))]
      [(= codepoint 57423) (cons 'home '(keypad))]
      [(= codepoint 57424) (cons 'end '(keypad))]
      [(= codepoint 57425) (cons 'insert '(keypad))]
      [(= codepoint 57426) (cons 'delete '(keypad))]
      [(= codepoint 57427) (cons 'keypad-begin '(keypad))]
      [(= codepoint 57358) (cons 'caps-lock '())]
      [(= codepoint 57359) (cons 'scroll-lock '())]
      [(= codepoint 57360) (cons 'num-lock '())]
      [(= codepoint 57361) (cons 'print-screen '())]
      [(= codepoint 57362) (cons 'pause '())]
      [(= codepoint 57363) (cons 'menu '())]
      [(and (<= 57376 codepoint) (<= codepoint 57398))
        (cons `(function ,(+ 13 (- codepoint 57376))) '())]
      [(and (<= 57428 codepoint) (<= codepoint 57440))
        (cons `(media ,(list-ref '(play pause play-pause reverse stop fast-forward rewind
                                   track-next
                                   track-previous
                                   record
                                   lower-volume
                                   raise-volume
                                   mute-volume)
                        (- codepoint 57428)))
          '())]
      [(and (<= 57441 codepoint) (<= codepoint 57454))
        (cons `(modifier ,(list-ref '(left-shift left-control left-alt left-super left-hyper left-meta
                                      right-shift
                                      right-control
                                      right-alt
                                      right-super
                                      right-hyper
                                      right-meta
                                      iso-level3-shift
                                      iso-level5-shift)
                           (- codepoint 57441)))
          '())]
      [else #f]))

  (define (modifier-key-modifier code)
    (and (pair? code)
      (eq? (car code) 'modifier)
      (case (cadr code)
        [(left-alt right-alt) 'alt]
        [(left-control right-control) 'control]
        [(left-shift right-shift) 'shift]
        [(left-super right-super) 'super]
        [(left-hyper right-hyper) 'hyper]
        [(left-meta right-meta) 'meta]
        [else #f])))

  (define (codepoint->key-code codepoint modifiers raw-mode-enabled?)
    (let ([functional (functional-code codepoint)])
      (cond
        [functional functional]
        [(= codepoint #x1b) (cons 'esc '())]
        [(= codepoint #x0d) (cons 'enter '())]
        [(and (= codepoint #x0a) (not raw-mode-enabled?)) (cons 'enter '())]
        [(= codepoint #x09) (cons (if (memq 'shift modifiers) 'backtab 'tab) '())]
        [(= codepoint #x7f) (cons 'backspace '())]
        [else (cons `(char ,(integer->char codepoint)) '())])))

  (define (parse-u-key bytes raw-mode-enabled?)
    (let* ([s (ascii-slice bytes 2 (- (len bytes) 1))]
           [parts (string-split s #\;)]
           [codepoint-parts (and (pair? parts) (string-split (car parts) #\:))]
           [codepoint (and codepoint-parts (string->nat (car codepoint-parts)))])
      (if (not codepoint)
        (invalid (len bytes))
        (call-with-values
          (lambda () (parse-modifiers-kind-state (cdr parts)))
          (lambda (modifiers kind state-from-modifiers)
            (let* ([translated (codepoint->key-code codepoint modifiers raw-mode-enabled?)]
                   [code (car translated)]
                   [state (append (cdr translated) state-from-modifiers)]
                   [modifier (modifier-key-modifier code)]
                   [modifiers (if modifier (add-modifier modifier modifiers) modifiers)]
                   [modifiers
                     (if (and (memq 'shift modifiers)
                          (pair? (cdr codepoint-parts))
                          (string->nat (cadr codepoint-parts)))
                       (let ([shifted (integer->char (string->nat (cadr codepoint-parts)))])
                         (set! code `(char ,shifted))
                         (remq 'shift modifiers))
                       modifiers)])
              (complete (key code modifiers kind state) (len bytes))))))))

  (define (special-key-code n)
    (cond
      [(or (= n 1) (= n 7)) 'home]
      [(= n 2) 'insert]
      [(= n 3) 'delete]
      [(or (= n 4) (= n 8)) 'end]
      [(= n 5) 'page-up]
      [(= n 6) 'page-down]
      [(and (<= 11 n) (<= n 15)) `(function ,(- n 10))]
      [(and (<= 17 n) (<= n 21)) `(function ,(- n 11))]
      [(and (<= 23 n) (<= n 26)) `(function ,(- n 12))]
      [(and (<= 28 n) (<= n 29)) `(function ,(- n 15))]
      [(and (<= 31 n) (<= n 34)) `(function ,(- n 17))]
      [else #f]))

  (define (parse-special-key bytes)
    (let* ([s (ascii-slice bytes 2 (- (len bytes) 1))]
           [parts (string-split s #\;)]
           [first (and (pair? parts) (string->nat (car parts)))]
           [code (and first (special-key-code first))])
      (if (not code)
        (invalid (len bytes))
        (call-with-values
          (lambda () (parse-modifiers-kind-state (cdr parts)))
          (lambda (modifiers kind state)
            (complete (key code modifiers kind state) (len bytes)))))))

  (define (modifier-key-code final)
    (case (integer->char final)
      [(#\A) 'up]
      [(#\B) 'down]
      [(#\C) 'right]
      [(#\D) 'left]
      [(#\F) 'end]
      [(#\H) 'home]
      [(#\P) '(function 1)]
      [(#\Q) '(function 2)]
      [(#\R) '(function 3)]
      [(#\S) '(function 4)]
      [else #f]))

  (define (parse-csi-modifier-key bytes)
    (let* ([final (u8 bytes (- (len bytes) 1))]
           [code (modifier-key-code final)])
      (if (not code)
        (invalid (len bytes))
        (let* ([s (ascii-slice bytes 2 (- (len bytes) 1))]
               [parts (string-split s #\;)])
          (call-with-values
            (lambda ()
              (if (and (pair? (cdr parts)) (parse-modifier-kind (cadr parts)))
                (parse-modifiers-kind-state (cdr parts))
                (let ([n (and (> (len bytes) 3)
                          (- (u8 bytes (- (len bytes) 2)) (char->integer #\0)))])
                  (values (if (and n (<= 0 n 9)) (parse-modifiers n) '()) 'press '()))))
            (lambda (modifiers kind state)
              (complete (key code modifiers kind state) (len bytes))))))))

  (define (parse-cursor-position bytes)
    (let* ([s (ascii-slice bytes 2 (- (len bytes) 1))]
           [parts (string-split s #\;)]
           [row (and (pair? parts) (string->nat (car parts)))]
           [column (and (pair? parts) (pair? (cdr parts)) (string->nat (cadr parts)))])
      (if (and row column (> row 0) (> column 0))
        (complete `(cursor-position ,(- column 1) ,(- row 1)) (len bytes))
        (invalid (len bytes)))))

  (define (parse-enhancement-flags bytes)
    (if (< (len bytes) 5)
      (incomplete)
      (let* ([bits (u8 bytes 3)]
             [flags (append (if (= 0 (bitwise-and bits 1)) '() '(disambiguate-escape-codes))
                     (if (= 0 (bitwise-and bits 2)) '() '(report-event-types))
                     (if (= 0 (bitwise-and bits 4)) '() '(report-alternate-keys))
                     (if (= 0 (bitwise-and bits 8)) '() '(report-all-keys-as-escape-codes)))])
        (complete `(keyboard-enhancement-flags ,flags) (len bytes)))))

  (define (byte-index bytes needle start)
    (let ([n (len bytes)] [m (bytevector-length needle)])
      (let outer ([i start])
        (cond
          [(> (+ i m) n) #f]
          [else
            (let inner ([j 0])
              (cond
                [(= j m) i]
                [(= (u8 bytes (+ i j)) (u8 needle j)) (inner (+ j 1))]
                [else (outer (+ i 1))]))]))))

  (define (parse-paste bytes)
    (let* ([end-marker '#vu8(#x1b #x5b #x32 #x30 #x31 #x7e)]
           [end (byte-index bytes end-marker 6)])
      (if end
        (complete (paste-event (utf8->string (sub-bytevector bytes 6 end)))
          (+ end (bytevector-length end-marker)))
        (incomplete))))

  (define (parse-cb cb)
    (let* ([button (bitwise-ior (bitwise-and cb #b00000011)
                    (bitwise-arithmetic-shift (bitwise-and cb #b11000000) -4))]
           [dragging? (= (bitwise-and cb #b00100000) #b00100000)]
           [kind (cond
                  [(and (= button 0) (not dragging?)) '(down left)]
                  [(and (= button 1) (not dragging?)) '(down middle)]
                  [(and (= button 2) (not dragging?)) '(down right)]
                  [(and (= button 0) dragging?) '(drag left)]
                  [(and (= button 1) dragging?) '(drag middle)]
                  [(and (= button 2) dragging?) '(drag right)]
                  [(and (= button 3) (not dragging?)) '(up left)]
                  [(and (or (= button 3) (= button 4) (= button 5)) dragging?) 'moved]
                  [(and (= button 4) (not dragging?)) 'scroll-up]
                  [(and (= button 5) (not dragging?)) 'scroll-down]
                  [(and (= button 6) (not dragging?)) 'scroll-left]
                  [(and (= button 7) (not dragging?)) 'scroll-right]
                  [else #f])]
           [modifiers (append (if (= (bitwise-and cb #b00000100) #b00000100) '(shift) '())
                       (if (= (bitwise-and cb #b00001000) #b00001000) '(alt) '())
                       (if (= (bitwise-and cb #b00010000) #b00010000) '(control) '()))])
      (and kind (cons kind modifiers))))

  (define (parse-normal-mouse bytes)
    (if (< (len bytes) 6)
      (incomplete)
      (let* ([parsed (parse-cb (- (u8 bytes 3) 32))]
             [column (- (- (u8 bytes 4) 32) 1)]
             [row (- (- (u8 bytes 5) 32) 1)])
        (if parsed
          (complete (mouse-event (car parsed) column row (cdr parsed)) 6)
          (invalid 6)))))

  (define (parse-rxvt-mouse bytes)
    (let* ([s (ascii-slice bytes 2 (- (len bytes) 1))]
           [parts (string-split s #\;)]
           [raw-cb (and (pair? parts) (string->nat (car parts)))]
           [column (and (pair? parts) (pair? (cdr parts)) (string->nat (cadr parts)))]
           [row (and (pair? parts) (pair? (cdr parts)) (pair? (cddr parts)) (string->nat (caddr parts)))]
           [parsed (and raw-cb (parse-cb (- raw-cb 32)))])
      (if (and parsed column row (> column 0) (> row 0))
        (complete (mouse-event (car parsed) (- column 1) (- row 1) (cdr parsed)) (len bytes))
        (invalid (len bytes)))))

  (define (parse-sgr-mouse bytes)
    (let ([last (u8 bytes (- (len bytes) 1))])
      (if (not (or (= last (char->integer #\M)) (= last (char->integer #\m))))
        (incomplete)
        (let* ([s (ascii-slice bytes 3 (- (len bytes) 1))]
               [parts (string-split s #\;)]
               [cb (and (pair? parts) (string->nat (car parts)))]
               [column (and (pair? parts) (pair? (cdr parts)) (string->nat (cadr parts)))]
               [row (and (pair? parts) (pair? (cdr parts)) (pair? (cddr parts)) (string->nat (caddr parts)))]
               [parsed (and cb (parse-cb cb))])
          (if (and parsed column row (> column 0) (> row 0))
            (let ([kind (if (and (= last (char->integer #\m))
                             (pair? (car parsed))
                             (eq? (caar parsed) 'down))
                         `(up ,(cadar parsed))
                         (car parsed))])
              (complete (mouse-event kind (- column 1) (- row 1) (cdr parsed)) (len bytes)))
            (invalid (len bytes)))))))

  (define (parse-utf8-char bytes)
    (let* ([first (u8 bytes 0)]
           [needed (cond
                    [(<= first #x7f) 1]
                    [(and (<= #xc0 first) (<= first #xdf)) 2]
                    [(and (<= #xe0 first) (<= first #xef)) 3]
                    [(and (<= #xf0 first) (<= first #xf7)) 4]
                    [else #f])])
      (cond
        [(not needed) (invalid 1)]
        [(< (len bytes) needed)
          (let loop ([i 1])
            (cond
              [(= i (len bytes)) (incomplete)]
              [(= (bitwise-and (u8 bytes i) #b11000000) #b10000000) (loop (+ i 1))]
              [else (invalid i)]))]
        [else
          (let loop ([i 1])
            (cond
              [(= i needed)
                (let ([s (utf8->string (sub-bytevector bytes 0 needed))])
                  (if (= (string-length s) 1)
                    (complete (char-key (string-ref s 0)) needed)
                    (invalid needed)))]
              [(= (bitwise-and (u8 bytes i) #b11000000) #b10000000) (loop (+ i 1))]
              [else (invalid (+ i 1))]))])))

  (define (parse-csi bytes raw-mode-enabled?)
    (cond
      [(= (len bytes) 2) (incomplete)]
      [(= (u8 bytes 2) (char->integer #\[))
        (if (= (len bytes) 3)
          (incomplete)
          (let ([b (u8 bytes 3)])
            (if (and (<= (char->integer #\A) b) (<= b (char->integer #\E)))
              (complete (key `(function ,(+ 1 (- b (char->integer #\A)))) '()) 4)
              (invalid 4))))]
      [(= (u8 bytes 2) (char->integer #\D)) (complete (key 'left '()) 3)]
      [(= (u8 bytes 2) (char->integer #\C)) (complete (key 'right '()) 3)]
      [(= (u8 bytes 2) (char->integer #\A)) (complete (key 'up '()) 3)]
      [(= (u8 bytes 2) (char->integer #\B)) (complete (key 'down '()) 3)]
      [(= (u8 bytes 2) (char->integer #\H)) (complete (key 'home '()) 3)]
      [(= (u8 bytes 2) (char->integer #\F)) (complete (key 'end '()) 3)]
      [(= (u8 bytes 2) (char->integer #\Z)) (complete (key 'backtab '(shift)) 3)]
      [(= (u8 bytes 2) (char->integer #\M)) (parse-normal-mouse bytes)]
      [(= (u8 bytes 2) (char->integer #\<)) (parse-sgr-mouse bytes)]
      [(= (u8 bytes 2) (char->integer #\I)) (complete (focus-gained-event) 3)]
      [(= (u8 bytes 2) (char->integer #\O)) (complete (focus-lost-event) 3)]
      [(= (u8 bytes 2) (char->integer #\;)) (parse-csi-modifier-key bytes)]
      [(= (u8 bytes 2) (char->integer #\P)) (complete (key '(function 1) '()) 3)]
      [(= (u8 bytes 2) (char->integer #\Q)) (complete (key '(function 2) '()) 3)]
      [(= (u8 bytes 2) (char->integer #\S)) (complete (key '(function 4) '()) 3)]
      [(= (u8 bytes 2) (char->integer #\?))
        (let ([last (u8 bytes (- (len bytes) 1))])
          (cond
            [(= last (char->integer #\u)) (parse-enhancement-flags bytes)]
            [(= last (char->integer #\c)) (complete '(primary-device-attributes) (len bytes))]
            [else (incomplete)]))]
      [(and (<= (char->integer #\0) (u8 bytes 2)) (<= (u8 bytes 2) (char->integer #\9)))
        (let ([last (u8 bytes (- (len bytes) 1))])
          (cond
            [(not (and (<= 64 last) (<= last 126))) (incomplete)]
            [(and (>= (len bytes) 6)
                (= (u8 bytes 2) (char->integer #\2))
                (= (u8 bytes 3) (char->integer #\0))
                (= (u8 bytes 4) (char->integer #\0))
                (= (u8 bytes 5) (char->integer #\~)))
              (parse-paste bytes)]
            [(= last (char->integer #\M)) (parse-rxvt-mouse bytes)]
            [(= last (char->integer #\~)) (parse-special-key bytes)]
            [(= last (char->integer #\u)) (parse-u-key bytes raw-mode-enabled?)]
            [(= last (char->integer #\R)) (parse-cursor-position bytes)]
            [else (parse-csi-modifier-key bytes)]))]
      [else (invalid (len bytes))]))

  (define (parse-esc-o bytes)
    (if (= (len bytes) 2)
      (incomplete)
      (let ([b (u8 bytes 2)])
        (case (integer->char b)
          [(#\D) (complete (key 'left '()) 3)]
          [(#\C) (complete (key 'right '()) 3)]
          [(#\A) (complete (key 'up '()) 3)]
          [(#\B) (complete (key 'down '()) 3)]
          [(#\H) (complete (key 'home '()) 3)]
          [(#\F) (complete (key 'end '()) 3)]
          [(#\P #\Q #\R #\S)
            (complete (key `(function ,(+ 1 (- b (char->integer #\P)))) '()) 3)]
          [else (invalid 3)]))))

  (define (parse-event bytes input-available? raw-mode-enabled?)
    (cond
      [(= (len bytes) 0) (incomplete)]
      [(= (u8 bytes 0) #x1b)
        (cond
          [(= (len bytes) 1)
            (if input-available?
              (incomplete)
              (complete (key 'esc '()) 1))]
          [(= (u8 bytes 1) (char->integer #\O)) (parse-esc-o bytes)]
          [(= (u8 bytes 1) (char->integer #\[)) (parse-csi bytes raw-mode-enabled?)]
          [(= (u8 bytes 1) #x1b) (complete (key 'esc '()) 1)]
          [else
            (call-with-values
              (lambda () (parse-event (sub-bytevector bytes 1 (len bytes)) input-available? raw-mode-enabled?))
              (lambda (status event consumed)
                (if (eq? status 'complete)
                  (complete (add-alt event) (+ consumed 1))
                  (values status event consumed))))])]
      [(= (u8 bytes 0) #x0d) (complete (key 'enter '()) 1)]
      [(and (= (u8 bytes 0) #x0a) (not raw-mode-enabled?)) (complete (key 'enter '()) 1)]
      [(= (u8 bytes 0) #x09) (complete (key 'tab '()) 1)]
      [(= (u8 bytes 0) #x7f) (complete (key 'backspace '()) 1)]
      [(and (<= #x01 (u8 bytes 0)) (<= (u8 bytes 0) #x1a))
        (complete (key `(char ,(integer->char (+ (- (u8 bytes 0) #x01) (char->integer #\a)))) '(control)) 1)]
      [(and (<= #x1c (u8 bytes 0)) (<= (u8 bytes 0) #x1f))
        (complete (key `(char ,(integer->char (+ (- (u8 bytes 0) #x1c) (char->integer #\4)))) '(control)) 1)]
      [(= (u8 bytes 0) 0) (complete (key '(char #\space) '(control)) 1)]
      [else (parse-utf8-char bytes)])))
