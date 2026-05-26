(library (capy term line)
  (export make-line-state line-state? line-state-prompt line-state-buffer
    line-state-cursor
    line-state-history
    line-state-history-index
    line-state-completer
    line-state-renderer
    line-state-continuation?
    line-state-result
    make-line-editor
    line-editor?
    line-editor-history
    line-editor-completer
    line-editor-renderer
    line-editor-continuation?
    line-completion-menu
    line-editor-add-history!
    line-edit-step
    read-line/edit)
  (import (rnrs)
    (only (capy) call-with-output-string define*)
    (capy term command)
    (capy term cursor)
    (capy term style)
    (capy term terminal)
    (capy term event))

  (define-record-type (<line-state> %make-line-state line-state?)
    (fields
      (immutable prompt line-state-prompt)
      (immutable buffer line-state-buffer)
      (immutable cursor line-state-cursor)
      (immutable history line-state-history)
      (immutable history-index line-state-history-index)
      (immutable completer line-state-completer)
      (immutable renderer line-state-renderer)
      (immutable continuation? line-state-continuation?)
      (immutable completion line-state-completion)
      (immutable result line-state-result)))

  (define* (make-line-state
            prompt
            buffer
            cursor
            history
            history-index
            completer
            #:key
            (renderer (lambda (buffer cursor) buffer))
            (continuation? (lambda (buffer cursor) #f))
            (completion #f))

    (%make-line-state prompt
      buffer
      cursor
      history
      history-index
      completer
      renderer
      continuation?
      completion
      #f))

  (define-record-type (<line-editor> %make-line-editor line-editor?)
    (fields
      (mutable history line-editor-history set-line-editor-history!)
      (mutable completer line-editor-completer set-line-editor-completer!)
      (mutable renderer line-editor-renderer set-line-editor-renderer!)
      (mutable continuation? line-editor-continuation? set-line-editor-continuation?!)))

  (define* (make-line-editor
            #:key
            (history '())
            (completer #f)
            (renderer (lambda (buffer cursor) buffer))
            (continuation? (lambda (buffer cursor) #f)))

    (%make-line-editor history completer renderer continuation?))

  (define (line-editor-add-history! editor line)
    (when (and (string? line) (> (string-length line) 0))
      (set-line-editor-history! editor (append (line-editor-history editor) (list line)))))

  (define (state-like state buffer cursor history-index result . maybe-completion)
    (%make-line-state (line-state-prompt state)
      buffer
      cursor
      (line-state-history state)
      history-index
      (line-state-completer state)
      (line-state-renderer state)
      (line-state-continuation? state)
      (if (null? maybe-completion) #f (car maybe-completion))
      result))

  (define (string-insert s index ch)
    (string-append (substring s 0 index)
      (string ch)
      (substring s index (string-length s))))

  (define (string-delete s index)
    (string-append (substring s 0 index)
      (substring s (+ index 1) (string-length s))))

  (define (key-code event)
    (and (key-event? event) (key-event-code event)))

  (define (key-modifiers event)
    (if (key-event? event) (key-event-modifiers event) '()))

  (define (control-char? event ch)
    (and (key-event? event)
      (equal? (key-event-code event) `(char ,ch))
      (memq 'control (key-event-modifiers event))))

  (define (history-ref/default history index fallback)
    (if (and (> index 0) (<= index (length history)))
      (list-ref history (- (length history) index))
      fallback))

  (define (history-up state)
    (let* ([history (line-state-history state)]
           [index (min (length history) (+ (line-state-history-index state) 1))]
           [text (history-ref/default history index (line-state-buffer state))])
      (state-like state text (string-length text) index #f)))

  (define (history-down state)
    (let* ([history (line-state-history state)]
           [index (max 0 (- (line-state-history-index state) 1))]
           [text (history-ref/default history index "")])
      (state-like state text (string-length text) index #f)))

  (define (line-start text cursor)
    (let loop ([index cursor])
      (cond
        [(zero? index) 0]
        [(char=? (string-ref text (- index 1)) #\newline) index]
        [else (loop (- index 1))])))

  (define (line-end text cursor)
    (let ([n (string-length text)])
      (let loop ([index cursor])
        (cond
          [(= index n) n]
          [(char=? (string-ref text index) #\newline) index]
          [else (loop (+ index 1))]))))

  (define (line-index text cursor)
    (let loop ([index 0] [row 0])
      (cond
        [(= index cursor) row]
        [(char=? (string-ref text index) #\newline)
          (loop (+ index 1) (+ row 1))]
        [else (loop (+ index 1) row)])))

  (define (line-column text cursor)
    (- cursor (line-start text cursor)))

  (define (multiline-buffer? text)
    (let loop ([index 0])
      (cond
        [(= index (string-length text)) #f]
        [(char=? (string-ref text index) #\newline) #t]
        [else (loop (+ index 1))])))

  (define (cursor-up-line text cursor)
    (let ([start (line-start text cursor)])
      (if (zero? start)
        cursor
        (let* ([column (line-column text cursor)]
               [previous-end (- start 1)]
               [previous-start (line-start text previous-end)]
               [previous-length (- previous-end previous-start)])
          (+ previous-start (min column previous-length))))))

  (define (cursor-down-line text cursor)
    (let* ([end (line-end text cursor)]
           [n (string-length text)])
      (if (= end n)
        cursor
        (let* ([column (line-column text cursor)]
               [next-start (+ end 1)]
               [next-end (line-end text next-start)]
               [next-length (- next-end next-start)])
          (+ next-start (min column next-length))))))

  (define (identifier-char? ch)
    (not (or (char-whitespace? ch)
          (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)))))

  (define (completion-start text cursor)
    (let ([limit (min cursor (string-length text))])
      (let loop ([index limit])
        (cond
          [(zero? index) 0]
          [(identifier-char? (string-ref text (- index 1)))
            (loop (- index 1))]
          [else index]))))

  (define (replace-range text start end replacement)
    (string-append (substring text 0 start)
      replacement
      (substring text end (string-length text))))

  (define (insert-sorted-by-shortest match matches)
    (cond
      [(null? matches) (list match)]
      [(< (string-length match) (string-length (car matches)))
        (cons match matches)]
      [else (cons (car matches)
             (insert-sorted-by-shortest match (cdr matches)))]))

  (define (sort-completions matches)
    (let loop ([matches matches] [sorted '()])
      (if (null? matches)
        sorted
        (loop (cdr matches) (insert-sorted-by-shortest (car matches) sorted)))))

  (define (styled-string text . specs)
    (call-with-output-string
      (lambda (port)
        (queue-command port (print-styled-content (apply stylize text specs))))))

  (define (line-completion-menu matches active-index)
    (call-with-output-string
      (lambda (port)
        (let loop ([matches matches] [index 0])
          (unless (null? matches)
            (when (> index 0)
              (display "  " port))
            (if (= index active-index)
              (display (styled-string (car matches) 'cyan 'bold 'reverse) port)
              (display (styled-string (car matches) 'dark-grey) port))
            (loop (cdr matches) (+ index 1)))))))

  (define (completion-apply state start matches index)
    (let* ([match (list-ref matches index)]
           [buffer (line-state-buffer state)]
           [cursor (line-state-cursor state)]
           [next-buffer (replace-range buffer start cursor match)]
           [next-cursor (+ start (string-length match))]
           [next-index (mod (+ index 1) (length matches))]
           [completion (list start matches next-index)])
      (values (state-like state next-buffer next-cursor 0 #f completion)
        (if (null? (cdr matches))
          'complete
          (list 'completions matches index)))))

  (define (complete-line state)
    (let ([completer (line-state-completer state)])
      (if (not completer)
        (values state 'continue)
        (let ([active (line-state-completion state)])
          (if active
            (completion-apply state (car active) (cadr active) (caddr active))
            (let ([matches (sort-completions
                            (completer (line-state-buffer state) (line-state-cursor state)))])
              (if (null? matches)
                (values state 'continue)
                (completion-apply state
                  (completion-start (line-state-buffer state)
                    (line-state-cursor state))
                  matches
                  0))))))))

  (define (completion-trigger-position? text cursor)
    (and (> cursor 0)
      (identifier-char? (string-ref text (- cursor 1)))
      (or (= cursor (string-length text))
        (not (identifier-char? (string-ref text cursor))))))

  (define (shift-enter? event)
    (memq 'shift (key-modifiers event)))

  (define (enter-accepts? state event)
    (let ([buffer (line-state-buffer state)]
          [cursor (line-state-cursor state)])
      (or (shift-enter? event)
        (not (multiline-buffer? buffer))
        (= cursor (string-length buffer)))))

  (define (insert-newline state buffer cursor)
    (let ([next (string-insert buffer cursor #\newline)])
      (values (state-like state next (+ cursor 1) 0 #f) 'continue)))

  (define (line-edit-step state event)
    (let* ([buffer (line-state-buffer state)]
           [cursor (line-state-cursor state)]
           [n (string-length buffer)]
           [code (key-code event)])
      (cond
        [(not (key-event? event)) (values state 'continue)]
        [(or (equal? code 'enter) (equal? code 'return))
          (if ((line-state-continuation? state) buffer cursor)
            (insert-newline state buffer cursor)
            (if (enter-accepts? state event)
              (values (state-like state buffer cursor 0 buffer) 'accept)
              (insert-newline state buffer cursor)))]
        [(control-char? event #\c)
          (values (state-like state buffer cursor 0 #f) 'abort)]
        [(and (control-char? event #\d) (= n 0))
          (values (state-like state buffer cursor 0 #f) 'eof)]
        [(control-char? event #\a)
          (values (state-like state buffer 0 0 #f) 'continue)]
        [(control-char? event #\e)
          (values (state-like state buffer n 0 #f) 'continue)]
        [(control-char? event #\u)
          (values (state-like state (substring buffer cursor n) 0 0 #f) 'continue)]
        [(control-char? event #\k)
          (values (state-like state (substring buffer 0 cursor) cursor 0 #f) 'continue)]
        [(equal? code 'left)
          (values (state-like state buffer (max 0 (- cursor 1)) 0 #f) 'continue)]
        [(equal? code 'right)
          (values (state-like state buffer (min n (+ cursor 1)) 0 #f) 'continue)]
        [(equal? code 'home)
          (values (state-like state buffer 0 0 #f) 'continue)]
        [(equal? code 'end)
          (values (state-like state buffer n 0 #f) 'continue)]
        [(equal? code 'up)
          (let ([next-cursor (cursor-up-line buffer cursor)])
            (if (= next-cursor cursor)
              (if (multiline-buffer? buffer)
                (values state 'continue)
                (values (history-up state) 'continue))
              (values (state-like state buffer next-cursor 0 #f) 'continue)))]
        [(equal? code 'down)
          (let ([next-cursor (cursor-down-line buffer cursor)])
            (if (= next-cursor cursor)
              (if (multiline-buffer? buffer)
                (values state 'continue)
                (values (history-down state) 'continue))
              (values (state-like state buffer next-cursor 0 #f) 'continue)))]
        [(equal? code 'backspace)
          (if (= cursor 0)
            (values state 'continue)
            (values (state-like state (string-delete buffer (- cursor 1)) (- cursor 1) 0 #f)
              'continue))]
        [(equal? code 'delete)
          (if (= cursor n)
            (values state 'continue)
            (values (state-like state (string-delete buffer cursor) cursor 0 #f)
              'continue))]
        [(equal? code 'tab)
          (if (and (line-state-completer state)
               (or (line-state-completion state)
                 (completion-trigger-position? buffer cursor)))
            (complete-line state)
            (let ([next (string-insert buffer cursor #\tab)])
              (values (state-like state next (+ cursor 1) 0 #f) 'continue)))]
        [(and (pair? code) (eq? (car code) 'char) (not (memq 'control (key-modifiers event))))
          (let ([next (string-insert buffer cursor (cadr code))])
            (values (state-like state next (+ cursor 1) 0 #f) 'continue))]
        [else (values state 'continue)])))

  (define (count-lines text)
    (let loop ([index 0] [lines 1])
      (cond
        [(= index (string-length text)) lines]
        [(char=? (string-ref text index) #\newline)
          (loop (+ index 1) (+ lines 1))]
        [else (loop (+ index 1) lines)])))

  (define (ansi-final-byte? ch)
    (and (char<=? #\@ ch) (char<=? ch #\~)))

  (define (visible-string-length text)
    (let ([n (string-length text)])
      (let loop ([index 0] [len 0])
        (cond
          [(= index n) len]
          [(char=? (string-ref text index) #\x1b)
            (let ([next (+ index 1)])
              (cond
                [(= next n) len]
                [(char=? (string-ref text next) #\[)
                  (let scan-csi ([j (+ next 1)])
                    (cond
                      [(= j n) len]
                      [(ansi-final-byte? (string-ref text j))
                        (loop (+ j 1) len)]
                      [else (scan-csi (+ j 1))]))]
                [(char=? (string-ref text next) #\])
                  (let scan-osc ([j (+ next 1)])
                    (cond
                      [(= j n) len]
                      [(char=? (string-ref text j) #\x07)
                        (loop (+ j 1) len)]
                      [else (scan-osc (+ j 1))]))]
                [else (loop (+ next 1) len)]))]
          [else (loop (+ index 1) (+ len 1))]))))

  (define tab-width 8)

  (define (next-tab-stop column)
    (+ column (- tab-width (mod column tab-width))))

  (define (buffer-display-column text start end base-column)
    (let loop ([index start] [column base-column])
      (cond
        [(= index end) column]
        [(char=? (string-ref text index) #\tab)
          (loop (+ index 1) (next-tab-stop column))]
        [else (loop (+ index 1) (+ column 1))])))

  (define (display-column state cursor)
    (let* ([buffer (line-state-buffer state)]
           [start (line-start buffer cursor)]
           [base (if (zero? (line-index buffer cursor))
                  (visible-string-length (line-state-prompt state))
                  0)])
      (buffer-display-column buffer start cursor base)))

  (define (move-to-render-start port state)
    (let ([row (line-index (line-state-buffer state)
                (line-state-cursor state))])
      (when (> row 0)
        (queue-command port (move-up row))))
    (display "\r" port))

  (define (clear-rendered-lines port state)
    (let ([lines (count-lines (line-state-buffer state))])
      (let loop ([index 0])
        (queue-command port (clear 'current-line))
        (when (< (+ index 1) lines)
          (queue-command port (move-down 1))
          (display "\r" port)
          (loop (+ index 1))))
      (when (> lines 1)
        (queue-command port (move-up (- lines 1)))))
    (display "\r" port))

  (define (move-to-input-cursor port state)
    (let* ([buffer (line-state-buffer state)]
           [end-row (line-index buffer (string-length buffer))]
           [cursor-row (line-index buffer (line-state-cursor state))]
           [target-column (display-column state (line-state-cursor state))])
      (when (> (- end-row cursor-row) 0)
        (queue-command port (move-up (- end-row cursor-row))))
      (queue-command port (move-to-column target-column))))

  (define (display-rendered-input port text)
    (let loop ([index 0])
      (when (< index (string-length text))
        (let ([ch (string-ref text index)])
          (when (char=? ch #\newline)
            (display "\r" port))
          (display ch port)
          (loop (+ index 1))))))

  (define (redraw-line port state . maybe-previous)
    (unless (null? maybe-previous)
      (move-to-render-start port (car maybe-previous))
      (clear-rendered-lines port (car maybe-previous)))
    (display "\r" port)
    (queue-command port (clear 'current-line))
    (display (line-state-prompt state) port)
    (display-rendered-input port
      ((line-state-renderer state)
        (line-state-buffer state)
        (line-state-cursor state)))
    (move-to-input-cursor port state)
    (flush-output-port port))

  (define (clear-transient-completion-line port)
    (queue-command port (save-position))
    (queue-command port (move-down 1))
    (display "\r" port)
    (queue-command port (clear 'current-line))
    (queue-command port (restore-position))
    (flush-output-port port))

  (define (clear-completion-line-if-visible port state)
    (when (line-state-completion state)
      (clear-transient-completion-line port)))

  (define (redraw-line/completions port state matches active-index)
    (redraw-line port state)
    (queue-command port (save-position))
    (move-to-input-end port state)
    (newline port)
    (display "\r" port)
    (queue-command port (clear 'current-line))
    (queue-command port (disable-line-wrap))
    (display (line-completion-menu matches active-index) port)
    (queue-command port (enable-line-wrap))
    (queue-command port (restore-position))
    (flush-output-port port))

  (define (move-to-input-end port state)
    (let* ([buffer (line-state-buffer state)]
           [cursor (line-state-cursor state)]
           [end (string-length buffer)])
      (if (= (line-index buffer cursor) (line-index buffer end))
        (let ([delta (- end cursor)])
          (when (> delta 0)
            (queue-command port (move-right delta))))
        (move-to-input-cursor port
          (state-like state
            buffer
            end
            (line-state-history-index state)
            (line-state-result state)
            (line-state-completion state))))))

  (define (finish-input-line port)
    (display "\r\n" port))

  (define (read-line/edit editor . args)
    (let* ([prompt (if (null? args) "> " (car args))]
           [source (if (or (null? args) (null? (cdr args))) #f (cadr args))]
           [port (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                  (current-output-port)
                  (caddr args))]
           [state (make-line-state prompt "" 0
                   (line-editor-history editor)
                   0
                   (line-editor-completer editor)
                   #:renderer
                   (line-editor-renderer editor)
                   #:continuation?
                   (line-editor-continuation? editor))]
           [manage-raw? (not source)]
           [was-raw? (and manage-raw? (raw-mode-enabled?))])
      (dynamic-wind
        (lambda ()
          (when (and manage-raw? (not was-raw?)) (enable-raw-mode!))
          (redraw-line port state))
        (lambda ()
          (let loop ([state state])
            (let ([event (if source (read-event source) (read-event))])
              (call-with-values
                (lambda () (line-edit-step state event))
                (lambda (next status)
                  (cond
                    [(eq? status 'accept)
                      (clear-completion-line-if-visible port state)
                      (move-to-input-end port next)
                      (finish-input-line port)
                      (line-editor-add-history! editor (line-state-result next))
                      (line-state-result next)]
                    [(or (eq? status 'abort) (eq? status 'eof))
                      (clear-completion-line-if-visible port state)
                      (move-to-input-end port next)
                      (finish-input-line port)
                      #f]
                    [(and (pair? status) (eq? (car status) 'completions))
                      (redraw-line/completions port next (cadr status) (caddr status))
                      (loop next)]
                    [else
                      (clear-completion-line-if-visible port state)
                      (redraw-line port next state)
                      (loop next)]))))))
        (lambda ()
          (when (and manage-raw? (not was-raw?)) (disable-raw-mode!)))))))
