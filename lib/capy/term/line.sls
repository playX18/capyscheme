(library (capy term line)
  (export make-line-state line-state? line-state-prompt line-state-buffer
    line-state-cursor
    line-state-history
    line-state-history-index
    line-state-completer
    line-state-renderer
    line-state-result
    make-line-editor
    line-editor?
    line-editor-history
    line-editor-completer
    line-editor-renderer
    line-completion-menu
    line-editor-add-history!
    line-edit-step
    read-line/edit)
  (import (rnrs)
    (only (capy) keyword->symbol call-with-output-string)
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
      (immutable completion line-state-completion)
      (immutable result line-state-result)))

  (define (make-line-state prompt buffer cursor history history-index completer . maybe-result)
    (let loop ([args maybe-result]
               [result #f]
               [renderer (lambda (buffer cursor) buffer)]
               [completion #f])
      (cond
        [(null? args)
          (%make-line-state prompt
            buffer
            cursor
            history
            history-index
            completer
            renderer
            completion
            result)]
        [(eq? (car args) #:renderer)
          (if (null? (cdr args))
            (assertion-violation 'make-line-state "keyword missing value" (car args))
            (loop (cddr args) result (cadr args) completion))]
        [(eq? (car args) #:completion)
          (if (null? (cdr args))
            (assertion-violation 'make-line-state "keyword missing value" (car args))
            (loop (cddr args) result renderer (cadr args)))]
        [else (loop (cdr args) (car args) renderer completion)])))

  (define-record-type (<line-editor> %make-line-editor line-editor?)
    (fields
      (mutable history line-editor-history set-line-editor-history!)
      (mutable completer line-editor-completer set-line-editor-completer!)
      (mutable renderer line-editor-renderer set-line-editor-renderer!)))

  (define (make-line-editor . args)
    (let loop ([args args] [history '()] [completer #f] [renderer (lambda (buffer cursor) buffer)])
      (cond
        [(null? args) (%make-line-editor history completer renderer)]
        [(null? (cdr args))
          (assertion-violation 'make-line-editor "keyword missing value" (car args))]
        [else
          (case (keyword->symbol (car args))
            [(history) (loop (cddr args) (cadr args) completer renderer)]
            [(completer) (loop (cddr args) history (cadr args) renderer)]
            [(renderer) (loop (cddr args) history completer (cadr args))]
            [else (assertion-violation 'make-line-editor "unknown keyword" (car args))])])))

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

  (define (line-edit-step state event)
    (let* ([buffer (line-state-buffer state)]
           [cursor (line-state-cursor state)]
           [n (string-length buffer)]
           [code (key-code event)])
      (cond
        [(not (key-event? event)) (values state 'continue)]
        [(or (equal? code 'enter) (equal? code 'return))
          (values (state-like state buffer cursor 0 buffer) 'accept)]
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
          (values (history-up state) 'continue)]
        [(equal? code 'down)
          (values (history-down state) 'continue)]
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
          (complete-line state)]
        [(and (pair? code) (eq? (car code) 'char) (not (memq 'control (key-modifiers event))))
          (let ([next (string-insert buffer cursor (cadr code))])
            (values (state-like state next (+ cursor 1) 0 #f) 'continue))]
        [else (values state 'continue)])))

  (define (redraw-line port state)
    (display "\r" port)
    (queue-command port (clear 'current-line))
    (display (line-state-prompt state) port)
    (display ((line-state-renderer state)
              (line-state-buffer state)
              (line-state-cursor state))
      port)
    (let ([delta (- (string-length (line-state-buffer state))
                  (line-state-cursor state))])
      (when (> delta 0)
        (queue-command port (move-left delta))))
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
    (queue-command port (move-down 1))
    (display "\r" port)
    (queue-command port (clear 'current-line))
    (display (line-completion-menu matches active-index) port)
    (queue-command port (restore-position))
    (flush-output-port port))

  (define (move-to-input-end port state)
    (let ([delta (- (string-length (line-state-buffer state))
                    (line-state-cursor state))])
      (when (> delta 0)
        (queue-command port (move-right delta)))))

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
                   #f
                   #:renderer
                   (line-editor-renderer editor))]
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
                      (newline port)
                      (line-editor-add-history! editor (line-state-result next))
                      (line-state-result next)]
                    [(or (eq? status 'abort) (eq? status 'eof))
                      (clear-completion-line-if-visible port state)
                      (move-to-input-end port next)
                      (newline port)
                      #f]
                    [(and (pair? status) (eq? (car status) 'completions))
                      (redraw-line/completions port next (cadr status) (caddr status))
                      (loop next)]
                    [else
                      (clear-completion-line-if-visible port state)
                      (redraw-line port next)
                      (loop next)]))))))
        (lambda ()
          (when (and manage-raw? (not was-raw?)) (disable-raw-mode!)))))))
