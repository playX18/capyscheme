(library (core fancy-repl)
  (export read-eval-print-loop
    fancy-repl-prompt
    fancy-repl-completion-prefix
    fancy-repl-completions
    fancy-repl-syntax-spans
    fancy-repl-matching-brackets
    fancy-repl-reader-diagnostic
    fancy-repl-incomplete-input?
    fancy-repl-render-input
    fancy-repl-value-class)

  (import (capy)
    (capy session)
    (capy term line)
    (capy term style)
    (capy term command)
    (core control)
    (core exceptions)
    (core parameters)
    (core primitives)
    (core repl)
    (rnrs)
    (srfi 8))

  (define (module-name->string name)
    (let loop ([name name] [out ""])
      (cond
        [(null? name) out]
        [(string=? out "")
          (loop (cdr name) (symbol->string (car name)))]
        [else
          (loop (cdr name)
            (string-append out " " (symbol->string (car name))))])))

  (define (styled-string text . specs)
    (call-with-output-string
      (lambda (port)
        (queue-command port (print-styled-content (apply stylize text specs))))))

  (define (fancy-repl-prompt)
    (string-append "(" (module-name->string (module-name (current-module))) ")> "))

  (define (identifier-char? ch)
    (not (or (char-whitespace? ch)
          (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)))))

  (define (fancy-repl-completion-prefix text cursor)
    (let ([limit (min cursor (string-length text))])
      (let loop ([index limit])
        (cond
          [(zero? index) (substring text 0 limit)]
          [(identifier-char? (string-ref text (- index 1)))
            (loop (- index 1))]
          [else (substring text index limit)]))))

  (define (regexp-quote text)
    (let loop ([index 0] [pieces '()])
      (if (= index (string-length text))
        (apply string-append (reverse pieces))
        (let* ([ch (string-ref text index)]
               [piece (if (memv ch '(#\\ #\. #\+ #\* #\? #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\|))
                       (string-append "\\" (string ch))
                       (string ch))])
          (loop (+ index 1) (cons piece pieces))))))

  (define (fancy-repl-completions text cursor)
    (let ([prefix (fancy-repl-completion-prefix text cursor)])
      (if (zero? (string-length prefix))
        '()
        (find-bindings (string-append "^" (regexp-quote prefix))
          (current-module)))))

  (define (completion-strings text cursor)
    (map symbol->string (fancy-repl-completions text cursor)))

  (define (delimiter? ch)
    (memv ch '(#\( #\) #\[ #\] #\{ #\})))

  (define (open-delimiter? ch)
    (memv ch '(#\( #\[ #\{)))

  (define (close-delimiter? ch)
    (memv ch '(#\) #\] #\})))

  (define (matching-open ch)
    (case ch
      [(#\)) #\(]
      [(#\]) #\[]
      [(#\}) #\{]
      [else #f]))

  (define (matching-close ch)
    (case ch
      [(#\() #\)]
      [(#\[) #\]]
      [(#\{) #\}]
      [else #f]))

  (define (token-class token)
    (cond
      [(or (string=? token "#t") (string=? token "#f")) 'boolean]
      [(string->number token) 'number]
      [else 'identifier]))

  (define (fancy-repl-syntax-spans text)
    (let ([n (string-length text)])
      (let loop ([index 0] [spans '()])
        (cond
          [(= index n) (reverse spans)]
          [(char-whitespace? (string-ref text index))
            (loop (+ index 1) spans)]
          [(char=? (string-ref text index) #\;)
            (loop n (cons (list 'comment index n) spans))]
          [(delimiter? (string-ref text index))
            (loop (+ index 1) (cons (list 'paren index (+ index 1)) spans))]
          [(char=? (string-ref text index) #\")
            (let scan ([j (+ index 1)] [escaped? #f])
              (cond
                [(= j n)
                  (loop n (cons (list 'string index n) spans))]
                [escaped?
                  (scan (+ j 1) #f)]
                [(char=? (string-ref text j) #\\)
                  (scan (+ j 1) #t)]
                [(char=? (string-ref text j) #\")
                  (loop (+ j 1) (cons (list 'string index (+ j 1)) spans))]
                [else (scan (+ j 1) #f)]))]
          [else
            (let scan ([j index])
              (if (or (= j n)
                   (char-whitespace? (string-ref text j))
                   (delimiter? (string-ref text j))
                   (char=? (string-ref text j) #\")
                   (char=? (string-ref text j) #\;))
                (loop j (cons (list (token-class (substring text index j)) index j) spans))
                (scan (+ j 1))))]))))

  (define (span-class-at spans index)
    (let loop ([spans spans])
      (cond
        [(null? spans) #f]
        [(and (<= (cadar spans) index) (< index (caddar spans)))
          (caar spans)]
        [else (loop (cdr spans))])))

  (define (syntax-ignored? spans index)
    (let ([class (span-class-at spans index)])
      (or (eq? class 'string) (eq? class 'comment))))

  (define (scan-matching-brackets text)
    (let ([spans (fancy-repl-syntax-spans text)]
          [n (string-length text)])
      (let loop ([index 0] [stack '()] [matches '()] [errors '()])
        (if (= index n)
          (values (reverse matches) (reverse errors) stack)
          (let ([ch (string-ref text index)])
            (cond
              [(syntax-ignored? spans index)
                (loop (+ index 1) stack matches errors)]
              [(open-delimiter? ch)
                (loop (+ index 1) (cons (cons ch index) stack) matches errors)]
              [(close-delimiter? ch)
                (if (and (pair? stack) (char=? (caar stack) (matching-open ch)))
                  (loop (+ index 1)
                    (cdr stack)
                    (cons (list (cdar stack) index) matches)
                    errors)
                  (loop (+ index 1)
                    stack
                    matches
                    (cons (list 'error index (+ index 1) "unmatched closing delimiter")
                      errors)))]
              [else (loop (+ index 1) stack matches errors)]))))))

  (define (fancy-repl-matching-brackets text cursor)
    (let ([target (cond
                   [(and (< cursor (string-length text))
                       (delimiter? (string-ref text cursor)))
                     cursor]
                   [(and (> cursor 0)
                       (delimiter? (string-ref text (- cursor 1))))
                     (- cursor 1)]
                   [else #f])])
      (if (not target)
        #f
        (call-with-values
          (lambda () (scan-matching-brackets text))
          (lambda (matches errors stack)
            (let loop ([matches matches])
              (cond
                [(null? matches) #f]
                [(or (= target (car (car matches)))
                    (= target (cadr (car matches))))
                  (car matches)]
                [else (loop (cdr matches))])))))))

  (define (find-unterminated-string text)
    (let ([n (string-length text)])
      (let loop ([index 0])
        (cond
          [(= index n) #f]
          [(char=? (string-ref text index) #\")
            (let scan ([j (+ index 1)] [escaped? #f])
              (cond
                [(= j n) (list 'error index n "unterminated string")]
                [escaped? (scan (+ j 1) #f)]
                [(char=? (string-ref text j) #\\) (scan (+ j 1) #t)]
                [(char=? (string-ref text j) #\") (loop (+ j 1))]
                [else (scan (+ j 1) #f)]))]
          [else (loop (+ index 1))]))))

  (define (fancy-repl-reader-diagnostic text cursor)
    (or (find-unterminated-string text)
      (call-with-values
        (lambda () (scan-matching-brackets text))
        (lambda (matches errors stack)
          (cond
            [(pair? errors) (car errors)]
            [(pair? stack)
              (let ([open (car stack)])
                (list 'error (cdr open) (string-length text) "unclosed opening delimiter"))]
            [else #f])))))

  (define (fancy-repl-incomplete-input? text)
    (call-with-values
      (lambda () (scan-matching-brackets text))
      (lambda (matches errors stack)
        (and (null? errors) (pair? stack)))))

  (define (style-for-class class)
    (case class
      [(paren) '(red)]
      [(string) '(green)]
      [(number) '(green)]
      [(boolean) '(green)]
      [(comment) '(dark-grey)]
      [(identifier) '(blue)]
      [else '(blue)]))

  (define delimiter-colors '(red yellow magenta cyan))

  (define (delimiter-color depth)
    (list-ref delimiter-colors (mod depth (length delimiter-colors))))

  (define (index-in-range? index start end)
    (and (<= start index) (< index end)))

  (define (index-in-diagnostic? diagnostic index)
    (and diagnostic
      (index-in-range? index (cadr diagnostic) (caddr diagnostic))))

  (define (index-in-match? match index)
    (and match
      (or (= index (car match)) (= index (cadr match)))))

  (define (fancy-repl-render-input text cursor)
    (let* ([spans (fancy-repl-syntax-spans text)]
           [match (fancy-repl-matching-brackets text cursor)]
           [diagnostic (fancy-repl-reader-diagnostic text cursor)])
      (call-with-output-string
        (lambda (port)
          (let loop ([index 0] [stack '()])
            (when (< index (string-length text))
              (let* ([ch (string-ref text index)]
                     [base (or (span-class-at spans index) 'identifier)]
                     [tracked-delimiter
                       (and (delimiter? ch)
                         (not (eq? base 'string))
                         (not (eq? base 'comment)))]
                     [next-stack
                       (cond
                         [(and tracked-delimiter (open-delimiter? ch))
                           (cons (cons ch (delimiter-color (length stack))) stack)]
                         [(and tracked-delimiter
                             (close-delimiter? ch)
                             (pair? stack)
                             (char=? (caar stack) (matching-open ch)))
                           (cdr stack)]
                         [else stack])]
                     [delimiter-specs
                       (cond
                         [(and tracked-delimiter (open-delimiter? ch))
                           (list (delimiter-color (length stack)))]
                         [(and tracked-delimiter
                             (close-delimiter? ch)
                             (pair? stack)
                             (char=? (caar stack) (matching-open ch)))
                           (list (cdar stack))]
                         [(and tracked-delimiter (close-delimiter? ch))
                           (list (delimiter-color (length stack)))]
                         [else #f])]
                     [specs (cond
                             [(index-in-diagnostic? diagnostic index)
                               '(red underlined)]
                             [delimiter-specs delimiter-specs]
                             [(index-in-match? match index)
                               (style-for-class base)]
                             [else (style-for-class base)])])
                (display (apply styled-string (string ch) specs) port)
                (loop (+ index 1) next-stack))))))))

  (define (fancy-repl-value-class value)
    (cond
      [(procedure? value) 'procedure]
      [(number? value) 'number]
      [(string? value) 'string]
      [(boolean? value) 'boolean]
      [(symbol? value) 'symbol]
      [else 'value]))

  (define (value-style class)
    (case class
      [(procedure) 'cyan]
      [(number) 'yellow]
      [(string) 'green]
      [(boolean) 'magenta]
      [(symbol) 'blue]
      [else 'white]))

  (define (print-one-value value)
    (let* ([class (fancy-repl-value-class value)]
           [color (value-style class)])
      (display (styled-string "=> " 'dark-grey))
      (display (styled-string (format #f "~a" value) color))
      (newline)))

  (define (print-values vals)
    (cond
      [(null? vals) (unspecified)]
      [(null? (cdr vals))
        (unless (eq? (car vals) (unspecified))
          (print-one-value (car vals)))]
      [else
        (let loop ([vals vals] [index 0])
          (unless (null? vals)
            (display (styled-string (format #f "[~a] = " index) 'dark-grey))
            (display (styled-string (format #f "~a" (car vals))
                      (value-style (fancy-repl-value-class (car vals)))))
            (newline)
            (loop (cdr vals) (+ index 1))))])
    (flush-output-port (current-output-port)))

  (define (print-completions matches)
    (unless (null? matches)
      (newline)
      (for-each
        (lambda (match)
          (display (styled-string match 'cyan))
          (display " "))
        matches)
      (newline)))

  (define (print-startup-banner)
    (display (styled-string "   ____                  " 'bold 'cyan))
    (newline)
    (display (styled-string "  / ___|__ _ _ __  _   _ " 'bold 'cyan))
    (newline)
    (display (styled-string " | |   / _` | '_ \\| | | |" 'bold 'cyan))
    (newline)
    (display (styled-string " | |__| (_| | |_) | |_| |" 'bold 'cyan))
    (newline)
    (display (styled-string "  \\____\\__,_| .__/ \\__, |" 'bold 'cyan))
    (newline)
    (display (styled-string "            |_|    |___/ " 'bold 'cyan))
    (newline)
    (display (styled-string
               (string-append "v" (implementation-version) " | R6RS/R7RS Scheme")
               'bold 'green))
    (newline)
    (newline)
    (flush-output-port (current-output-port)))

  (define (read-eval-print-loop)
    (define editor
      (make-line-editor
        #:completer
        (lambda (text cursor)
          (completion-strings text cursor))
        #:renderer
        fancy-repl-render-input
        #:continuation?
        (lambda (text cursor)
          (fancy-repl-incomplete-input? text))))
    (print-startup-banner)
    (let loop ()
      (guard (c
              [else
                (flush-output-port (current-output-port))
                ((current-exception-printer) c)])
        (let ([line (read-line/edit editor
                     (styled-string (fancy-repl-prompt) 'bold 'green))])
          (cond
            [(not line)
              (format #t "Goodbye!~%")
              (flush-output-port (current-output-port))
              (exit 0)]
            [else
              (let ([form (call-with-input-string line read)])
                (unless (eof-object? form)
                  (receive ans (eval form (current-module))
                    (print-values ans))))])))
      (loop))))
