;; A command-runner API for the args library

(define-library (capy args runner)
  (import (scheme base)
          (srfi 1)
          (capy args grammar)
          (capy args parser)
          (capy args results)
          (capy args help optional))
  (cond-expand
    ((library (srfi 130))
      (import (srfi 130)))
    (else
      (import (only (capy args string)
                    string-join
                    string-pad-right))))
  (export
    make-command-runner
    command-runner-add-command!
    command-runner?
    command-runner-executable-name
    command-runner-description
    command-runner-commands
    command-runner-grammar
    command-runner-usage
    command-runner-parse
    command-runner-run-command
    command-runner-run

    command
    command?
    command-name
    command-parent
    command-runner
    command-global-results
    command-results
    command-grammar
    command-subcommands
    command-aliases
    command-hidden?

    command-description
    command-run
    command-summary
    command-invocation
    command-category
    command-add-subcommand!)
  (begin
    (define-record-type <command-runner>
      (%command-runner executable-name
                       description
                       commands
                       grammar)
      command-runner?
      (executable-name command-runner-executable-name)
      (description command-runner-description)
      (commands command-runner-commands command-runner-commands-set!)
      (grammar command-runner-grammar command-runner-grammar-set!))

    (define (make-command-runner executable-name description)
      (define runner (%command-runner executable-name
                       description
                       '() ; commands
                       (make-grammar)))
      (grammar-add-flag! (command-runner-grammar runner) "help"
        'abbr: "h"
        'help: "Display help information for the command."
        'hide-negated-usage?: #t)
      (command-runner-add-command! runner help-command)
      runner)


    (define (command-runner-usage runner)
      (usage-without-description runner))

    (define (usage-without-description runner)
      (define usage-prefix "Usage: ")
      (define buffer (open-output-string))
      (write-string (string-append usage-prefix
                                  (command-runner-invocation runner)) buffer)
      (write-string "\n" buffer)
      (write-string "Global options:\n" buffer)
      (write-string (grammar-usage (command-runner-grammar runner)) buffer)
      (write-string "\n" buffer)
      (write-string (get-command-usage (command-runner-commands runner) #f #f) buffer)
      (newline buffer)
      (write-string (string-append "Run '" (command-runner-executable-name runner) " help <command>' for details on a specific command.") buffer)
      (get-output-string buffer))

    (define (sort-command-names names)
      (define (insert name sorted)
        (cond
          ((null? sorted) (list name))
          ((string<? name (car sorted)) (cons name sorted))
          (else (cons (car sorted) (insert name (cdr sorted))))))
      (let loop ((names names)
                 (sorted '()))
        (if (null? names)
          sorted
          (loop (cdr names) (insert (car names) sorted)))))

    (define (get-command-usage commands subcommand? default-command)
      (define names (map car (filter
        (lambda (entry)
          (define name (car entry))
          (define cmd (cdr entry))
          ;; exclude aliases:
          (not (member name (command-aliases cmd))))
          commands)))

      (define visible (filter (lambda (name)
        (define cmd (cdr (assoc name commands)))
        (not (command-hidden? cmd))) names))

      (define sorted-names (if (null? visible)
        (sort-command-names names)
        (sort-command-names visible)))

      (let loop ((categories '()) (names sorted-names))
        (cond
          ((null? names)
            (let* ((buffer (open-output-string))
                   (length (fold (lambda (name acc)
                                  (max acc (string-length name)))
                              0 sorted-names))
                   (column-start (+ length 5)))
              (write-string (string-append "Available " (if subcommand? "sub" "") "commands:") buffer)
              (for-each
                (lambda (entry)
                  (define category (car entry))
                  (define cmds (cdr entry))
                  (when (not (zero? (string-length category)))
                    (newline buffer)
                    (newline buffer)
                    (write-string category buffer))
                  (for-each
                    (lambda (cmd)
                      (define default-marker (if (equal? (command-name cmd) default-command) "(default)" ""))
                      (newline buffer)
                      (write-string
                        (string-append
                          "  "
                         (string-pad-right
                           (command-name cmd)
                           length)
                        "   "
                        default-marker)
                        buffer)
                      (write-string (command-summary cmd) buffer))
                    cmds))
                categories )
              (get-output-string buffer)))
          (else
            (let* ((name (car names))
                   (names (cdr names))
                   (cmd (cdr (assoc name commands)))
                   (category (command-category cmd)))
              (let ((existing (assoc category categories)))
                (if existing
                  (begin
                    (set-cdr! existing (cons cmd (cdr existing)))
                    (loop categories names))
                  (loop (cons (cons category (list cmd)) categories) names))))))))

    (define (command-runner-invocation runner)
      (define default (grammar-default-command (command-runner-grammar runner)))
      (cond
        (default
          (string-append (command-runner-executable-name runner) " [" default "] [arguments]"))
        (else
          (string-append (command-runner-executable-name runner) " <command> [arguments]"))))

    (define (command-set-runner-recursively! cmd runner)
      (command-runner-set! cmd runner)
      (for-each
        (lambda (entry)
          (command-set-runner-recursively! (cdr entry) runner))
        (command-subcommands cmd)))


    (define (command-runner-add-command! runenr cmd . rest)
      (let-optionals rest ((default? #f))
        (when (and default? (not (null? (command-subcommands cmd))))
          (error "default command must be a leaf command."))
        (when (and default? (grammar-default-command (command-runner-grammar runenr)))
          (error "runner already has a default command."))

        (let loop ((names (append (list (command-name cmd))
                              (command-aliases cmd)))
                   (cmds (command-runner-commands runenr)))
          (cond
            ((null? names)
              (command-runner-commands-set! runenr cmds))
            (else
              (let ((name (car names))
                    (next (cdr names)))
                (grammar-add-command! (command-runner-grammar runenr) name (command-grammar cmd))
                (loop next (cons (cons name cmd) cmds))))))
        (when default?
          (grammar-default-command-set!
            (command-runner-grammar runenr)
            (command-name cmd)))
        (command-set-runner-recursively! cmd runenr)))

    (define (command-runner-parse runner args)
      (grammar-parse (command-runner-grammar runner) args))
    (define (command-runner-run runner args)
      (command-runner-run-command runner (command-runner-parse runner args)))



    (define (command-runner-run-command runner top-results)
      (define top-flags (argument-results-flags top-results))
      (call/cc
        (lambda (exit)
          (let loop ((results top-results)
                    (commands (command-runner-commands runner))
                    (command #f)
                    (command-str (command-runner-executable-name runner)))
            (cond
              ((not (null? commands))
                (cond
                  ((not (argument-results-command results))
                    (cond
                      ((null? (argument-results-rest results))
                        (cond
                          ((not command)
                            (command-runner-print-usage runner)
                            (exit #f))
                          (else
                            (error (string-append "No subcommand provided for command '" command-str "'.")))))
                      (else
                        (let ((requested (car (argument-results-rest results))))
                          (if command
                            (error (string-append "Could not find a subcommand named '" requested "' for command '" command-str "'"))
                            (error (string-append "Missing subcommand for '" command-str "'.")))))))
                  (else
                    (let* ((results (argument-results-command results))
                           (command (cdr (assoc (argument-results-name results) commands)))
                           (commands (command-subcommands command))
                           (command-str (string-append command-str " " (argument-results-name results))))

                        (command-results-set! command results)
                        (command-global-results-set! command top-results)

                        (when (and (argument-results-has-option? results "help")
                                   ((argument-results-flags results) "help"))
                          (command-print-usage command)
                          (exit #f))
                        (loop results commands command command-str)
                        ))))
              (else
                (when ((argument-results-flags top-results) "help")
                  (command-print-usage command)
                  (exit #f))
                (command-run command)))))))

    (define (command-runner-print-usage runner)
      (define usage-no-desc (command-runner-usage runner))
      (write-string (string-append (command-runner-description runner) "\n\n" usage-no-desc) (current-output-port))
      (flush-output-port (current-output-port)))

    ;; Commands: Simple OOP based API for defining commands and running them



    (define-record-type <command>
      (%command
        name

        parent
        runner
        global-results
        results
        grammar
        subcommands

        wrapper
        aliases
        hidden?)
      command?
      (name command-name)
      (parent command-parent command-parent-set!)
      (runner command-runner command-runner-set!)
      (global-results command-global-results command-global-results-set!)
      (results command-results command-results-set!)
      (grammar command-grammar command-grammar-set!)
      (subcommands command-subcommands command-subcommands-set!)
      (wrapper command-wrapper command-wrapper-set!)
      (aliases command-aliases)
      (hidden? command-hidden?))

    (define (command-default-category cmd)
      "")
    (define (command-default-summary cmd)
      (command-description cmd))

    (define (command-parents cmd)
      (let loop ((parents '())
                 (parent (command-parent cmd)))
        (cond
          ((not parent) (reverse parents))
          (else
            (loop (cons parent parents)
                  (command-parent parent))))))

    (define (command-default-invocation cmd)
      (define parents
        (append
          (list (command-name cmd))
          (map command-name (command-parents cmd))
          (list (command-runner-executable-name
                  (command-runner cmd)))))
      (define invocation (string-join (reverse parents) " "))
      (define grammar (command-grammar cmd))

      (cond
        ((grammar-default-command grammar)
          (string-append invocation " " "[<subcommand>] [arguments]"))
        ((not (null? (command-subcommands cmd)))
          (string-append invocation " " "<subcommand> [arguments]"))
        (else
          (string-append invocation " [arguments]"))))

    (define (command-default-run cmd)
      (error (string-append "Leaf command '" (command-name cmd) "' must override 'run: in `command` definition.")))

    (define (command-default-description cmd)
      "")

    (define (command-description cmd)
      (define handler ((command-wrapper cmd) 'description))
      (handler cmd))

    (define (command-run cmd)
      (define handler ((command-wrapper cmd) 'run))
      (handler cmd))

    (define (command-summary cmd)
      (define handler ((command-wrapper cmd) 'summary))
      (handler cmd))

    (define (command-invocation cmd)
      (define handler ((command-wrapper cmd) 'invocation))
      (handler cmd))

    (define (command-category cmd)
      (define handler ((command-wrapper cmd) 'category))
      (handler cmd))

    (define (command-usage-without-description cmd)

      (let* ((usage-prefix "Usage: ")
             (buffer (open-output-string)))
        (write-string usage-prefix buffer)
        (write-string (command-invocation cmd) buffer)
        (newline buffer)
        (write-string (grammar-usage (command-grammar cmd)) buffer)
        (newline buffer)
        (unless (null? (command-subcommands cmd))
          (write-string (get-command-usage (command-subcommands cmd)
                                           (not (null? (command-subcommands cmd)))
                                           (grammar-default-command (command-grammar cmd))) buffer)
          (newline buffer))
        (newline buffer)
        (write-string (string-append "Run '" (command-runner-executable-name (command-runner cmd)) " help' to see global options.") buffer)
        (get-output-string buffer)) )

    (define (command-print-usage cmd)
      (write-string (command-description cmd) (current-output-port))
      (write-string "\n\n" (current-output-port))
      (write-string (command-usage-without-description cmd) (current-output-port))
      (flush-output-port (current-output-port)))

      ;(define usage-no-desc (command-usage-without-description cmd))
      ;(write-string (string-append (command-description cmd) "\n\n" usage-no-desc) (current-output-port))
      ;(flush-output-port (current-output-port)))

    (define (command name . rest)
      (let-keywords* rest
        ((category command-default-category)
         (summary command-default-summary)
         (invocation command-default-invocation)
         (description command-default-description)
         (run command-default-run)
         (grammar (make-grammar)) ; user can provide pre-built grammar
         (aliases '())
         (hidden? #f))
        (let ((wrapper (lambda (method)
          (case method
            ((category) category)
            ((summary) summary)
            ((invocation) invocation)
            ((description) (if (string? description)
                              (lambda args description)
                              description))
            ((run) run)))))
        (when (not (grammar-allow-anything? grammar))

          (grammar-add-flag! grammar "help"
            'abbr: "h"
            'help: "Display help information for this command."))
        (%command name
                  #f ; parent
                  #f ; runner
                  #f ; global-results
                  #f ; results
                  grammar
                  '() ; subcommands
                  wrapper
                  aliases
                  hidden?))))

    (define (command-add-subcommand! cmd subcommand . rest)
      (let-optionals rest ((default? #f))
        (when (and default? (not (null? (command-subcommands cmd))))
          (error "default command must be a leaf command."))
        (when (and default? (grammar-default-command (command-grammar cmd)))
          (error "command already has a default command."))

        (let loop ((names (append (list (command-name subcommand))
                              (command-aliases subcommand)))
                   (subs (command-subcommands cmd)))
          (cond
            ((null? names)
              (command-subcommands-set! cmd subs))
            (else
              (let ((name (car names))
                    (next (cdr names)))
                (grammar-add-command! (command-grammar cmd) name (command-grammar subcommand))
                (loop next (cons (cons name subcommand) subs))))))
        (when default?
          (grammar-default-command-set!
            (command-grammar cmd)
            (command-name subcommand)))
        (command-parent-set! subcommand cmd)
        (when (command-runner cmd)
          (command-set-runner-recursively! subcommand (command-runner cmd)))))


    ;; pre-built help command

    (define (help-description cmd)
      (define exe-name (command-runner-executable-name (command-runner cmd)))
      (string-append "Display help information for '" exe-name "'."))

    (define (help-invocation cmd)
      (define exe-name (command-runner-executable-name (command-runner cmd)))
      (string-append exe-name " help [<command>]"))

    (define (help-run cmd)
      (define rest (argument-results-rest (command-results cmd)))
      (cond
        ((null? rest)
          (command-runner-print-usage (command-runner cmd)))
        (else
          (let loop ((names rest)
                     (commands (command-runner-commands (command-runner cmd)))
                     (command-str (command-runner-executable-name (command-runner cmd))))
            (let* ((name (car names))
                   (entry (assoc name commands)))
              (unless entry
                (error (string-append "Could not find a command named '" name "' for '" command-str "'.")))
              (let ((target (cdr entry)))
                (cond
                  ((null? (cdr names))
                    (command-print-usage target))
                  (else
                    (loop (cdr names)
                          (command-subcommands target)
                          (string-append command-str " " name))))))))))

    (define help-command (command "help"
      'description: help-description
      'hidden?: #t
      'invocation: help-invocation
      'run: help-run
    ))))
