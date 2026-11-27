(library (capy term command)
  (export make-command command? command-name command->string
    queue-command
    queue-commands
    execute-command
    execute-commands
    begin-synchronized-update
    end-synchronized-update
    with-synchronized-update)
  (import (rnrs) (capy term private ansi))

  (define-record-type (<command> %make-command command?)
    (fields
      (immutable name command-name)
      (immutable renderer command-renderer)))

  (define (make-command name renderer)
    (%make-command
      name
      (cond
        [(procedure? renderer) renderer]
        [(string? renderer) (lambda () renderer)]
        [else (assertion-violation 'make-command
               "expected command renderer procedure or string"
               renderer)])))

  (define (command->string cmd)
    (unless (command? cmd)
      (assertion-violation 'command->string "expected terminal command" cmd))
    ((command-renderer cmd)))

  (define (queue-command port cmd)
    (display (command->string cmd) port)
    port)

  (define (queue-commands port cmds)
    (for-each (lambda (cmd) (queue-command port cmd)) cmds)
    port)

  (define (execute-command port cmd)
    (queue-command port cmd)
    (flush-output-port port)
    port)

  (define (execute-commands port cmds)
    (queue-commands port cmds)
    (flush-output-port port)
    port)

  (define (begin-synchronized-update)
    (make-command 'begin-synchronized-update (lambda () (csi "?2026h"))))

  (define (end-synchronized-update)
    (make-command 'end-synchronized-update (lambda () (csi "?2026l"))))

  (define (with-synchronized-update port thunk)
    (dynamic-wind
      (lambda () (execute-command port (begin-synchronized-update)))
      thunk
      (lambda () (execute-command port (end-synchronized-update))))))
