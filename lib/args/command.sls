(library (args command)
  (export)
  (import (rnrs) (args argparser) (args results))
  

  (define-record-type 
    (command-runner %make-command-runner command-runner?)
    (fields 
      executable-name
      description
      commands
      arg-parser ;; global options parser
    ))
  
  (define (add-command runner command)
    (define names (append (list (command-name command)) (command-aliases command)))
    (define parser (command-runner-arg-parser runner))
    (for-each
      (lambda (name)
        (argparser-add-command parser name command (command-parser command)))
      names)
    (command-runner-set! command runner))
  
  (define-record-type 
    (command %make-command command?)
    (fields 
      (mutable name)
      (mutable description)
      (mutable summary)
      (mutable category)
      (mutable get-invocation)
      (mutable parent)
      (mutable runner)
      (mutable global-results)
      (mutable arg-results)
      (mutable parser)
      (mutable usage)
      (mutable subcommands)
      (mutable hidden?)
      (mutable takes-arguments?)
      (mutable aliases)
      (mutable suggestion-aliases)
    )  
  )
)