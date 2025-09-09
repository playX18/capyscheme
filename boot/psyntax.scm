
(let ([syntax? (module-ref (current-module) 'syntax?)]
      [make-syntax (module-ref (current-module) 'make-syntax)]
      [syntax-expression (module-ref (current-module) 'syntax-expression)]
      [syntax-wrap (module-ref (current-module) 'syntax-wrap)]
      [syntax-module (module-ref (current-module) 'syntax-module)]
      [syntax-sourcev (module-ref (current-module) 'syntax-sourcev)])
    syntax?
)