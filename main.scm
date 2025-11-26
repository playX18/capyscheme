(import (capy compiler tree-il)
        (capy pretty-print))


(define code 
'(begin 
  (define-syntax my-when 
    (syntax-rules () 
      [(my-when test body ...) 
        (if test 
          (begin body ...) 
          (unspecified))]))
  (my-when (> 3 2) 
    (display "3 is greater than 2") 
    (newline))))

(pretty-print (tree-il->scheme (expand code)))