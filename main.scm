(import (capy keywords) (capy pretty-print) (capy compiler tree-il))

(pretty-print (tree-il->scheme (expand '(define (foo [x 0]) x))))

(foo 0)