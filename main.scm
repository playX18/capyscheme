(import (capy expeditor) (capy))

(dynamic-wind 
    (lambda () (raw-mode))
    (lambda () 
        (receive (cols rows) (get-screen-size)
            (format #t "Screen size: ~a columns, ~a rows~%" cols rows)))
    (lambda () (no-raw-mode)))