(library (crossterm)
    (export)
    (import 
            (crossterm tty)
            (crossterm terminal)
            (crossterm cursor)
            (crossterm event)
            (crossterm macros))

(with-raw-mode 
    (format #t "in raw mode~%")
    (execute (current-output-port)
        (enable-blinking)
        (set-cursor-style 'steady-underscore))
    (read))

(receive (cols rows) (window-size)
    (format #t "Terminal size: ~a cols, ~a rows~%" cols rows))


)