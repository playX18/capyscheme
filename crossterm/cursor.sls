#!r6rs 

(library (crossterm cursor)
    (export 
        move-to
        move-to-next-line
        move-to-previous-line
        move-to-column
        move-to-row
        move-up
        move-right
        move-down
        move-left
        save-position   
        restore-position
        hide-cursor 
        show-cursor 
        enable-blinking
        disable-blinking
        set-cursor-style)
    (import 
        (crossterm macros)
        (rnrs))


    (define (move-to col row)
        (lambda (port)
            (format port (csi "~a;~aH") row col)))
    (define (move-to-next-line n)
        (lambda (port)
            (format port (csi "~aE") n)))

    (define (move-to-previous-line n)
        (lambda (port)
            (format port (csi "~aF") n)))

    (define (move-to-column col)
        (lambda (port)
            (format port (csi "~aG") col)))
    
    (define (move-to-row row)
        (lambda (port)
            (format port (csi "~a;1H") row)))
    (define (move-up n)
        (lambda (port)
            (format port (csi "~aA") n)))
    (define (move-right n)
        (lambda (port)
            (format port (csi "~aC") n)))
    (define (move-down n)
        (lambda (port)  
            (format port (csi "~aB") n)))
    (define (move-left n)
        (lambda (port)
            (format port (csi "~aD") n)))
    (define (save-position)
        (lambda (port)
            (format port "\x1B7;")))
    (define (restore-position)
        (lambda (port)
            (format port "\x1B8;")))
    (define (hide-cursor)
        (lambda (port)
            (format port (csi "?25l"))))
    (define (show-cursor)
        (lambda (port)
            (format port (csi "?25h"))))
    (define (enable-blinking)
        (lambda (port)
            (format port (csi "?12h"))))
    (define (disable-blinking)
        (lambda (port)
            (format port (csi "?12l"))))

    (define (set-cursor-style style)
        (define s (case style
            [(default) "\x1b;[0 q"]
            [(blinking-block) "\x1b;[1 q"]
            [(steady-block) "\x1b;[2 q"]
            [(blinking-underscore) "\x1b;[3 q"]
            [(steady-underscore) "\x1b;[4 q"]
            [(blinking-bar) "\x1b;[5 q"]
            [(steady-bar) "\x1b;[6 q"] ))
        (lambda (port)
            (write-string s port)))        
            
    )