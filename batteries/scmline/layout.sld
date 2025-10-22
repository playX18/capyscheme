(define-library (scmline layout)
    (export
        ; position
        position position?
        position-col position-row
        set-position-col! set-position-row!
        position=?
        position>?
        position<?
        position>=?
        position<=?
        ; layout
        layout
        layout?
        layout-prompt-size set-layout-prompt-size!
        layout-default-prompt? set-layout-default-prompt!
        layout-cursor set-layout-cursor!
        layout-end set-layout-end!
        layout-has-info? set-layout-has-info!)
    (import (rnrs records syntactic (6)))
(begin
    (define-record-type 
        (<position> position position?)
        (fields 
            (mutable col position-col set-position-col!)
            (mutable row position-row set-position-row!)))
    
    (define (position=? x y)
        (and (= (position-col x) (position-col y))
             (= (position-row x) (position-row y))))

    (define (position>? x y)
        (cond 
            [(= (position-row x) (position-row y))
                (> (position-col x) (position-col y))]
            [else 
                (> (position-row x) (position-row y))]))
    
    (define (position>=? x y)
        (cond 
            [(= (position-row x) (position-row y))
                (>= (position-col x) (position-col y))]
            [else 
                (>= (position-row x) (position-row y))]))
    
    (define (position<? x y)
        (cond 
            [(= (position-row x) (position-row y))
                (< (position-col x) (position-col y))]
            [else 
                (< (position-row x) (position-row y))]))
 
    (define (position<=? x y)
        (cond 
            [(= (position-row x) (position-row y))
                (<= (position-col x) (position-col y))]
            [else 
                (<= (position-row x) (position-row y))]))
    
    (define-record-type 
        (<layout> %layout layout?)
        (fields 
            (mutable prompt-size layout-prompt-size set-layout-prompt-size!)
            (mutable default-prompt? layout-default-prompt? set-layout-default-prompt!)
            (mutable cursor layout-cursor set-layout-cursor!)
            (mutable end layout-end set-layout-end!)
            (mutable has-info? layout-has-info? set-layout-has-info!)))
    
    (define (layout)
        (%layout (position 0 0)
                 #f
                 (position 0 0)
                 (position 0 0)
                 #f))))