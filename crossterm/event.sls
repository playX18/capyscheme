#!r6rs
(library (crossterm event)
    (export
        keycode? modifier? media? 

        ; Keycodes
        keycode-backspace make-keycode-backspace keycode-backspace?
        keycode-enter make-keycode-enter keycode-enter?
        keycode-left make-keycode-left keycode-left?
        keycode-right make-keycode-right keycode-right?
        keycode-up make-keycode-up keycode-up?
        keycode-down make-keycode-down keycode-down?
        keycode-home make-keycode-home keycode-home?
        keycode-end make-keycode-end keycode-end?
        keycode-page-up make-keycode-page-up keycode-page-up?
        keycode-page-down make-keycode-page-down keycode-page-down?
        keycode-tab make-keycode-tab keycode-tab?
        keycode-back-tab make-keycode-back-tab keycode-back-tab?
        keycode-delete make-keycode-delete keycode-delete?
        keycode-insert make-keycode-insert keycode-insert?
        keycode-f make-keycode-f keycode-f? keycode-f-fn
        keycode-char make-keycode-char keycode-char? keycode-char-ch
        keycode-null make-keycode-null keycode-null?
        keycode-esc make-keycode-esc keycode-esc?
        keycode-caps-lock make-keycode-caps-lock keycode-caps-lock?
        keycode-scroll-lock make-keycode-scroll-lock keycode-scroll-lock?
        keycode-num-lock make-keycode-num-lock keycode-num-lock?
        keycode-print-screen make-keycode-print-screen keycode-print-screen?
        keycode-pause make-keycode-pause keycode-pause?
        keycode-menu make-keycode-menu keycode-menu?
        keycode-keypad-begin make-keycode-keypad-begin keycode-keypad-begin?
        keycode-media make-keycode-media keycode-media? keycode-media-media
        keycode-modifier make-keycode-modifier keycode-modifier? keycode-modifier-modifier

        ; Modifiers
        modifier-lshift make-modifier-lshift modifier-lshift?
        modifier-lctrl make-modifier-lctrl modifier-lctrl?
        modifier-lalt make-modifier-lalt modifier-lalt?
        modifier-lsuper make-modifier-lsuper modifier-lsuper?
        modifier-lhyper make-modifier-lhyper modifier-lhyper?
        modifier-lmeta make-modifier-lmeta modifier-lmeta?
        modifier-rshift make-modifier-rshift modifier-rshift?
        modifier-rctrl make-modifier-rctrl modifier-rctrl?
        modifier-ralt make-modifier-ralt modifier-ralt?
        modifier-rsuper make-modifier-rsuper modifier-rsuper?
        modifier-rhyper make-modifier-rhyper modifier-rhyper?
        modifier-rmeta make-modifier-rmeta modifier-rmeta?
        modifier-iso3-shift make-modifier-iso3-shift modifier-iso3-shift?
        modifier-iso5-shift make-modifier-iso5-shift modifier-iso5-shift?

        ; Media
        play make-play play?
        pause make-pause pause?
        play/pause make-play/pause play/pause?
        reverse make-reverse reverse?
        stop make-stop stop?
        fast-forward make-fast-forward fast-forward?
        rewind make-rewind rewind?
        next-track make-next-track next-track?
        previous-track make-previous-track previous-track?
        record make-record record?
        lower-volume make-lower-volume lower-volume?
        raise-volume make-raise-volume raise-volume?
        mute-volume make-mute-volume mute-volume?)

    (import (rnrs) 
            (rnrs hashtables)
            (core match))

(eval-when (expand load eval)
    (define *keycodes* (make-eq-hashtable))
    (define *modifiers* (make-eq-hashtable))
    (define *media* (make-eq-hashtable)))

(define (keycode? obj)
    (and (record? obj)
        (hashtable-exists? *keycodes* (record-type obj))))

(define (modifier? obj)
    (and (record? obj)
        (hashtable-exists? *modifiers* (record-type obj))))
(define (media? obj)
    (and (record? obj)
        (hashtable-exists? *media* (record-type obj))))

(define-syntax define-keycode 
    (lambda (stx)
        (syntax-case stx () 
            [(_ name fld ...)
                #`(begin 
                    (define-record-type name 
                        (fields fld ...))
                    (hashtable-set! *keycodes* name #t))])))

(define-syntax define-modifier
    (lambda (stx)
        (syntax-case stx ()
            [(_ name)
                #`(begin 
                    (define-record-type name)
                    (hashtable-set! *modifiers* name #t))])))

(define-syntax define-media
    (lambda (stx)
        (syntax-case stx ()
            [(_ name)
                #`(begin 
                    (define-record-type name)
                    (hashtable-set! *media* name #t))])))

(define-keycode keycode-backspace)
(define-keycode keycode-enter)
(define-keycode keycode-left)
(define-keycode keycode-right)
(define-keycode keycode-up)
(define-keycode keycode-down)
(define-keycode keycode-home)
(define-keycode keycode-end)
(define-keycode keycode-page-up)
(define-keycode keycode-page-down)
(define-keycode keycode-tab)
(define-keycode keycode-back-tab)
(define-keycode keycode-delete)
(define-keycode keycode-insert)
(define-keycode keycode-f fn)
(define-keycode keycode-char ch)
(define-keycode keycode-null)
(define-keycode keycode-esc)
(define-keycode keycode-caps-lock)
(define-keycode keycode-scroll-lock)
(define-keycode keycode-num-lock)
(define-keycode keycode-print-screen)
(define-keycode keycode-pause)
(define-keycode keycode-menu)
(define-keycode keycode-keypad-begin)
(define-keycode keycode-media media)
(define-keycode keycode-modifier modifier)


(define-modifier modifier-lshift)
(define-modifier modifier-lctrl)
(define-modifier modifier-lalt)
(define-modifier modifier-lsuper)
(define-modifier modifier-lhyper)
(define-modifier modifier-lmeta)
(define-modifier modifier-rshift)
(define-modifier modifier-rctrl)
(define-modifier modifier-ralt)
(define-modifier modifier-rsuper)
(define-modifier modifier-rhyper)
(define-modifier modifier-rmeta)
(define-modifier modifier-iso3-shift)
(define-modifier modifier-iso5-shift)

(define-media play)
(define-media pause)
(define-media play/pause)
(define-media reverse)
(define-media stop)
(define-media fast-forward)
(define-media rewind)
(define-media next-track)
(define-media previous-track)
(define-media record)
(define-media lower-volume)
(define-media raise-volume)
(define-media mute-volume)


    )
    