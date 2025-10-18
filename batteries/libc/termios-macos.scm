(export
    ;; Special Control Characters
    VEOF VEOL VEOL2 VERASE VWERASE VKILL VREPRINT VINTR VQUIT VSUSP
    VDSUSP VSTART VSTOP VLNEXT VDISCARD VMIN VTIME VSTATUS NCCS
    
    ;; Input flags
    IGNBRK BRKINT IGNPAR PARMRK INPCK ISTRIP INLCR IGNCR ICRNL
    IXON IXOFF IXANY IMAXBEL IUTF8
    
    ;; Output flags
    OPOST ONLCR OXTABS ONOEOT OCRNL ONOCR ONLRET OFILL NLDLY TABDLY
    CRDLY FFDLY BSDLY VTDLY OFDEL NL0 NL1 NL2 NL3 TAB0 TAB1 TAB2 TAB3
    CR0 CR1 CR2 CR3 FF0 FF1 BS0 BS1 VT0 VT1
    
    ;; Control flags
    CIGNORE CSIZE CS5 CS6 CS7 CS8 CSTOPB CREAD PARENB PARODD HUPCL
    CLOCAL CCTS_OFLOW CRTS_IFLOW CDTR_IFLOW CDSR_OFLOW CCAR_OFLOW MDMBUF
    
    ;; Local flags
    ECHOKE ECHOE ECHOK ECHO ECHONL ECHOPRT ECHOCTL ISIG ICANON ALTWERASE
    IEXTEN EXTPROC TOSTOP FLUSHO NOKERNINFO PENDIN NOFLSH
    
    ;; Types
    tcflag_t cc_t speed_t
    
    ;; Record type
    <termios> termios termios? termios-input-flags termios-output-flags
    termios-control-flags termios-local-flags termios-control-chars
    termios-input-speed termios-output-speed

    set-termios-input-flags! set-termios-output-flags!
    set-termios-control-flags! set-termios-local-flags!
    set-termios-control-chars! set-termios-input-speed! set-termios-output-speed!
    
    ;; C struct
    %termios sizeof-termios read-termios write-termios!
    
    ;; Action constants
    TCSANOW TCSADRAIN TCSAFLUSH TCSASOFT
    
    ;; Speed constants
    B0 B50 B75 B110 B134 B150 B200 B300 B600 B1200 B1800 B2400 B4800
    B9600 B19200 B38400 B7200 B14400 B28800 B57600 B76800 B115200
    B230400 EXTA EXTB
    
    ;; Flow control constants
    TCIFLUSH TCOFLUSH TCIOFLUSH TCOOFF TCOON TCIOFF TCION
    
    ; decls
    tcgetattr
    tcsetattr
    tcdrain
    tcflow
    tcflush
    tcsendbreak)

;; Special Control Characters - Index into c_cc[] character array
(define VEOF 0)          ; ICANON
(define VEOL 1)          ; ICANON
(define VEOL2 2)         ; ICANON together with IEXTEN
(define VERASE 3)        ; ICANON
(define VWERASE 4)       ; ICANON together with IEXTEN
(define VKILL 5)         ; ICANON
(define VREPRINT 6)      ; ICANON together with IEXTEN
(define VINTR 8)         ; ISIG
(define VQUIT 9)         ; ISIG
(define VSUSP 10)        ; ISIG
(define VDSUSP 11)       ; ISIG together with IEXTEN
(define VSTART 12)       ; IXON, IXOFF
(define VSTOP 13)        ; IXON, IXOFF
(define VLNEXT 14)       ; IEXTEN
(define VDISCARD 15)     ; IEXTEN
(define VMIN 16)         ; !ICANON
(define VTIME 17)        ; !ICANON
(define VSTATUS 18)      ; ICANON together with IEXTEN
(define NCCS 20)

;; Input flags - software input processing
(define IGNBRK #x00000001)    ; ignore BREAK condition
(define BRKINT #x00000002)    ; map BREAK to SIGINTR
(define IGNPAR #x00000004)    ; ignore (discard) parity errors
(define PARMRK #x00000008)    ; mark parity and framing errors
(define INPCK #x00000010)     ; enable checking of parity errors
(define ISTRIP #x00000020)    ; strip 8th bit off chars
(define INLCR #x00000040)     ; map NL into CR
(define IGNCR #x00000080)     ; ignore CR
(define ICRNL #x00000100)     ; map CR to NL (ala CRMOD)
(define IXON #x00000200)      ; enable output flow control
(define IXOFF #x00000400)     ; enable input flow control
(define IXANY #x00000800)     ; any char will restart after stop
(define IMAXBEL #x00002000)   ; ring bell on input queue full
(define IUTF8 #x00004000)     ; maintain state for UTF-8 VERASE

;; Output flags - software output processing
(define OPOST #x00000001)     ; enable following output processing
(define ONLCR #x00000002)     ; map NL to CR-NL (ala CRMOD)
(define OXTABS #x00000004)    ; expand tabs to spaces
(define ONOEOT #x00000008)    ; discard EOT's (^D) on output
(define OCRNL #x00000010)     ; map CR to NL on output
(define ONOCR #x00000020)     ; no CR output at column 0
(define ONLRET #x00000040)    ; NL performs CR function
(define OFILL #x00000080)     ; use fill characters for delay
(define NLDLY #x00000300)     ; \n delay
(define TABDLY #x00000c04)    ; horizontal tab delay
(define CRDLY #x00003000)     ; \r delay
(define FFDLY #x00004000)     ; form feed delay
(define BSDLY #x00008000)     ; \b delay
(define VTDLY #x00010000)     ; vertical tab delay
(define OFDEL #x00020000)     ; fill is DEL, else NUL
(define NL0 #x00000000)
(define NL1 #x00000100)
(define NL2 #x00000200)
(define NL3 #x00000300)
(define TAB0 #x00000000)
(define TAB1 #x00000400)
(define TAB2 #x00000800)
(define TAB3 #x00000004)
(define CR0 #x00000000)
(define CR1 #x00001000)
(define CR2 #x00002000)
(define CR3 #x00003000)
(define FF0 #x00000000)
(define FF1 #x00004000)
(define BS0 #x00000000)
(define BS1 #x00008000)
(define VT0 #x00000000)
(define VT1 #x00010000)

;; Control flags - hardware control of terminal
(define CIGNORE #x00000001)   ; ignore control flags
(define CSIZE #x00000300)     ; character size mask
(define CS5 #x00000000)       ; 5 bits (pseudo)
(define CS6 #x00000100)       ; 6 bits
(define CS7 #x00000200)       ; 7 bits
(define CS8 #x00000300)       ; 8 bits
(define CSTOPB #x00000400)    ; send 2 stop bits
(define CREAD #x00000800)     ; enable receiver
(define PARENB #x00001000)    ; parity enable
(define PARODD #x00002000)    ; odd parity, else even
(define HUPCL #x00004000)     ; hang up on last close
(define CLOCAL #x00008000)    ; ignore modem status lines
(define CCTS_OFLOW #x00010000) ; CTS flow control of output
(define CRTS_IFLOW #x00020000) ; RTS flow control of input
(define CDTR_IFLOW #x00040000) ; DTR flow control of input
(define CDSR_OFLOW #x00080000) ; DSR flow control of output
(define CCAR_OFLOW #x00100000) ; DCD flow control of output
(define MDMBUF #x00100000)    ; old name for CCAR_OFLOW

;; Local flags
(define ECHOKE #x00000001)    ; visual erase for line kill
(define ECHOE #x00000002)     ; visually erase chars
(define ECHOK #x00000004)     ; echo NL after line kill
(define ECHO #x00000008)      ; enable echoing
(define ECHONL #x00000010)    ; echo NL even if ECHO is off
(define ECHOPRT #x00000020)   ; visual erase mode for hardcopy
(define ECHOCTL #x00000040)   ; echo control chars as ^(Char)
(define ISIG #x00000080)      ; enable signals INTR, QUIT, [D]SUSP
(define ICANON #x00000100)    ; canonicalize input lines
(define ALTWERASE #x00000200) ; use alternate WERASE algorithm
(define IEXTEN #x00000400)    ; enable DISCARD and LNEXT
(define EXTPROC #x00000800)   ; external processing
(define TOSTOP #x00400000)    ; stop background jobs from output
(define FLUSHO #x00800000)    ; output being flushed (state)
(define NOKERNINFO #x02000000) ; no kernel output from VSTATUS
(define PENDIN #x20000000)    ; XXX retype pending input (state)
(define NOFLSH #x80000000)    ; don't flush after interrupt
(eval-when (expand load eval)
    (define tcflag_t unsigned-long)
    (define cc_t     uint8)
    (define speed_t  unsigned-long))

(define-record-type <termios>
    (termios input-flags output-flags control-flags local-flags
            control-chars
            input-speed output-speed)
    termios?
    (input-flags      termios-input-flags set-termios-input-flags!)
    (output-flags     termios-output-flags set-termios-output-flags!)
    (control-flags    termios-control-flags set-termios-control-flags!)
    (local-flags      termios-local-flags set-termios-local-flags!)
    (control-chars    termios-control-chars set-termios-control-chars!)
    (input-speed      termios-input-speed set-termios-input-speed!)
    (output-speed     termios-output-speed set-termios-output-speed!))

(define-c-struct %termios                        ; <sys/termios.h>
    sizeof-termios
    termios
    read-termios
    write-termios!
    (input-flags      unsigned-long)
    (output-flags     unsigned-long)
    (control-flags    unsigned-long)
    (local-flags      unsigned-long)
    (control-chars    `(array ,uint8 20))
    (input-speed      unsigned-long)
    (output-speed     unsigned-long))

(define TCSANOW 0)      ; make change immediate
(define TCSADRAIN 1)    ; drain output, then change
(define TCSAFLUSH 2)    ; drain output, flush input
(define TCSASOFT  #x10) ; flag - don't alter h.w state

; standard speeds
(define B0 0)
(define B50 50)
(define B75 75)
(define B110 110)
(define B134 134)
(define B150 150)
(define B200 200)
(define B300 300)
(define B600 600)
(define B1200 1200)
(define B1800 1800)
(define B2400 2400)
(define B4800 4800)
(define B9600 9600)
(define B19200 19200)
(define B38400 38400)
(define B7200 7200)
(define B14400 14400)
(define B28800 28800)
(define B57600 57600)
(define B76800 76800)
(define B115200 115200)
(define B230400 230400)
(define EXTA 19200)
(define EXTB 38400)

(define TCIFLUSH  1)
(define TCOFLUSH  2)
(define TCIOFLUSH 3)
(define TCOOFF    1)
(define TCOON     2)
(define TCIOFF    3)
(define TCION     4)

; begin decls
(define tcgetattr 
    (let ([proc (foreign-library-function 
                    #f 
                    "tcgetattr"
                    int
                    `(,int *))])
        (lambda (fd)
            (let* ([bv (make-bytevector/nonmoving sizeof-termios 0)]
                    [ret (proc fd (bytevector->pointer bv))])
                (if (zero? ret)
                    (read-termios bv)
                    (error 'tcgetattr "tcgetattr failed" ret))))))
(define tcsetattr 
    (let ([proc (foreign-library-function 
                    #f
                    "tcsetattr"
                    int 
                    `(,int ,int *))])
        (lambda (fd actions termios)
            (define bv (make-bytevector/nonmoving sizeof-termios))

            (write-termios! bv 0 
                (termios-input-flags termios)
                (termios-output-flags termios)
                (termios-control-flags termios)
                (termios-local-flags termios)
                (termios-control-chars termios)
                (termios-input-speed termios)
                (termios-output-speed termios))
            (let ([ret (proc fd actions (bytevector->pointer bv))])
                (unless (zero? ret)
                    (error 'tcsetattr "tcgetattr failed" ret))
                (unspecified)))))

(define tcdrain 
    (let ([proc (foreign-library-function 
                    #f
                    "tcdrain"
                    int
                    `(,int))])
        (lambda (fd)
            (let ([ret (proc fd)])
                (unless (zero? ret)
                    (error 'tcdrain "tcdrain failed" ret))
                (unspecified)))))

(define tcflow 
    (let ([proc (foreign-library-function 
                    #f
                    "tcflow"
                    int
                    `(,int ,int))])
        (lambda (fd action)
            (let ([ret (proc fd action)])
                (unless (zero? ret)
                    (error 'tcflow "tcflow failed" ret))
                (unspecified)))))

(define tcflush 
    (let ([proc (foreign-library-function 
                    #f
                    "tcflush"
                    int
                    `(,int ,int))])
        (lambda (fd queue)
            (let ([ret (proc fd queue)])
                (unless (zero? ret)
                    (error 'tcflush "tcflush failed" ret))
                (unspecified)))))

(define tcsendbreak 
    (let ([proc (foreign-library-function 
                    #f
                    "tcsendbreak"
                    int
                    `(,int ,int))])
        (lambda (fd duration)
            (let ([ret (proc fd duration)])
                (unless (zero? ret)
                    (error 'tcsendbreak "tcsendbreak failed" ret))
                (unspecified)))))
; end decls