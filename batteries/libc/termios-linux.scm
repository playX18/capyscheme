

;; termios constants for all UNIX-like systems except macOS
;; based on musl

(export
    ;; Control characters indices
    VINTR VQUIT VERASE VKILL VEOF VTIME VMIN VSWTC VSTART VSTOP
    VSUSP VEOL VREPRINT VDISCARD VWERASE VLNEXT VEOL2
    
    ;; Input flags
    IGNBRK BRKINT IGNPAR PARMRK INPCK ISTRIP INLCR IGNCR ICRNL
    IUCLC IXON IXANY IXOFF IMAXBEL IUTF8
    
    ;; Output flags
    OPOST OLCUC ONLCR OCRNL ONOCR ONLRET OFILL OFDEL
    NLDLY NL0 NL1 CRDLY CR0 CR1 CR2 CR3 TABDLY TAB0 TAB1 TAB2 TAB3
    BSDLY BS0 BS1 FFDLY FF0 FF1 VTDLY VT0 VT1
    
    ;; Baud rates
    B0 B50 B75 B110 B134 B150 B200 B300 B600 B1200 B1800 B2400
    B4800 B9600 B19200 B38400 B57600 B115200 B230400 B460800
    B500000 B576000 B921600 B1000000 B1152000 B1500000 B2000000
    B2500000 B3000000 B3500000 B4000000
    
    ;; Control flags
    CSIZE CS5 CS6 CS7 CS8 CSTOPB CREAD PARENB PARODD HUPCL CLOCAL
    
    ;; Local flags
    ISIG ICANON ECHO ECHOE ECHOK ECHONL NOFLSH TOSTOP IEXTEN
    
    ;; tcflow() and tcflush() constants
    TCOOFF TCOON TCIOFF TCION TCIFLUSH TCOFLUSH TCIOFLUSH
    
    ;; tcsetattr() constants
    TCSANOW TCSADRAIN TCSAFLUSH
    
    ;; GNU/BSD extensions
    EXTA EXTB CBAUD CBAUDEX CIBAUD CMSPAR CRTSCTS XCASE ECHOCTL
    ECHOPRT ECHOKE FLUSHO PENDIN EXTPROC XTABS
    
    ;; Constants
    NCCS
    
    ;; Record type and accessors
    <termios> termios termios?
    termios-input-flags termios-output-flags termios-control-flags
    termios-local-flags termios-line-discipline termios-control-chars
    termios-input-speed termios-output-speed
    set-termios-input-flags! set-termios-output-flags!
    set-termios-control-flags! set-termios-local-flags!
    set-termios-control-chars! set-termios-input-speed! set-termios-output-speed
    set-termios-line-discipline!
    
    ;; C struct bindings
    sizeof-termios read-termios write-termios!
    
    ;; Function bindings
    cfmakeraw tcgetattr tcsetattr tcdrain tcflow tcflush tcsendbreak

    TIOCGWINSZ TIOCSWINSZ
    get-winsize
    set-winsize!
    winsize
    winsize?
    winsize-rows
    winsize-cols
    winsize-xpixel
    winsize-ypixel
    set-winsize-rows!
    set-winsize-cols!
    set-winsize-xpixel!
    set-winsize-ypixel!)

;; Control characters indices
(define VINTR     0)
(define VQUIT     1)
(define VERASE    2)
(define VKILL     3)
(define VEOF      4)
(define VTIME     5)
(define VMIN      6)
(define VSWTC     7)
(define VSTART    8)
(define VSTOP     9)
(define VSUSP    10)
(define VEOL     11)
(define VREPRINT 12)
(define VDISCARD 13)
(define VWERASE  14)
(define VLNEXT   15)
(define VEOL2    16)

;; Input flags
(define IGNBRK  #o000001)
(define BRKINT  #o000002)
(define IGNPAR  #o000004)
(define PARMRK  #o000010)
(define INPCK   #o000020)
(define ISTRIP  #o000040)
(define INLCR   #o000100)
(define IGNCR   #o000200)
(define ICRNL   #o000400)
(define IUCLC   #o001000)
(define IXON    #o002000)
(define IXANY   #o004000)
(define IXOFF   #o010000)
(define IMAXBEL #o020000)
(define IUTF8   #o040000)

;; Output flags
(define OPOST  #o000001)
(define OLCUC  #o000002)
(define ONLCR  #o000004)
(define OCRNL  #o000010)
(define ONOCR  #o000020)
(define ONLRET #o000040)
(define OFILL  #o000100)
(define OFDEL  #o000200)

(define NLDLY  #o000400)
(define NL0    #o000000)
(define NL1    #o000400)
(define CRDLY  #o003000)
(define CR0    #o000000)
(define CR1    #o001000)
(define CR2    #o002000)
(define CR3    #o003000)
(define TABDLY #o014000)
(define TAB0   #o000000)
(define TAB1   #o004000)
(define TAB2   #o010000)
(define TAB3   #o014000)
(define BSDLY  #o020000)
(define BS0    #o000000)
(define BS1    #o020000)
(define FFDLY  #o100000)
(define FF0    #o000000)
(define FF1    #o100000)

(define VTDLY  #o040000)
(define VT0    #o000000)
(define VT1    #o040000)

;; Baud rates
(define B0       #o000000)
(define B50      #o000001)
(define B75      #o000002)
(define B110     #o000003)
(define B134     #o000004)
(define B150     #o000005)
(define B200     #o000006)
(define B300     #o000007)
(define B600     #o000010)
(define B1200    #o000011)
(define B1800    #o000012)
(define B2400    #o000013)
(define B4800    #o000014)
(define B9600    #o000015)
(define B19200   #o000016)
(define B38400   #o000017)

(define B57600   #o010001)
(define B115200  #o010002)
(define B230400  #o010003)
(define B460800  #o010004)
(define B500000  #o010005)
(define B576000  #o010006)
(define B921600  #o010007)
(define B1000000 #o010010)
(define B1152000 #o010011)
(define B1500000 #o010012)
(define B2000000 #o010013)
(define B2500000 #o010014)
(define B3000000 #o010015)
(define B3500000 #o010016)
(define B4000000 #o010017)



;; Control flags
(define CSIZE  #o000060)
(define CS5    #o000000)
(define CS6    #o000020)
(define CS7    #o000040)
(define CS8    #o000060)
(define CSTOPB #o000100)
(define CREAD  #o000200)
(define PARENB #o000400)
(define PARODD #o001000)
(define HUPCL  #o002000)
(define CLOCAL #o004000)

;; Local flags
(define ISIG   #o000001)
(define ICANON #o000002)
(define ECHO   #o000010)
(define ECHOE  #o000020)
(define ECHOK  #o000040)
(define ECHONL #o000100)
(define NOFLSH #o000200)
(define TOSTOP #o000400)
(define IEXTEN #o100000)

;; tcflow() and tcflush() constants
(define TCOOFF 0)
(define TCOON  1)
(define TCIOFF 2)
(define TCION  3)

(define TCIFLUSH  0)
(define TCOFLUSH  1)
(define TCIOFLUSH 2)

;; tcsetattr() constants
(define TCSANOW   0)
(define TCSADRAIN 1)
(define TCSAFLUSH 2)

;; GNU/BSD extensions
(define EXTA    #o000016)
(define EXTB    #o000017)
(define CBAUD   #o010017)
(define CBAUDEX #o010000)
(define CIBAUD  #o002003600000)
(define CMSPAR  #o010000000000)
(define CRTSCTS #o020000000000)

(define XCASE   #o000004)
(define ECHOCTL #o001000)
(define ECHOPRT #o002000)
(define ECHOKE  #o004000)
(define FLUSHO  #o010000)
(define PENDIN  #o040000)
(define EXTPROC #o200000)

(define XTABS  #o014000)

(define NCCS 32)
(define-record-type <winsize>
    (winsize rows cols xpixel ypixel)
    winsize?
    (rows winsize-rows set-winsize-rows!)
    (cols winsize-cols set-winsize-cols!)
    (xpixel winsize-xpixel set-winsize-xpixel!)
    (ypixel winsize-ypixel set-winsize-ypixel!))
    
(define-c-struct %winsize
    sizeof-winsize
    winsize
    read-winsize
    write-winsize!
    (rows uint16)
    (cols uint16)   
    (xpixel uint16)
    (ypixel uint16))


(define-record-type <termios>
    (termios input-flags output-flags control-flags local-flags
            line-discipline control-chars
            input-speed output-speed)
    termios?
    (input-flags      termios-input-flags set-termios-input-flags!)
    (output-flags     termios-output-flags set-termios-output-flags!)
    (control-flags    termios-control-flags set-termios-control-flags!)
    (local-flags      termios-local-flags set-termios-local-flags!)
    (line-discipline  termios-line-discipline set-termios-line-discipline!)
    (control-chars    termios-control-chars set-termios-control-chars!)
    (input-speed      termios-input-speed set-termios-input-speed!)
    (output-speed     termios-output-speed set-termios-output-speed!))

(define-c-struct %termios                        ; <bits/termios.h>
    sizeof-termios
    termios
    read-termios
    write-termios!
    (input-flags      unsigned-int)
    (output-flags     unsigned-int)
    (control-flags    unsigned-int)
    (local-flags      unsigned-int)
    (line-discipline  uint8)
    (control-chars    `(array ,uint8 32))
    (input-speed      unsigned-int)
    (output-speed     unsigned-int))




; begin decls
(define cfmakeraw 
    (let ([proc (foreign-library-function 
                    #f
                    "cfmakeraw"
                    void
                    `(*))])
        (lambda (t)
            (define bv (make-bytevector/nonmoving sizeof-termios))
            (write-termios! bv 0 
                (termios-input-flags t)
                (termios-output-flags t)
                (termios-control-flags t)
                (termios-local-flags t)
                (termios-line-discipline t)
                (termios-control-chars t)
                (termios-input-speed t)
                (termios-output-speed t))
            (proc (bytevector->pointer bv))
            (read-termios bv))))

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
                (termios-line-discipline termios)
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


(define TIOCGWINSZ  21523)
(define TIOCSWINSZ  21524)

(define (get-winsize fd)
    (define bv (make-bytevector/nonmoving sizeof-winsize))
    (define res (ioctl/pointer fd TIOCGWINSZ (bytevector->pointer bv)))
    (unless (zero? res)
        (assertion-violation 'winsize "ioctl TIOCGWINSZ failed" res))
    (let* ([ws-struct (read-winsize bv)]
           [rows (winsize-rows ws-struct)]
           [cols (winsize-cols ws-struct)]
           [xpixel (winsize-xpixel ws-struct)]
           [ypixel (winsize-ypixel ws-struct)])
        (winsize rows cols xpixel ypixel)))


(define (set-winsize! fd ws)
    (define bv (make-bytevector/nonmoving sizeof-winsize))
    (write-winsize! bv 0
        (winsize-rows ws)
        (winsize-cols ws)
        (winsize-xpixel ws) 
        (winsize-ypixel ws))
    (unless (zero? (%ioctl fd TIOCSWINSZ (bytevector->pointer bv)))
        (error 'set-winsize! "ioctl TIOCSWINSZ failed" res))
    (unspecified))