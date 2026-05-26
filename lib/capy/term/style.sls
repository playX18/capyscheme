(library (capy term style)
  (export rgb rgb? rgb-r rgb-g rgb-b
          ansi-color ansi-color? ansi-color-n
          make-content-style content-style? content-style-foreground
          content-style-background content-style-underline-color
          content-style-attributes
          styled-content? styled-content-text styled-content-style
          style with-style stylize
          set-foreground-color set-background-color set-underline-color
          set-colors reset-color set-attribute set-attributes set-style
          print print-styled-content start-hyperlink end-hyperlink
          available-color-count force-color-output! color-output-forced?)
  (import (rnrs)
          (only (capy) getenv keyword->symbol)
          (capy term private ansi)
          (capy term command))

  (define-record-type (rgb make-rgb rgb?)
    (fields
      (immutable r rgb-r)
      (immutable g rgb-g)
      (immutable b rgb-b)))

  (define-record-type (ansi-color make-ansi-color ansi-color?)
    (fields (immutable n ansi-color-n)))

  (define-record-type (<content-style> %make-content-style content-style?)
    (fields
      (immutable foreground content-style-foreground)
      (immutable background content-style-background)
      (immutable underline-color content-style-underline-color)
      (immutable attributes content-style-attributes)))

  (define-record-type (<styled-content> make-styled-content styled-content?)
    (fields
      (immutable text styled-content-text)
      (immutable style styled-content-style)))

  (define force-color-output-state #f)

  (define (rgb r g b)
    (define (u8 who n)
      (unless (and (integer? n) (<= 0 n 255))
        (assertion-violation who "expected integer in u8 range" n))
      n)
    (make-rgb (u8 'rgb r) (u8 'rgb g) (u8 'rgb b)))

  (define (ansi-color n)
    (unless (and (integer? n) (<= 0 n 255))
      (assertion-violation 'ansi-color "expected integer in u8 range" n))
    (make-ansi-color n))

  (define (make-content-style . args)
    (let loop ([args args] [foreground #f] [background #f] [underline #f] [attributes '()])
      (cond
        [(null? args) (%make-content-style foreground background underline attributes)]
        [(null? (cdr args))
         (assertion-violation 'make-content-style "keyword missing value" (car args))]
        [else
          (case (keyword->symbol (car args))
            [(foreground) (loop (cddr args) (cadr args) background underline attributes)]
            [(background) (loop (cddr args) foreground (cadr args) underline attributes)]
            [(underline-color) (loop (cddr args) foreground background (cadr args) attributes)]
            [(attributes) (loop (cddr args) foreground background underline (cadr args))]
            [else (assertion-violation 'make-content-style "unknown keyword" (car args))])])))

  (define (command name body)
    (make-command name (lambda () body)))

  (define (uint8-string who n)
    (unless (and (integer? n) (<= 0 n 255))
      (assertion-violation who "expected integer in u8 range" n))
    (number->string n))

  (define (base-color-code color foreground? bright-base normal-base reset-code)
    (case color
      [(reset) reset-code]
      [(black) normal-base]
      [(red) (+ normal-base 1)]
      [(green) (+ normal-base 2)]
      [(yellow) (+ normal-base 3)]
      [(blue) (+ normal-base 4)]
      [(magenta) (+ normal-base 5)]
      [(cyan) (+ normal-base 6)]
      [(grey) (+ normal-base 7)]
      [(dark-grey) bright-base]
      [(dark-red) (+ bright-base 1)]
      [(dark-green) (+ bright-base 2)]
      [(dark-yellow) (+ bright-base 3)]
      [(dark-blue) (+ bright-base 4)]
      [(dark-magenta) (+ bright-base 5)]
      [(dark-cyan) (+ bright-base 6)]
      [(white) (+ bright-base 7)]
      [else #f]))

  (define (color-params who color normal-base bright-base reset-code extended-base)
    (cond
      [(symbol? color)
       (let ([code (base-color-code color (= normal-base 30) bright-base normal-base reset-code)])
         (if code
           (list code)
           (assertion-violation who "unknown color" color)))]
      [(ansi-color? color) (list extended-base 5 (ansi-color-n color))]
      [(rgb? color) (list extended-base 2 (rgb-r color) (rgb-g color) (rgb-b color))]
      [else (assertion-violation who "unknown color" color)]))

  (define (foreground-params color)
    (color-params 'set-foreground-color color 30 90 39 38))

  (define (background-params color)
    (color-params 'set-background-color color 40 100 49 48))

  (define (underline-params color)
    (color-params 'set-underline-color color 0 0 59 58))

  (define (attribute-param attr)
    (case attr
      [(reset) "0"]
      [(bold) "1"]
      [(dim) "2"]
      [(italic) "3"]
      [(underlined) "4"]
      [(double-underlined) "4:2"]
      [(undercurled) "4:3"]
      [(underdotted) "4:4"]
      [(underdashed) "4:5"]
      [(slow-blink) "5"]
      [(rapid-blink) "6"]
      [(reverse) "7"]
      [(hidden) "8"]
      [(crossed-out) "9"]
      [(fraktur) "20"]
      [(no-bold) "21"]
      [(normal-intensity) "22"]
      [(no-italic) "23"]
      [(no-underline) "24"]
      [(no-blink) "25"]
      [(no-reverse) "27"]
      [(no-hidden) "28"]
      [(not-crossed-out) "29"]
      [(framed) "51"]
      [(encircled) "52"]
      [(over-lined) "53"]
      [(not-framed-or-encircled) "54"]
      [(not-over-lined) "55"]
      [else (assertion-violation 'set-attribute "unknown attribute" attr)]))

  (define (style->params style)
    (append
      (if (content-style-foreground style)
        (foreground-params (content-style-foreground style))
        '())
      (if (content-style-background style)
        (background-params (content-style-background style))
        '())
      (if (content-style-underline-color style)
        (underline-params (content-style-underline-color style))
        '())
      (map attribute-param (content-style-attributes style))))

  (define (set-foreground-color color)
    (command 'set-foreground-color (sgr (foreground-params color))))

  (define (set-background-color color)
    (command 'set-background-color (sgr (background-params color))))

  (define (set-underline-color color)
    (command 'set-underline-color (sgr (underline-params color))))

  (define (set-colors foreground background)
    (command 'set-colors (sgr (append (foreground-params foreground)
                                      (background-params background)))))

  (define (reset-color)
    (command 'reset-color (sgr '(0))))

  (define (set-attribute attr)
    (command 'set-attribute (sgr (list (attribute-param attr)))))

  (define (set-attributes attrs)
    (command 'set-attributes (sgr (map attribute-param attrs))))

  (define (set-style style)
    (command 'set-style (sgr (style->params style))))

  (define (print text)
    (command 'print
             (if (string? text)
               text
               (assertion-violation 'print "expected string" text))))

  (define (style text)
    (make-styled-content text (make-content-style)))

  (define (with-style text content-style)
    (make-styled-content text content-style))

  (define (stylize text . specs)
    (define (apply-spec spec foreground background attributes)
      (cond
        [(and (symbol? spec)
              (> (string-length (symbol->string spec)) 3)
              (string=? (substring (symbol->string spec) 0 3) "on-"))
         (values foreground
                 (string->symbol (substring (symbol->string spec) 3 (string-length (symbol->string spec))))
                 attributes)]
        [(memq spec '(bold dim italic underlined double-underlined undercurled underdotted
                           underdashed slow-blink rapid-blink reverse hidden crossed-out
                           fraktur no-bold normal-intensity no-italic no-underline no-blink
                           no-reverse no-hidden not-crossed-out framed encircled over-lined
                           not-framed-or-encircled not-over-lined))
         (values foreground background (append attributes (list spec)))]
        [else (values spec background attributes)]))
    (let loop ([specs specs] [foreground #f] [background #f] [attributes '()])
      (if (null? specs)
        (with-style text (make-content-style #:foreground foreground
                                             #:background background
                                             #:attributes attributes))
        (call-with-values
          (lambda () (apply-spec (car specs) foreground background attributes))
          (lambda (next-foreground next-background next-attributes)
            (loop (cdr specs) next-foreground next-background next-attributes))))))

  (define (print-styled-content content)
    (unless (styled-content? content)
      (assertion-violation 'print-styled-content "expected styled content" content))
    (let* ([params (style->params (styled-content-style content))]
           [body (if (null? params)
                   (styled-content-text content)
                   (string-append (sgr params) (styled-content-text content) (sgr '(0))))])
      (command 'print-styled-content body)))

  (define (start-hyperlink uri)
    (unless (string? uri)
      (assertion-violation 'start-hyperlink "expected string uri" uri))
    (command 'start-hyperlink
             (string-append esc "]8;;" (osc-escape uri) esc "\\")))

  (define (end-hyperlink)
    (command 'end-hyperlink (string-append esc "]8;;" esc "\\")))

  (define (string-contains? haystack needle)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (let loop ([i 0])
        (cond
          [(> (+ i nlen) hlen) #f]
          [(string=? (substring haystack i (+ i nlen)) needle) #t]
          [else (loop (+ i 1))]))))

  (define (available-color-count)
    (let ([colorterm (or (getenv "COLORTERM") "")]
          [term (or (getenv "TERM") "")])
      (cond
        [(or (string-contains? colorterm "24bit")
             (string-contains? colorterm "truecolor")
             (string-contains? term "24bit")
             (string-contains? term "truecolor"))
         65535]
        [(or (string-contains? colorterm "256")
             (string-contains? term "256"))
         256]
        [else 8])))

  (define (force-color-output! enabled)
    (set! force-color-output-state (and enabled #t))
    force-color-output-state)

  (define (color-output-forced?)
    force-color-output-state))
