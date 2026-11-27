;;; HTML5 from SXML, so that pages are quasiquoted data:
;;;
;;;   (sxml->html `(ul (@ (class "items"))
;;;                  ,@(map (lambda (x) `(li ,x)) '("a" "b <c>"))))
;;;     => "<ul class=\"items\"><li>a</li><li>b &lt;c&gt;</li></ul>"
;;;
;;;   (sxml->html '(html (head (title "Hi")) (body (p "x"))) #:doctype? #t)
;;;     => "<!DOCTYPE html>\n<html><head><title>Hi</title></head><body><p>x</p></body></html>"
;;;
;;;   (define-template (card title . body)
;;;     (div (@ (class "card")) (h2 ,title) ,@body))
;;;   (sxml->html `(main (,card (@) "Title" (p "text"))))   ; a procedure as a tag: a component
;;;
;;; The nodes are those of (capy sxml): (tag (@ (name value) ...) child ...), text, and
;;; (*TOP* ...), (*COMMENT* "...") and (*RAW* "...") (written as it is, not escaped); the
;;; lower-case *comment* and *raw* work too. Text is escaped; numbers and characters are text.
;;; For templates, a list whose head is not a symbol or a procedure is spliced into its
;;; parent, and #f, () and the unspecified value write nothing, so `,@` and `,(and test node)`
;;; both work.
;;;
;;; HTML5 rules are followed: void elements (br, img, input, meta, ...) have no end tag and
;;; cannot have children; the text of script and style is written unescaped (and may not
;;; contain its own end tag); attribute values are quoted and escaped.
;;;
;;; Attributes: #f leaves the attribute out and #t (or no value) writes a boolean attribute,
;;; `(disabled ,editing?)`. A list as the value is joined with spaces, without its #f
;;; elements: `(class ("btn" ,(and primary? "btn-primary")))`.
;;;
;;; A procedure in the place of a tag is a component: (,proc (@ attribute ...) child ...)
;;; is written as the result of (proc ATTRIBUTES CHILD ...), where ATTRIBUTES is the list
;;; ((name value) ...), empty when there is no (@ ...).

(library (capy html)
  (export sxml->html
    write-html
    html-escape
    define-template
    html-void-element?)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define)))

  (define void-elements
    '(area base br col embed hr img input link meta source track wbr))

  ;; Whether the element NAME (a symbol) is void: it has no end tag and no children.
  (define (html-void-element? name)
    (and (memq name void-elements) #t))

  ;; Elements whose text is not parsed as HTML: escaping it would change it.
  (define raw-text-elements '(script style))

  (define (fail who message . irritants)
    (apply assertion-violation who message irritants))

  (define (unspecified? x) (eq? x (if #f #f)))

  (define (skip? x) (or (not x) (null? x) (unspecified? x)))

  (define (number->text x)
    (let ([s (number->string x)])
      (if (and (> (string-length s) 1) (char=? (string-ref s 0) #\+))
        (substring s 1 (string-length s))
        s)))

  (define (text-of x who)
    (cond
      [(string? x) x]
      [(number? x) (number->text x)]
      [(char? x) (string x)]
      [(symbol? x) (symbol->string x)]
      [else (fail who "not text" x)]))

  ;; Writes S with & < > escaped, and '"' too when ATTRIBUTE?.
  (define (write-escaped s out attribute?)
    (let ([n (string-length s)])
      (let loop ([i 0] [from 0])
        (define (flush) (when (< from i) (put-string out s from (- i from))))
        (define (escape text) (flush) (put-string out text) (loop (+ i 1) (+ i 1)))
        (if (= i n)
          (flush)
          (let ([c (string-ref s i)])
            (cond
              [(char=? c #\&) (escape "&amp;")]
              [(char=? c #\<) (escape "&lt;")]
              [(char=? c #\>) (escape "&gt;")]
              [(and attribute? (char=? c #\")) (escape "&quot;")]
              [else (loop (+ i 1) from)]))))))

  ;; The text S with & < > and " escaped, for HTML text or attribute values.
  (define (html-escape s)
    (call-with-string-output-port (lambda (out) (write-escaped s out #t))))

  ;; Tag names: a letter, then letters, digits, '-', '_', '.' or ':' (custom elements, SVG).
  (define (check-tag name who)
    (let* ([s (symbol->string name)] [n (string-length s)])
      (unless (and (> n 0)
                (char-alphabetic? (string-ref s 0))
                (< (char->integer (string-ref s 0)) 128)
                (let loop ([i 1])
                  (or (= i n)
                    (let ([c (string-ref s i)])
                      (and (< (char->integer c) 128)
                        (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_ #\. #\:)))
                        (loop (+ i 1)))))))
        (fail who "not a valid HTML tag name" name))
      s))

  ;; Attribute names: anything but whitespace, controls and " ' > / =.
  (define (check-attribute-name name who)
    (let* ([s (symbol->string name)] [n (string-length s)])
      (unless (and (> n 0)
                (let loop ([i 0])
                  (or (= i n)
                    (let ([c (string-ref s i)])
                      (and (not (char-whitespace? c))
                        (>= (char->integer c) 32)
                        (not (memv c '(#\" #\' #\> #\/ #\= #\<)))
                        (loop (+ i 1)))))))
        (fail who "not a valid HTML attribute name" name))
      s))

  (define (attribute-list? x) (and (pair? x) (eq? (car x) '@)))

  (define (split-element x)
    (if (and (pair? (cdr x)) (attribute-list? (cadr x)))
      (values (cdadr x) (cddr x))
      (values '() (cdr x))))

  (define (write-attribute-value v out who)
    (cond
      [(string? v) (write-escaped v out #t)]
      [(list? v)
        (let loop ([items (filter (lambda (x) (not (skip? x))) v)] [first? #t])
          (unless (null? items)
            (unless first? (put-char out #\space))
            (write-attribute-value (car items) out who)
            (loop (cdr items) #f)))]
      [else (write-escaped (text-of v who) out #t)]))

  (define (write-attributes attrs out who)
    (for-each
      (lambda (a)
        (unless (and (pair? a) (symbol? (car a)) (list? a))
          (fail who "an attribute must be (name value)" a))
        (let ([name (check-attribute-name (car a) who)] [values (cdr a)])
          (cond
            [(and (pair? values) (null? (cdr values)) (eq? (car values) #f)) #f]
            [(or (null? values) (and (null? (cdr values)) (eq? (car values) #t)))
              (put-char out #\space)
              (put-string out name)]
            [else
              (put-char out #\space)
              (put-string out name)
              (put-string out "=\"")
              (for-each (lambda (v) (write-attribute-value v out who)) values)
              (put-char out #\")])))
      attrs))

  (define (string-ci-contains? s part)
    (let ([n (string-length s)] [m (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i m) n)
          (or (string-ci=? (substring s i (+ i m)) part) (loop (+ i 1)))))))

  (define (contains? s part)
    (let ([n (string-length s)] [m (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i m) n)
          (or (string=? (substring s i (+ i m)) part) (loop (+ i 1)))))))

  ;; The text of the children of a script or style element.
  (define (raw-text name children who)
    (let ([text (apply string-append
                  (map (lambda (c)
                         (cond
                           [(skip? c) ""]
                           [(and (pair? c) (memq (car c) '(*RAW* *raw*)))
                             (apply string-append (map (lambda (x) (text-of x who)) (cdr c)))]
                           [(pair? c) (fail who (string-append "<" (symbol->string name) "> can only hold text") c)]
                           [else (text-of c who)]))
                    children))])
      (when (string-ci-contains? text (string-append "</" (symbol->string name)))
        (fail who (string-append "the text of <" (symbol->string name) "> cannot contain </"
                    (symbol->string name))
          text))
      text))

  (define (write-node x out who)
    (cond
      [(string? x) (write-escaped x out #f)]
      [(or (number? x) (char? x) (symbol? x)) (write-escaped (text-of x who) out #f)]
      [(skip? x) #f]
      [(pair? x)
        (let ([head (car x)])
          (cond
            [(eq? head '*TOP*)
              (for-each (lambda (c) (unless (attribute-list? c) (write-node c out who))) (cdr x))]
            [(memq head '(*COMMENT* *comment*))
              (let ([text (apply string-append (map (lambda (c) (text-of c who)) (cdr x)))])
                (when (or (contains? text "-->") (contains? text "--!>")
                        (and (> (string-length text) 0) (char=? (string-ref text 0) #\>)))
                  (fail who "this text cannot be in an HTML comment" text))
                (put-string out "<!--")
                (put-string out text)
                (put-string out "-->"))]
            [(memq head '(*RAW* *raw*))
              (for-each (lambda (c) (put-string out (text-of c who))) (cdr x))]
            [(eq? head '*DECL*)
              (put-string out "<!")
              (let loop ([parts (cdr x)] [first? #t])
                (unless (null? parts)
                  (unless first? (put-char out #\space))
                  (put-string out (text-of (car parts) who))
                  (loop (cdr parts) #f)))
              (put-char out #\>)]
            [(memq head '(*PI* @@)) #f]
            [(eq? head '@)
              (fail who "an attribute list (@ ...) must come right after the tag" x)]
            [(symbol? head) (write-element x out who)]
            [(procedure? head)
              (let-values ([(attrs children) (split-element x)])
                (write-node (apply head attrs children) out who))]
            [else (for-each (lambda (c) (write-node c out who)) x)]))]
      [else (fail who "this value cannot be written as HTML" x)]))

  (define (write-element x out who)
    (let*-values ([(head) (car x)]
                  [(name) (check-tag head who)]
                  [(attrs children) (split-element x)])
      (put-char out #\<)
      (put-string out name)
      (write-attributes attrs out who)
      (put-char out #\>)
      (cond
        [(memq head void-elements)
          (unless (for-all skip? children)
            (fail who (string-append "<" name "> is a void element and cannot have children") x))]
        [(memq head raw-text-elements)
          (put-string out (raw-text head children who))
          (put-string out "</") (put-string out name) (put-char out #\>)]
        [else
          (for-each (lambda (c) (write-node c out who)) children)
          (put-string out "</") (put-string out name) (put-char out #\>)])))

  ;; Writes NODE as HTML to PORT; #:doctype? first writes <!DOCTYPE html> and a newline.
  (define (write-html node [port (current-output-port)] #:doctype? [doctype? #f])
    (when doctype? (put-string port "<!DOCTYPE html>\n"))
    (write-node node port 'write-html))

  ;; NODE as an HTML string; #:doctype? starts it with <!DOCTYPE html> and a newline.
  (define (sxml->html node #:doctype? [doctype? #f])
    (call-with-string-output-port
      (lambda (out)
        (when doctype? (put-string out "<!DOCTYPE html>\n"))
        (write-node node out 'sxml->html))))

  ;; (define-template (name formal ...) template) defines a procedure that returns TEMPLATE
  ;; quasiquoted, so `,x` and `,@xs` in it refer to the formals. The formals are those of
  ;; `define`: optional and keyword arguments work.
  ;;
  ;;   (define-template (link href text #:new-tab? [new-tab? #f])
  ;;     (a (@ (href ,href) (target ,(and new-tab? "_blank"))) ,text))
  (define-syntax define-template
    (syntax-rules ()
      [(_ (name . formals) template)
        (define (name . formals) (quasiquote template))]))
  )
