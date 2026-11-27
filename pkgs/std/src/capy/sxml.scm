;;; SXML: XML as S-expressions. Writing SXML as XML, parsing XML into SXML, and selecting
;;; nodes by path.
;;;
;;;   (sxml->xml '(note (@ (id "n1") (lang "en")) (to "Tove") (body "A & B")))
;;;     => "<note id=\"n1\" lang=\"en\"><to>Tove</to><body>A &amp; B</body></note>"
;;;   (xml->sxml "<note id=\"n1\"><to>Tove</to></note>")
;;;     => (*TOP* (note (@ (id "n1")) (to "Tove")))
;;;   (sxml-select doc "//item/@href")   => ("a.html" "b.html")
;;;   (sxml-find doc "channel/title")    => (title "News"), or #f
;;;   (sxml-text (sxml-find doc "channel/title"))  => "News"
;;;
;;; A node is one of:
;;;
;;;   (name (@ (attr "value") ...) child ...)   an element; the attribute list is optional
;;;   "text"                                    text (numbers and characters are text too)
;;;   (*TOP* node ...)                          a document
;;;   (*PI* target "data")                      a processing instruction, <?target data?>
;;;   (*COMMENT* "text")                        a comment
;;;   (*DECL* DOCTYPE html)                     a declaration, <!DOCTYPE html>
;;;   (*RAW* "<b>x</b>")                        text written as it is, not escaped
;;;
;;; For templates written with quasiquote, a list whose head is not a symbol is spliced into
;;; its parent, and #f and () are left out, so `,@` and `(and test node)` both work. An
;;; attribute whose value is #f is left out; #t (or no value) writes the name as the value.
;;; Names with a namespace prefix (`svg:rect`) are written and read as they are.
;;;
;;; The parser checks that its input is well-formed XML 1.0: one root element, matching tags,
;;; unique attributes, valid names, characters and references. It expands the five predefined
;;; entities, character references and the internal entities of the DOCTYPE (at most a million
;;; characters in all), never external ones, and gives an `&xml-error` with the line and
;;; column of the first problem. The DOCTYPE is not part of the result.

(library (capy sxml)
  (export
    ;; writing
    sxml->xml
    write-xml
    ;; parsing
    xml->sxml
    ;; access
    sxml-element?
    sxml-name
    sxml-attributes
    sxml-attr
    sxml-children
    sxml-elements
    sxml-text
    ;; selection
    sxml-select
    sxml-find
    ;; errors
    &xml-error
    xml-error?
    xml-error-line
    xml-error-column)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define)))

  (define-condition-type &xml-error &error
    make-xml-error xml-error?
    (line xml-error-line)
    (column xml-error-column))

  (define (raise-xml-error who message line column irritants)
    (raise
      (condition
        (make-xml-error line column)
        (make-who-condition who)
        (make-message-condition
          (if line
            (string-append message " at line " (number->string line)
              ", column " (number->string column))
            message))
        (make-irritants-condition irritants))))

  (define (write-error who message . irritants)
    (raise-xml-error who message #f #f irritants))

  (define (xml-char? c)
    (let ([n (char->integer c)])
      (or (fx>=? n #x20)
        (fx=? n #x9) (fx=? n #xA) (fx=? n #xD))))

  (define (name-start-char? c)
    (let ([n (char->integer c)])
      (or (and (fx>=? n 97) (fx<=? n 122))
        (and (fx>=? n 65) (fx<=? n 90))
        (fx=? n 95) (fx=? n 58)
        (and (fx>=? n #xC0) (fx<=? n #xD6))
        (and (fx>=? n #xD8) (fx<=? n #xF6))
        (and (fx>=? n #xF8) (fx<=? n #x2FF))
        (and (fx>=? n #x370) (fx<=? n #x37D))
        (and (fx>=? n #x37F) (fx<=? n #x1FFF))
        (and (fx>=? n #x200C) (fx<=? n #x200D))
        (and (fx>=? n #x2070) (fx<=? n #x218F))
        (and (fx>=? n #x2C00) (fx<=? n #x2FEF))
        (and (fx>=? n #x3001) (fx<=? n #xD7FF))
        (and (fx>=? n #xF900) (fx<=? n #xFDCF))
        (and (fx>=? n #xFDF0) (fx<=? n #xFFFD))
        (and (fx>=? n #x10000) (fx<=? n #xEFFFF)))))

  (define (name-char? c)
    (or (name-start-char? c)
      (let ([n (char->integer c)])
        (or (and (fx>=? n 48) (fx<=? n 57))
          (fx=? n 45) (fx=? n 46) (fx=? n #xB7)
          (and (fx>=? n #x300) (fx<=? n #x36F))
          (and (fx>=? n #x203F) (fx<=? n #x2040))))))

  (define (xml-name? s)
    (let ([n (string-length s)])
      (and (> n 0)
        (name-start-char? (string-ref s 0))
        (let loop ([i 1])
          (or (= i n) (and (name-char? (string-ref s i)) (loop (+ i 1))))))))

  (define (space? c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab) (char=? c #\return)))

  (define special-names '(*TOP* *PI* *COMMENT* *DECL* *RAW* @ @@ *ENTITY* *NAMESPACES*))

  ;; Whether X is an element: a list whose head is a symbol other than the special ones.
  (define (sxml-element? x)
    (and (pair? x) (symbol? (car x)) (not (memq (car x) special-names))))

  ;; The name of the element NODE, or #f when NODE is not one.
  (define (sxml-name node)
    (and (sxml-element? node) (car node)))

  (define (attribute-list? x)
    (and (pair? x) (eq? (car x) '@)))

  ;; The attributes of NODE, as ((name value) ...); () when it has none.
  (define (sxml-attributes node)
    (if (and (pair? node) (pair? (cdr node)) (attribute-list? (cadr node)))
      (cdadr node)
      '()))

  ;; The value of the attribute NAME (a symbol) of NODE, or DEFAULT (#f) when it has none.
  (define (sxml-attr node name [default #f])
    (let ([a (assq name (sxml-attributes node))])
      (cond
        [(not a) default]
        [(null? (cdr a)) (symbol->string name)]
        [else (apply string-append (map text-of (cdr a)))])))

  ;; The contents of NODE after its name and attributes.
  (define (sxml-children node)
    (cond
      [(not (pair? node)) '()]
      [(and (pair? (cdr node)) (attribute-list? (cadr node))) (skip-annotations (cddr node))]
      [else (skip-annotations (cdr node))]))

  (define (skip-annotations l)
    (if (and (pair? l) (pair? (car l)) (eq? (caar l) '@@)) (skip-annotations (cdr l)) l))

  ;; The element children of NODE, all of them or those named NAME.
  (define (sxml-elements node [name #f])
    (filter (lambda (c) (and (sxml-element? c) (or (not name) (eq? (car c) name))))
      (flatten-children (sxml-children node))))

  ;; Children with the spliced lists of templates flattened into place.
  (define (flatten-children l)
    (let loop ([l l] [out '()])
      (cond
        [(null? l) (reverse out)]
        [(and (pair? (car l)) (not (symbol? (caar l)))) (loop (cdr l) (append (reverse (flatten-children (car l))) out))]
        [else (loop (cdr l) (cons (car l) out))])))

  (define (text-of x)
    (cond
      [(string? x) x]
      [(char? x) (string x)]
      [(number? x) (number->text x)]
      [(symbol? x) (symbol->string x)]
      [else ""]))

  (define (number->text x)
    (let ([s (number->string x)])
      (if (and (> (string-length s) 1) (char=? (string-ref s 0) #\+)) (substring s 1 (string-length s)) s)))

  ;; The text of NODE and everything in it, joined: "A & B" for (p "A " (b "&") " B").
  (define (sxml-text node)
    (call-with-string-output-port
      (lambda (out)
        (let walk ([x node])
          (cond
            [(or (string? x) (char? x) (number? x)) (put-string out (text-of x))]
            [(and (pair? x) (memq (car x) '(*COMMENT* *PI* *DECL* @ @@))) #f]
            [(and (pair? x) (eq? (car x) '*RAW*)) (for-each walk (cdr x))]
            [(and (pair? x) (symbol? (car x))) (for-each walk (sxml-children x))]
            [(pair? x) (for-each walk x)]
            [else #f])))))


  ;; A path is a list of steps, or a string of steps separated by '/':
  ;;
  ;;   name       "name"     the element children of that name
  ;;   *          "*"        all element children
  ;;   //         "//"       the nodes and all the elements in them, at any depth, so that
  ;;                         "//a" is every a element of the document
  ;;   (@ name)   "@name"    the values of the attribute
  ;;   *text*     "text()"   the text children
  ;;   a procedure           the nodes it is true of
  ;;
  ;; A path starts from NODE itself: from a `*TOP*`, "html/body" is the body of the root.

  (define (parse-path path)
    (cond
      [(list? path) path]
      [(string? path)
        (let* ([n (string-length path)]
               [segments (let loop ([i 0] [from 0] [out '()])
                           (cond
                             [(= i n) (reverse (cons (substring path from n) out))]
                             [(char=? (string-ref path i) #\/)
                               (loop (+ i 1) (+ i 1) (cons (substring path from i) out))]
                             [else (loop (+ i 1) from out)]))]
               ;; "/a" starts at NODE like "a"; an empty segment elsewhere is "//"
               [segments (if (and (> n 0) (char=? (string-ref path 0) #\/)) (cdr segments) segments)])
          (let loop ([segs segments] [out '()])
            (cond
              [(null? segs) (reverse out)]
              [(string=? (car segs) "")
                (if (and (pair? (cdr segs)) (not (null? out)) (eq? (car out) '//))
                  (loop (cdr segs) out)
                  (loop (cdr segs) (cons '// out)))]
              [(string=? (car segs) "*") (loop (cdr segs) (cons '* out))]
              [(string=? (car segs) "text()") (loop (cdr segs) (cons '*text* out))]
              [(char=? (string-ref (car segs) 0) #\@)
                (loop (cdr segs)
                  (cons (list '@ (string->symbol (substring (car segs) 1 (string-length (car segs))))) out))]
              [else (loop (cdr segs) (cons (string->symbol (car segs)) out))])))]
      [else (assertion-violation 'sxml-select "a path is a list of steps or a string" path)]))

  (define (descendants-or-self node)
    (let loop ([x node] [out '()])
      (fold-left (lambda (out c) (if (sxml-element? c) (loop c out) out))
        (cons x out)
        (flatten-children (sxml-children x)))))

  (define (unique nodes)
    (let ([seen (make-eq-hashtable)])
      (filter (lambda (x)
                (and (not (hashtable-contains? seen x))
                  (begin (hashtable-set! seen x #t) #t)))
        nodes)))

  (define (apply-step step nodes)
    (cond
      [(eq? step '//)
        (unique (apply append (map (lambda (n) (reverse (descendants-or-self n))) nodes)))]
      [(eq? step '*) (apply append (map (lambda (n) (sxml-elements n)) nodes))]
      [(eq? step '*text*)
        (apply append
          (map (lambda (n) (filter string? (flatten-children (sxml-children n)))) nodes))]
      [(symbol? step) (apply append (map (lambda (n) (sxml-elements n step)) nodes))]
      [(and (pair? step) (eq? (car step) '@) (pair? (cdr step)))
        (let ([name (cadr step)])
          (let loop ([ns nodes] [out '()])
            (if (null? ns)
              (reverse out)
              (let ([v (sxml-attr (car ns) name)])
                (loop (cdr ns) (if v (cons v out) out))))))]
      [(procedure? step) (filter step nodes)]
      [else (assertion-violation 'sxml-select "not a path step" step)]))

  ;; The nodes (or attribute values, or strings) that PATH selects from NODE, in document order.
  (define (sxml-select node path)
    (fold-left (lambda (nodes step) (apply-step step nodes)) (list node) (parse-path path)))

  ;; The first of what PATH selects from NODE, or DEFAULT (#f) when it selects nothing.
  (define (sxml-find node path [default #f])
    (let ([found (sxml-select node path)])
      (if (null? found) default (car found))))

  ;; ---- Writing ----

  (define (unspecified? x) (eq? x (if #f #f)))

  ;; What a template leaves out: #f, () and the unspecified value of a one-armed `if`.
  (define (skip? x) (or (not x) (null? x) (unspecified? x)))

  (define (contains? s part)
    (let ([n (string-length s)] [m (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i m) n)
          (or (string=? (substring s i (+ i m)) part) (loop (+ i 1)))))))

  (define (check-chars s who)
    (string-for-each
      (lambda (c)
        (unless (xml-char? c)
          (write-error who "a character that XML cannot hold" c)))
      s))

  ;; Escapes the text S; ATTRIBUTE? also escapes '"' and the whitespace that attribute value
  ;; normalisation would turn into spaces.
  (define (write-escaped s out attribute? who)
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
              [(char=? c #\return) (escape "&#13;")]
              [(and attribute? (char=? c #\")) (escape "&quot;")]
              [(and attribute? (char=? c #\newline)) (escape "&#10;")]
              [(and attribute? (char=? c #\tab)) (escape "&#9;")]
              [(xml-char? c) (loop (+ i 1) from)]
              [else (write-error who "a character that XML cannot hold" c)]))))))

  (define (check-name name who what)
    (let ([s (symbol->string name)])
      (unless (xml-name? s)
        (write-error who (string-append "not a valid XML " what " name") name))
      s))

  (define (write-attributes attrs out who)
    (for-each
      (lambda (a)
        (unless (and (pair? a) (symbol? (car a)))
          (write-error who "an attribute must be (name value)" a))
        (let ([name (check-name (car a) who "attribute")]
              [values (cdr a)])
          (cond
            [(and (pair? values) (null? (cdr values)) (eq? (car values) #f)) #f]
            [(or (null? values) (and (null? (cdr values)) (eq? (car values) #t)))
              (put-char out #\space) (put-string out name)
              (put-string out "=\"") (put-string out name) (put-char out #\")]
            [else
              (put-char out #\space) (put-string out name) (put-string out "=\"")
              (for-each (lambda (v) (write-attribute-value v out who)) values)
              (put-char out #\")])))
      attrs))

  (define (write-attribute-value v out who)
    (cond
      [(string? v) (write-escaped v out #t who)]
      [(or (number? v) (char? v) (symbol? v)) (write-escaped (text-of v) out #t who)]
      [(list? v) (for-each (lambda (x) (write-attribute-value x out who)) v)]
      [else (write-error who "an attribute value must be text" v)]))

  ;; Whether the children of an element are only elements and markup, so that indenting them
  ;; adds no text that matters.
  (define (element-only? children)
    (and (pair? children)
      (for-all (lambda (c) (and (pair? c) (symbol? (car c)) (not (eq? (car c) '*RAW*)))) children)))

  (define (write-sxml-xml node out who indent)
    (define unit (cond
                   [(not indent) #f]
                   [(and (integer? indent) (exact? indent) (>= indent 0)) (make-string indent #\space)]
                   [(string? indent) indent]
                   [else (assertion-violation who "#:indent must be #f, a count of spaces or a string" indent)]))
    (define (newline-indent level)
      (put-char out #\newline)
      (let loop ([k level]) (when (> k 0) (put-string out unit) (loop (- k 1)))))
    (define (write-node x level)
      (cond
        [(string? x) (write-escaped x out #f who)]
        [(or (number? x) (char? x) (symbol? x)) (write-escaped (text-of x) out #f who)]
        [(skip? x) #f]
        [(pair? x)
          (let ([head (car x)])
            (cond
              [(eq? head '*TOP*)
                (let ([children (filter (lambda (c) (not (or (attribute-list? c) (skip? c))))
                                  (flatten-children (skip-annotations (cdr x))))])
                  (let loop ([cs children] [first? #t])
                    (unless (null? cs)
                      (when (and unit (not first?)) (put-char out #\newline))
                      (write-node (car cs) level)
                      (loop (cdr cs) #f))))]
              [(eq? head '*PI*)
                (unless (and (pair? (cdr x)) (symbol? (cadr x)))
                  (write-error who "a processing instruction is (*PI* target \"data\")" x))
                (let ([target (check-name (cadr x) who "processing instruction")]
                      [data (apply string-append (map text-of (cddr x)))])
                  (when (contains? data "?>")
                    (write-error who "a processing instruction cannot contain \"?>\"" data))
                  (check-chars data who)
                  (put-string out "<?") (put-string out target)
                  (unless (string=? data "") (put-char out #\space) (put-string out data))
                  (put-string out "?>"))]
              [(eq? head '*COMMENT*)
                (let ([text (apply string-append (map text-of (cdr x)))])
                  (when (or (contains? text "--")
                          (and (> (string-length text) 0)
                            (char=? (string-ref text (- (string-length text) 1)) #\-)))
                    (write-error who "a comment cannot contain \"--\" or end with \"-\"" text))
                  (check-chars text who)
                  (put-string out "<!--") (put-string out text) (put-string out "-->"))]
              [(eq? head '*DECL*)
                (put-string out "<!")
                (let loop ([parts (cdr x)] [first? #t])
                  (unless (null? parts)
                    (unless first? (put-char out #\space))
                    (let ([p (car parts)])
                      (if (string? p)
                        (begin (put-char out #\") (put-string out p) (put-char out #\"))
                        (put-string out (text-of p))))
                    (loop (cdr parts) #f)))
                (put-char out #\>)]
              [(eq? head '*RAW*) (for-each (lambda (s) (put-string out (text-of s))) (cdr x))]
              [(eq? head '@)
                (write-error who "an attribute list (@ ...) must come right after an element name" x)]
              [(memq head '(@@ *ENTITY* *NAMESPACES*)) #f]
              [(symbol? head)
                (let ([name (check-name head who "element")]
                      [children (filter (lambda (c) (not (skip? c))) (flatten-children (sxml-children x)))])
                  (put-char out #\<) (put-string out name)
                  (write-attributes (sxml-attributes x) out who)
                  (if (null? children)
                    (put-string out "/>")
                    (begin
                      (put-char out #\>)
                      (if (and unit (element-only? children))
                        (begin
                          (for-each (lambda (c) (newline-indent (+ level 1)) (write-node c (+ level 1)))
                            children)
                          (newline-indent level))
                        (for-each (lambda (c) (write-node c (+ level 1))) children))
                      (put-string out "</") (put-string out name) (put-char out #\>))))]
              [else (for-each (lambda (c) (write-node c level)) x)]))]
        [else (write-error who "this value cannot be written as XML" x)]))
    (write-node node 0))

  (define (has-declaration? node)
    (and (pair? node) (eq? (car node) '*TOP*)
      (let ([first (find (lambda (c) (not (attribute-list? c))) (cdr node))])
        (and (pair? first) (eq? (car first) '*PI*) (pair? (cdr first)) (eq? (cadr first) 'xml)))))

  ;; Writes NODE as XML to PORT. #:declaration? starts with <?xml version="1.0"
  ;; encoding="UTF-8"?> (unless NODE is a *TOP* that has one); #:indent (spaces or a string)
  ;; puts the children of elements that hold no text on lines of their own.
  (define (write-xml node [port (current-output-port)]
            #:declaration? [declaration? #f]
            #:indent [indent #f])
    (when (and declaration? (not (has-declaration? node)))
      (put-string port "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      (put-char port #\newline))
    (write-sxml-xml node port 'write-xml indent))

  ;; NODE as an XML string; the keywords are those of `write-xml`.
  (define (sxml->xml node #:declaration? [declaration? #f] #:indent [indent #f])
    (call-with-string-output-port
      (lambda (out)
        (when (and declaration? (not (has-declaration? node)))
          (put-string out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
          (put-char out #\newline))
        (write-sxml-xml node out 'sxml->xml indent))))


  ;; The text with line ends normalised to #\newline (XML 1.0, 2.11).
  (define (normalize-line-ends s)
    (if (not (string-index s #\return))
      s
      (call-with-string-output-port
        (lambda (out)
          (let ([n (string-length s)])
            (let loop ([i 0])
              (when (< i n)
                (let ([c (string-ref s i)])
                  (if (char=? c #\return)
                    (begin
                      (put-char out #\newline)
                      (loop (if (and (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\newline))
                              (+ i 2)
                              (+ i 1))))
                    (begin (put-char out c) (loop (+ i 1))))))))))))

  (define (string-index s c)
    (let ([n (string-length s)])
      (let loop ([i 0])
        (cond [(= i n) #f] [(char=? (string-ref s i) c) i] [else (loop (+ i 1))]))))

  (define max-expansion 1000000)

  ;; The SXML of the XML document INPUT: a string, a textual input port (read to its end) or a
  ;; UTF-8 bytevector. #:trim-whitespace? leaves out the text nodes that are only whitespace;
  ;; #:comments? keeps comments as (*COMMENT* "..."); #:entities is an alist of more entities,
  ;; ((name . "replacement text") ...), that the document may refer to.
  (define (xml->sxml input
            #:trim-whitespace? [trim? #f]
            #:comments? [comments? #f]
            #:entities [extra-entities '()])
    (let* ([text (cond
                   [(string? input) input]
                   [(bytevector? input) (utf8->string input)]
                   [(and (input-port? input) (textual-port? input)) (get-string-all input)]
                   [else (assertion-violation 'xml->sxml "not a string, a textual port or a bytevector" input)])]
           [text (if (eof-object? text) "" text)]
           [text (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\xFEFF))
                   (substring text 1 (string-length text))
                   text)])
      (parse-document (normalize-line-ends text) trim? comments?
        (map (lambda (e) (cons (if (symbol? (car e)) (symbol->string (car e)) (car e)) (cdr e)))
          extra-entities))))

  (define (parse-document s trim? comments? extra-entities)
    (define n (string-length s))
    (define i 0)
    ;; name -> replacement text; #f for an external entity
    (define entities (make-hashtable string-hash string=?))
    (define expanded 0)

    (define (locate pos)
      (let loop ([k 0] [line 1] [column 1])
        (cond
          [(= k pos) (values line column)]
          [(char=? (string-ref s k) #\newline) (loop (+ k 1) (+ line 1) 1)]
          [else (loop (+ k 1) line (+ column 1))])))
    (define (fail-at pos message . irritants)
      (let-values ([(line column) (locate (min pos n))])
        (raise-xml-error 'xml->sxml message line column irritants)))
    (define (fail message . irritants) (apply fail-at i message irritants))

    (define (peek) (if (< i n) (string-ref s i) #f))
    (define (at? word)
      (let ([m (string-length word)])
        (and (<= (+ i m) n)
          (let loop ([k 0])
            (or (= k m) (and (char=? (string-ref s (+ i k)) (string-ref word k)) (loop (+ k 1))))))))
    (define (expect! word what)
      (if (at? word) (set! i (+ i (string-length word))) (fail (string-append "expected " what))))
    (define (skip-space!)
      (let loop () (when (and (< i n) (space? (string-ref s i))) (set! i (+ i 1)) (loop))))
    (define (require-space! what)
      (unless (and (< i n) (space? (string-ref s i))) (fail (string-append "expected whitespace " what)))
      (skip-space!))
    ;; the index of WORD from I, or #f
    (define (search word)
      (let ([m (string-length word)])
        (let loop ([k i])
          (cond
            [(> (+ k m) n) #f]
            [(and (char=? (string-ref s k) (string-ref word 0))
               (string=? (substring s k (+ k m)) word)) k]
            [else (loop (+ k 1))]))))

    (define (check-chars-between from to)
      (let loop ([k from])
        (when (< k to)
          (unless (xml-char? (string-ref s k))
            (fail-at k (string-append "character U+"
                         (number->string (char->integer (string-ref s k)) 16)
                         " is not allowed in XML")))
          (loop (+ k 1)))))

    (define (read-name what)
      (let ([start i])
        (unless (and (< i n) (name-start-char? (string-ref s i)))
          (fail (string-append "expected " what)))
        (set! i (+ i 1))
        (let loop () (when (and (< i n) (name-char? (string-ref s i))) (set! i (+ i 1)) (loop)))
        (substring s start i)))

    ;; A character reference after "&#"; I is after the '#'.
    (define (read-char-ref start)
      (let ([semi (let find ([k i]) (cond [(= k n) #f] [(char=? (string-ref s k) #\;) k]
                                      [(> (- k i) 10) #f] [else (find (+ k 1))]))])
        (unless semi (fail-at start "malformed character reference"))
        (let ([c (char-ref-value (string-append "#" (substring s i semi)) start)])
          (set! i (+ semi 1))
          c)))

    ;; The character of the reference "#123" or "#x1F" (the text between '&' and ';').
    (define (char-ref-value ref start)
      (let* ([hex? (and (> (string-length ref) 1) (char=? (string-ref ref 1) #\x))]
             [digits (substring ref (if hex? 2 1) (string-length ref))]
             [ok? (and (> (string-length digits) 0)
                    (for-all (lambda (c)
                               (or (char<=? #\0 c #\9)
                                 (and hex? (or (char<=? #\a c #\f) (char<=? #\A c #\F)))))
                      (string->list digits)))]
             [code (and ok? (string->number digits (if hex? 16 10)))])
        (unless code
          (fail-at start (string-append "malformed character reference '&" ref ";'")))
        (unless (or (<= #x20 code #xD7FF) (memv code '(#x9 #xA #xD))
                  (<= #xE000 code #xFFFD) (<= #x10000 code #x10FFFF))
          (fail-at start (string-append "'&" ref ";' refers to a character XML does not allow")))
        (integer->char code)))

    (define (predefined? name) (member name '("lt" "gt" "amp" "apos" "quot")))

    ;; The replacement text of the entity NAME, referred to at START.
    (define (entity-text name start)
      (cond
        [(string=? name "lt") "<"]
        [(string=? name "gt") ">"]
        [(string=? name "amp") "&"]
        [(string=? name "apos") "'"]
        [(string=? name "quot") "\""]
        [(assoc name extra-entities) => cdr]
        [(hashtable-contains? entities name)
          (or (hashtable-ref entities name #f)
            (fail-at start (string-append "the external entity '" name "' is not loaded")))]
        [else (fail-at start (string-append "undefined entity '&" name ";'"))]))

    (define (count-expansion! text start)
      (set! expanded (+ expanded (string-length text)))
      (when (> expanded max-expansion)
        (fail-at start "entity expansion is too large")))

    ;; ---- Attribute values ----

    ;; The value of a quoted attribute; I is at the quote.
    (define (read-attribute-value)
      (let ([q (peek)])
        (unless (memv q '(#\" #\'))
          (fail "expected a quoted attribute value"))
        (set! i (+ i 1))
        (call-with-string-output-port
          (lambda (out)
            (let loop ()
              (let ([c (peek)])
                (cond
                  [(not c) (fail "unterminated attribute value")]
                  [(char=? c q) (set! i (+ i 1))]
                  [(char=? c #\<) (fail "'<' is not allowed in an attribute value")]
                  [(char=? c #\&)
                    (let ([start i])
                      (set! i (+ i 1))
                      (if (eqv? (peek) #\#)
                        (begin (set! i (+ i 1)) (put-char out (read-char-ref start)))
                        (let ([name (read-name "an entity name after '&'")])
                          (unless (eqv? (peek) #\;) (fail "expected ';' after an entity name"))
                          (set! i (+ i 1))
                          (put-entity-in-attribute! name out '() start))))
                    (loop)]
                  [(space? c) (put-char out #\space) (set! i (+ i 1)) (loop)]
                  [(xml-char? c) (put-char out c) (set! i (+ i 1)) (loop)]
                  [else (fail "a character XML does not allow")])))))))

    ;; Puts the replacement text of the entity NAME, referred to at START in an attribute
    ;; value, with the references in it expanded. ACTIVE are the entities being expanded.
    (define (put-entity-in-attribute! name out active start)
      (when (member name active)
        (fail-at start (string-append "entity '" name "' refers to itself")))
      (let ([text (entity-text name start)])
        (if (predefined? name)
          (put-string out text)
          (let ([m (string-length text)])
            (count-expansion! text start)
            (let loop ([k 0])
              (when (< k m)
                (let ([c (string-ref text k)])
                  (cond
                    [(char=? c #\<)
                      (fail-at start (string-append "the entity '" name "' puts '<' in an attribute value"))]
                    [(char=? c #\&)
                      (let ([semi (let find ([j k]) (cond [(= j m) #f] [(char=? (string-ref text j) #\;) j] [else (find (+ j 1))]))])
                        (unless semi
                          (fail-at start (string-append "malformed reference in the entity '" name "'")))
                        (let ([ref (substring text (+ k 1) semi)])
                          (if (and (> (string-length ref) 0) (char=? (string-ref ref 0) #\#))
                            (put-char out (char-ref-value ref start))
                            (begin
                              (unless (xml-name? ref)
                                (fail-at start (string-append "malformed reference in the entity '" name "'")))
                              (put-entity-in-attribute! ref out (cons name active) start))))
                        (loop (+ semi 1)))]
                    [(space? c) (put-char out #\space) (loop (+ k 1))]
                    [else (put-char out c) (loop (+ k 1))]))))))))

    ;; ---- Content ----

    ;; Reads content up to the end tag of NAME (or to the end of the input when NAME is #f,
    ;; for the replacement text of an entity) and returns the nodes, in order.
    (define (read-content name active)
      (let loop ([nodes '()] [text '()])
        (define (flush)
          (if (null? text)
            nodes
            (let ([t (apply string-append (reverse text))])
              (if (and trim? (string-only-space? t)) nodes (cons t nodes)))))
        (let ([c (peek)])
          (cond
            [(not c)
              (if name
                (fail (string-append "end of input inside <" name ">"))
                (reverse (flush)))]
            [(char=? c #\<)
              (cond
                [(at? "</")
                  (if name
                    (let ([start i])
                      (set! i (+ i 2))
                      (let ([end-name (read-name "an element name after '</'")])
                        (unless (string=? end-name name)
                          (fail-at start (string-append "</" end-name "> does not match <" name ">")))
                        (skip-space!)
                        (expect! ">" "'>' to end the end tag")
                        (reverse (flush))))
                    (fail "an end tag in the replacement text of an entity"))]
                [(at? "<!--")
                  (let ([comment (read-comment)])
                    (if comments?
                      (loop (cons comment (flush)) '())
                      (loop nodes text)))]
                [(at? "<![CDATA[")
                  (set! i (+ i 9))
                  (let ([end (search "]]>")])
                    (unless end (fail "unterminated CDATA section"))
                    (check-chars-between i end)
                    (let ([t (substring s i end)])
                      (set! i (+ end 3))
                      (loop nodes (cons t text))))]
                [(at? "<?") (loop (cons (read-pi) (flush)) '())]
                [(at? "<!") (fail "a declaration is not allowed inside an element")]
                [else (loop (cons (read-element active) (flush)) '())])]
            [(char=? c #\&)
              (let ([start i])
                (set! i (+ i 1))
                (if (eqv? (peek) #\#)
                  (begin (set! i (+ i 1)) (loop nodes (cons (string (read-char-ref start)) text)))
                  (let ([ename (read-name "an entity name after '&'")])
                    (unless (eqv? (peek) #\;) (fail "expected ';' after an entity name"))
                    (set! i (+ i 1))
                    (when (member ename active)
                      (fail-at start (string-append "entity '" ename "' refers to itself")))
                    (let ([replacement (entity-text ename start)])
                      (unless (predefined? ename) (count-expansion! replacement start))
                      (if (or (predefined? ename)
                            (not (or (string-index replacement #\<) (string-index replacement #\&))))
                        (loop nodes (cons replacement text))
                        (expand-entity-content replacement ename active start nodes text loop))))))]
            [else
              (let ([start i])
                (let scan ()
                  (when (and (< i n) (not (memv (string-ref s i) '(#\< #\&))))
                    (set! i (+ i 1))
                    (scan)))
                (let ([t (substring s start i)])
                  (let ([bad (and (string-index t #\]) (search-in t "]]>"))])
                    (when bad (fail-at (+ start bad) "\"]]>\" is not allowed in text")))
                  (check-chars-between start i)
                  (loop nodes (cons t text))))]))))

    ;; The replacement text of an entity, read as content: text joins the text around it.
    (define (expand-entity-content replacement ename active start nodes text loop)
      (let ([saved-s s] [saved-n n] [saved-i i])
        (set! s replacement) (set! n (string-length replacement)) (set! i 0)
        (let ([inner (guard (e [(xml-error? e)
                                (set! s saved-s) (set! n saved-n) (set! i saved-i)
                                (fail-at start (string-append "in the entity '" ename "': "
                                                 (condition-message e)))])
                       (read-content #f (cons ename active)))])
          (set! s saved-s) (set! n saved-n) (set! i saved-i)
          ;; leading text of INNER joins TEXT; the rest are nodes
          (let merge ([inner inner] [nodes nodes] [text text])
            (cond
              [(null? inner) (loop nodes text)]
              [(string? (car inner)) (merge (cdr inner) nodes (cons (car inner) text))]
              [else
                (let ([flushed (if (null? text)
                                 nodes
                                 (let ([t (apply string-append (reverse text))])
                                   (if (and trim? (string-only-space? t)) nodes (cons t nodes))))])
                  (merge (cdr inner) (cons (car inner) flushed) '()))])))))

    (define (search-in t word)
      (let ([m (string-length word)] [k (string-length t)])
        (let loop ([j 0])
          (cond
            [(> (+ j m) k) #f]
            [(string=? (substring t j (+ j m)) word) j]
            [else (loop (+ j 1))]))))

    (define (read-comment)
      (let ([start i])
        (set! i (+ i 4))
        (let ([end (search "--")])
          (unless end (fail-at start "unterminated comment"))
          (unless (and (< (+ end 2) n) (char=? (string-ref s (+ end 2)) #\>))
            (fail-at end "\"--\" is not allowed inside a comment"))
          (check-chars-between i end)
          (let ([text (substring s i end)])
            (set! i (+ end 3))
            (list '*COMMENT* text)))))

    (define (read-pi)
      (let ([start i])
        (set! i (+ i 2))
        (let ([target (read-name "a processing instruction target after '<?'")])
          (when (string-ci=? target "xml")
            (fail-at start "the XML declaration must come first, and the target 'xml' is reserved"))
          (let ([data (cond
                        [(at? "?>") ""]
                        [else
                          (require-space! "after the target of a processing instruction")
                          (let ([end (search "?>")])
                            (unless end (fail-at start "unterminated processing instruction"))
                            (check-chars-between i end)
                            (let ([d (substring s i end)]) (set! i end) d))])])
            (set! i (+ i 2))
            (list '*PI* (string->symbol target) data)))))

    (define (read-element active)
      (let ([start i])
        (set! i (+ i 1))
        (let ([name (read-name "an element name after '<'")])
          (let loop ([attributes '()])
            (let ([had-space? (and (< i n) (space? (string-ref s i)))])
              (skip-space!)
              (cond
                [(at? "/>")
                  (set! i (+ i 2))
                  (make-element name (reverse attributes) '())]
                [(at? ">")
                  (set! i (+ i 1))
                  (make-element name (reverse attributes) (read-content name active))]
                [(not (peek)) (fail-at start (string-append "unterminated start tag <" name))]
                [else
                  (unless had-space? (fail "expected whitespace before an attribute"))
                  (let* ([attr-start i]
                         [attr (read-name "an attribute name, '>' or '/>'")])
                    (when (assq (string->symbol attr) attributes)
                      (fail-at attr-start (string-append "duplicate attribute '" attr "'")))
                    (skip-space!)
                    (expect! "=" (string-append "'=' after the attribute name '" attr "'"))
                    (skip-space!)
                    (let ([value (read-attribute-value)])
                      (loop (cons (list (string->symbol attr) value) attributes))))]))))))

    (define (make-element name attributes children)
      (let ([tag (string->symbol name)])
        (if (null? attributes)
          (cons tag children)
          (cons* tag (cons '@ attributes) children))))

    ;; ---- Prolog ----

    (define (read-xml-declaration)
      ;; I is at "<?xml" followed by whitespace
      (let ([start i])
        (set! i (+ i 5))
        (let ([end (search "?>")])
          (unless end (fail-at start "unterminated XML declaration"))
          (let ([data (let trim ([d (substring s i end)])
                        (if (and (> (string-length d) 0) (space? (string-ref d 0)))
                          (trim (substring d 1 (string-length d)))
                          d))])
            (unless (and (>= (string-length data) 7) (string=? (substring data 0 7) "version"))
              (fail-at start "the XML declaration must start with the version"))
            (set! i (+ end 2))
            (list '*PI* 'xml (let rtrim ([d data])
                               (let ([k (string-length d)])
                                 (if (and (> k 0) (space? (string-ref d (- k 1))))
                                   (rtrim (substring d 0 (- k 1)))
                                   d))))))))

    (define (skip-quoted!)
      (let ([q (peek)])
        (set! i (+ i 1))
        (let ([end (let find ([k i]) (cond [(= k n) #f] [(char=? (string-ref s k) q) k] [else (find (+ k 1))]))])
          (unless end (fail "unterminated quoted string in the DOCTYPE"))
          (let ([text (substring s i end)])
            (set! i (+ end 1))
            text))))

    ;; The literal value of an entity declaration, with its character references expanded.
    (define (read-entity-value)
      (let ([q (peek)])
        (unless (memv q '(#\" #\')) (fail "expected the quoted value of the entity"))
        (set! i (+ i 1))
        (call-with-string-output-port
          (lambda (out)
            (let loop ()
              (let ([c (peek)])
                (cond
                  [(not c) (fail "unterminated entity value")]
                  [(char=? c q) (set! i (+ i 1))]
                  [(and (char=? c #\&) (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\#))
                    (let ([start i])
                      (set! i (+ i 2))
                      (put-char out (read-char-ref start))
                      (loop))]
                  [(char=? c #\%) (fail "parameter entity references are not supported")]
                  [else (put-char out c) (set! i (+ i 1)) (loop)])))))))

    (define (read-entity-declaration)
      ;; I is after "<!ENTITY"
      (require-space! "after <!ENTITY")
      (if (eqv? (peek) #\%)
        ;; a parameter entity: skipped
        (skip-declaration!)
        (let ([name (read-name "an entity name")])
          (require-space! "after the entity name")
          (cond
            [(memv (peek) '(#\" #\'))
              (let ([value (read-entity-value)])
                (unless (hashtable-contains? entities name)
                  (hashtable-set! entities name value))
                (skip-space!)
                (expect! ">" "'>' to end the entity declaration"))]
            [else
              (unless (hashtable-contains? entities name)
                (hashtable-set! entities name #f))
              (skip-declaration!)]))))

    ;; Skips to the '>' that ends a markup declaration, over quoted strings.
    (define (skip-declaration!)
      (let loop ()
        (let ([c (peek)])
          (cond
            [(not c) (fail "unterminated declaration in the DOCTYPE")]
            [(char=? c #\>) (set! i (+ i 1))]
            [(memv c '(#\" #\')) (skip-quoted!) (loop)]
            [else (set! i (+ i 1)) (loop)]))))

    (define (read-doctype)
      ;; I is at "<!DOCTYPE"
      (set! i (+ i 9))
      (require-space! "after <!DOCTYPE")
      (read-name "the name of the root element in the DOCTYPE")
      (let loop ()
        (skip-space!)
        (let ([c (peek)])
          (cond
            [(not c) (fail "unterminated DOCTYPE")]
            [(char=? c #\>) (set! i (+ i 1))]
            [(memv c '(#\" #\')) (skip-quoted!) (loop)]
            [(char=? c #\[) (set! i (+ i 1)) (read-internal-subset) (loop)]
            [(name-start-char? c) (read-name "SYSTEM or PUBLIC") (loop)]
            [else (fail "unexpected character in the DOCTYPE")]))))

    (define (read-internal-subset)
      (let loop ()
        (skip-space!)
        (cond
          [(not (peek)) (fail "unterminated internal subset of the DOCTYPE")]
          [(eqv? (peek) #\]) (set! i (+ i 1))]
          [(at? "<!ENTITY") (set! i (+ i 8)) (read-entity-declaration) (loop)]
          [(at? "<!--") (read-comment) (loop)]
          [(at? "<?") (read-pi) (loop)]
          [(at? "<!") (set! i (+ i 2)) (skip-declaration!) (loop)]
          [(eqv? (peek) #\%)
            (set! i (+ i 1))
            (read-name "a parameter entity name")
            (expect! ";" "';' after a parameter entity name")
            (loop)]
          [else (fail "unexpected character in the internal subset of the DOCTYPE")])))

    ;; Comments, PIs and whitespace, before or after the root element.
    (define (read-misc nodes)
      (skip-space!)
      (cond
        [(at? "<!--")
          (let ([c (read-comment)]) (read-misc (if comments? (cons c nodes) nodes)))]
        [(at? "<?") (read-misc (cons (read-pi) nodes))]
        [else nodes]))

    (define (string-only-space? t)
      (let loop ([k 0])
        (or (= k (string-length t)) (and (space? (string-ref t k)) (loop (+ k 1))))))

    (let* ([nodes (if (and (at? "<?xml") (< (+ i 5) n) (space? (string-ref s (+ i 5))))
                    (list (read-xml-declaration))
                    '())]
           [nodes (read-misc nodes)]
           [nodes (if (at? "<!DOCTYPE") (begin (read-doctype) (read-misc nodes)) nodes)])
      (cond
        [(not (peek)) (fail "no root element")]
        [(not (eqv? (peek) #\<)) (fail "expected the root element")]
        [(at? "<!") (fail "expected the root element")])
      (let* ([root (read-element '())]
             [nodes (read-misc (cons root nodes))])
        (when (< i n)
          (fail (if (eqv? (peek) #\<)
                  "only one root element is allowed"
                  "text is not allowed after the root element")))
        (cons '*TOP* (reverse nodes)))))
  )
