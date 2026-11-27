(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy html))

(test-begin "capy html")

(define (error-message thunk)
  (guard (e [(message-condition? e) (condition-message e)] [#t 'other])
    (thunk)
    #f))

(define (contains? s part)
  (let ([n (string-length s)] [m (string-length part)])
    (let loop ([i 0])
      (and (<= (+ i m) n)
        (or (string=? (substring s i (+ i m)) part) (loop (+ i 1)))))))

(test-group "elements"
  (test-equal "element" "<p>hi</p>" (sxml->html '(p "hi")))
  (test-equal "empty non-void element" "<div></div>" (sxml->html '(div)))
  (test-equal "nested" "<ul><li>a</li><li>b</li></ul>" (sxml->html '(ul (li "a") (li "b"))))
  (test-equal "doctype?" "<!DOCTYPE html>\n<html><body></body></html>" (sxml->html '(html (body)) #:doctype? #t))
  (test-equal "*TOP*" "<p>a</p><p>b</p>" (sxml->html '(*TOP* (p "a") (p "b"))))
  (test-equal "*DECL*" "<!DOCTYPE html><html></html>" (sxml->html '(*TOP* (*DECL* DOCTYPE html) (html))))
  (test-equal "custom element and SVG names" "<my-widget><svg:rect></svg:rect></my-widget>"
    (sxml->html '(my-widget (svg:rect))))
  (test-equal "write-html" "<b>x</b>" (call-with-string-output-port (lambda (out) (write-html '(b "x") out)))))

(test-group "void elements"
  (test-equal "br" "<p>a<br>b</p>" (sxml->html '(p "a" (br) "b")))
  (test-equal "img with attributes" "<img src=\"a.png\" alt=\"\">" (sxml->html '(img (@ (src "a.png") (alt "")))))
  (test-equal "all of them"
    "<area><base><br><col><embed><hr><img><input><link><meta><source><track><wbr>"
    (sxml->html '(*TOP* (area) (base) (br) (col) (embed) (hr) (img) (input) (link) (meta) (source) (track) (wbr))))
  (test-assert "html-void-element?" (and (html-void-element? 'br) (not (html-void-element? 'div))))
  (test-assert "a void element with children"
    (contains? (error-message (lambda () (sxml->html '(br "x")))) "void element"))
  (test-equal "left-out children are fine" "<br>" (sxml->html '(br #f ()))))

(test-group "escaping"
  (test-equal "text" "<p>a &amp; b &lt;c&gt; \"q\" 'a'</p>" (sxml->html '(p "a & b <c> \"q\" 'a'")))
  (test-equal "attribute" "<a title=\"&quot;x&quot; &amp; &lt;y&gt; 'z'\"></a>"
    (sxml->html '(a (@ (title "\"x\" & <y> 'z'")))))
  (test-equal "html-escape" "&lt;a href=&quot;x&quot;&gt;&amp;" (html-escape "<a href=\"x\">&"))
  (test-equal "numbers and characters" "<p>1 2.5 c 0.0</p>" (sxml->html '(p 1 " " 2.5 " " #\c " " 0.0)))
  (test-equal "non-ASCII is written as is" "<p>λ😀</p>" (sxml->html '(p "λ😀")))
  (test-equal "*RAW*" "<div><b>raw</b></div>" (sxml->html '(div (*RAW* "<b>raw</b>"))))
  (test-equal "*raw*" "<div>&nbsp;</div>" (sxml->html '(div (*raw* "&nbsp;"))))
  (test-equal "comment" "<!--note-->" (sxml->html '(*COMMENT* "note")))
  (test-equal "lower-case comment" "<!-- note -->" (sxml->html '(*comment* " note ")))
  (test-assert "comment that ends early" (error-message (lambda () (sxml->html '(*COMMENT* "a-->b"))))))

(test-group "raw text elements"
  (test-equal "script" "<script>if (a < b && c) { x = \"</\" + \"p>\"; }</script>"
    (sxml->html '(script "if (a < b && c) { x = \"</\" + \"p>\"; }")))
  (test-equal "style" "<style>a > b { content: \"&\"; }</style>" (sxml->html '(style "a > b { content: \"&\"; }")))
  (test-equal "script with attributes" "<script src=\"a.js\" defer></script>" (sxml->html '(script (@ (src "a.js") (defer)))))
  (test-assert "script that would end early"
    (error-message (lambda () (sxml->html '(script "x = '</SCRIPT>'")))))
  (test-assert "an element in a script" (error-message (lambda () (sxml->html '(script (b "x"))))))
  (test-equal "title and textarea are escaped" "<title>a &lt; b</title><textarea>&lt;/textarea&gt;</textarea>"
    (sxml->html '(*TOP* (title "a < b") (textarea "</textarea>")))))

(test-group "attributes"
  (test-equal "boolean true" "<input checked>" (sxml->html '(input (@ (checked #t)))))
  (test-equal "boolean false" "<input>" (sxml->html '(input (@ (checked #f)))))
  (test-equal "no value" "<input disabled>" (sxml->html '(input (@ (disabled)))))
  (test-equal "number and symbol" "<td colspan=\"2\" align=\"left\"></td>" (sxml->html '(td (@ (colspan 2) (align left)))))
  (test-equal "list joined with spaces" "<p class=\"btn primary\"></p>" (sxml->html '(p (@ (class ("btn" #f "primary"))))))
  (test-equal "several values are concatenated" "<a href=\"/a/b\"></a>" (sxml->html '(a (@ (href "/a" "/b")))))
  (test-equal "empty attribute list" "<p></p>" (sxml->html '(p (@))))
  (test-equal "data and aria attributes" "<div data-x=\"1\" aria-label=\"l\"></div>"
    (sxml->html '(div (@ (data-x "1") (aria-label "l")))))
  (test-assert "an invalid attribute name" (error-message (lambda () (sxml->html '(p (@ (|a b| "x")))))))
  (test-assert "an invalid tag name" (error-message (lambda () (sxml->html '(|<p>| "x")))))
  (test-assert "misplaced attributes" (contains? (error-message (lambda () (sxml->html '(p "x" (@ (a "b")))))) "right after")))

(test-group "templates"
  (define items '("one" "two <2>"))
  (define-template (page title . body)
    (html (head (title ,title)) (body ,@body)))
  (define-template (link href text #:new-tab? [new-tab? #f])
    (a (@ (href ,href) (target ,(and new-tab? "_blank"))) ,text))
  (define (card attrs . children)
    `(div (@ (class "card") ,@attrs) ,@children))
  (test-equal "quasiquote with ,@" "<ul><li>one</li><li>two &lt;2&gt;</li></ul>"
    (sxml->html `(ul ,@(map (lambda (x) `(li ,x)) items))))
  (test-equal "a list is spliced without ,@" "<ul><li>one</li><li>two &lt;2&gt;</li></ul>"
    (sxml->html `(ul ,(map (lambda (x) `(li ,x)) items))))
  (test-equal "left-out values" "<p>a</p>" (sxml->html `(p ,(and #f '(b "x")) "a" ,(if #f #f) ())))
  (test-equal "conditional attribute" "<option value=\"a\" selected></option>"
    (sxml->html `(option (@ (value "a") (selected ,(string=? "a" "a"))))))
  (test-equal "define-template" "<html><head><title>T</title></head><body><p>x</p><p>y</p></body></html>"
    (sxml->html (page "T" '(p "x") '(p "y"))))
  (test-equal "define-template with a keyword" "<a href=\"/x\" target=\"_blank\">go</a><a href=\"/y\">stay</a>"
    (sxml->html `(*TOP* ,(link "/x" "go" #:new-tab? #t) ,(link "/y" "stay"))))
  (test-equal "a component" "<main><div class=\"card\" id=\"c\"><p>body</p></div></main>"
    (sxml->html `(main (,card (@ (id "c")) (p "body")))))
  (test-equal "a component without attributes" "<div class=\"card\">x</div>"
    (sxml->html `(,card "x")))
  (test-equal "components nest" "<div class=\"card\"><div class=\"card\">in</div></div>"
    (sxml->html `(,card (,card "in")))))

(test-end)
