(import (rnrs)
  (srfi 64)
  (capy uri))

(test-begin "capy uri")

(define (parts s)
  (let ([u (string->uri s)])
    (list (uri-scheme u) (uri-authority u) (uri-path u) (uri-query u) (uri-fragment u))))

(test-group "parse and unparse"
  (test-equal "full" '("http" "u@h:80" "/a/b" "q=1&r=2" "frag") (parts "http://u@h:80/a/b?q=1&r=2#frag"))
  (test-equal "empty authority" '("file" "" "/a/b" #f #f) (parts "file:///a/b"))
  (test-equal "no authority" '("file" #f "/a/b" #f #f) (parts "file:/a/b"))
  (test-equal "opaque" '("mailto" #f "me@example.com" #f #f) (parts "mailto:me@example.com"))
  (test-equal "relative reference" '(#f #f "a/b" "q" #f) (parts "a/b?q"))
  (test-equal "empty query is not absent" '("x" "h" "/" "" #f) (parts "x://h/?"))
  (test-equal "untitled scheme" '("untitled" #f "Untitled-1" #f #f) (parts "untitled:Untitled-1"))
  (test-equal "colon in a path is not a scheme" '(#f #f "1a:b" #f #f) (parts "1a:b"))
  (for-each
    (lambda (s) (test-equal s s (uri->string (string->uri s))))
    '("http://u@h:80/a/b?q=1&r=2#frag" "file:///a/b%20c" "file:/a" "mailto:x@y" "a/b?q" "x://h/?" "//h/p" "#f" ""))
  (test-equal "make-uri" "https://example.com/p?x#y"
    (uri->string (make-uri "https" "example.com" "/p" "x" "y"))))

(test-group "percent encoding"
  (test-equal "unreserved stay" "aZ09-._~" (percent-encode "aZ09-._~"))
  (test-equal "space and specials" "a%20b%23%3F%25" (percent-encode "a b#?%"))
  (test-equal "slash by default" "a%2Fb" (percent-encode "a/b"))
  (test-equal "kept characters" "a/b%20c" (percent-encode "a/b c" "/"))
  (test-equal "UTF-8" "%CE%BB%F0%9F%98%80" (percent-encode "\x3bb;\x1F600;"))
  (test-equal "decode" "a b#?%" (percent-decode "a%20b%23%3f%25"))
  (test-equal "decode UTF-8" "\x3bb;\x1F600;" (percent-decode "%CE%BB%F0%9F%98%80"))
  (test-equal "malformed escapes stay" "100%zz%4" (percent-decode "100%zz%4"))
  (test-equal "round trip" "d\xE9;j\xE0; vu/\x65E5;\x672C;.scm"
    (percent-decode (percent-encode "d\xE9;j\xE0; vu/\x65E5;\x672C;.scm"))))

(test-group "file URIs"
  (test-equal "simple" "file:///home/me/a.scm" (path->file-uri "/home/me/a.scm"))
  (test-equal "spaces" "file:///home/me/a%20b.scm" (path->file-uri "/home/me/a b.scm"))
  (test-equal "special characters" "file:///t/%23x%3F%25" (path->file-uri "/t/#x?%"))
  (test-equal "unicode" "file:///t/%CE%BB" (path->file-uri "/t/\x3bb;"))
  (test-equal "root" "file:///" (path->file-uri "/"))
  (test-equal "windows drive" "file:///C:/Users/me/a%20b.scm" (path->file-uri "C:\\Users\\me\\a b.scm"))
  (test-equal "windows drive, forward slashes" "file:///c:/x" (path->file-uri "c:/x"))
  (test-equal "relative path is rejected" 'error
    (guard (e [#t 'error]) (path->file-uri "a/b")))
  (test-equal "to path" "/home/me/a b.scm" (file-uri->path "file:///home/me/a%20b.scm"))
  (test-equal "to path, localhost" "/a" (file-uri->path "file://localhost/a"))
  (test-equal "to path, no authority" "/a" (file-uri->path "file:/a"))
  (test-equal "to path, unicode" "/t/\x3bb;" (file-uri->path "file:///t/%CE%BB"))
  (test-equal "to path, drive" "C:/Users/me" (file-uri->path "file:///C:/Users/me"))
  (test-equal "to path, encoded colon" "c:/x" (file-uri->path "file:///c%3A/x"))
  (test-equal "query and fragment are not part of the path" "/a" (file-uri->path "file:///a?q#f"))
  (test-equal "other scheme" #f (file-uri->path "http://h/a"))
  (test-equal "remote host" #f (file-uri->path "file://server/share/a"))
  (test-assert "file-uri?" (file-uri? "file:///a"))
  (test-assert "file-uri? upper case" (file-uri? "FILE:///a"))
  (test-assert "not file-uri?" (not (file-uri? "untitled:x")))
  (for-each
    (lambda (p) (test-equal p p (file-uri->path (path->file-uri p))))
    '("/" "/a/b/c.scm" "/with space/\x65E5;\x672C;/#?%.scm" "/x/\x1F600;" "C:/dir/f.scm")))

(test-end)
