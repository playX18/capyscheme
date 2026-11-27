;;; URIs (RFC 3986) and `file://` URIs as the language server protocol uses them.
;;;
;;;   (uri->string (string->uri "http://u@h:80/a/b?q=1#f"))  => same string
;;;   (uri-scheme (string->uri "file:///a/b"))               => "file"
;;;   (path->file-uri "/home/me/a b.scm")  => "file:///home/me/a%20b.scm"
;;;   (file-uri->path "file:///C:/x/y")    => "C:/x/y"
;;;   (percent-encode "a b")  => "a%20b"      (percent-decode "a%20b") => "a b"
;;;
;;; The components of a parsed `uri` are kept as written (still percent-encoded); a component
;;; that is absent is #f, which is not the same as an empty one (`file:///x` has the empty
;;; authority "", `file:/x` has none). Paths are strings with `/` separators; a Windows path
;;; (`C:\x` or `C:/x`) becomes `file:///C:/x` and comes back as `C:/x`.

(library (capy uri)
  (export make-uri
    uri?
    uri-scheme
    uri-authority
    uri-path
    uri-query
    uri-fragment
    string->uri
    uri->string
    percent-encode
    percent-decode
    path->file-uri
    file-uri->path
    file-uri?)
  (import (rnrs))

  (define-record-type (<uri> make-uri uri?)
    (fields
      (immutable scheme uri-scheme)
      (immutable authority uri-authority)
      (immutable path uri-path)
      (immutable query uri-query)
      (immutable fragment uri-fragment)))

  ;; The index of the first character of `chars` found in `str` from `start`, or `end`.
  (define (scan str start end chars)
    (let loop ([i start])
      (cond
        [(>= i end) end]
        [(memv (string-ref str i) chars) i]
        [else (loop (+ i 1))])))

  (define (scheme-char? c first?)
    (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))
      (and (not first?)
        (or (and (char>=? c #\0) (char<=? c #\9)) (memv c '(#\+ #\- #\.))))))

  ;; The index of the `:` that ends a scheme at the start of `str`, or #f.
  (define (scheme-end str)
    (let ([n (string-length str)])
      (and (> n 0)
        (scheme-char? (string-ref str 0) #t)
        (let loop ([i 1])
          (cond
            [(>= i n) #f]
            [(char=? (string-ref str i) #\:) i]
            [(scheme-char? (string-ref str i) #f) (loop (+ i 1))]
            [else #f])))))

  ;; Splits a URI reference into its five components, as in RFC 3986 appendix B.
  (define (string->uri str)
    (let* ([n (string-length str)]
           [se (scheme-end str)]
           [scheme (and se (substring str 0 se))]
           [i (if se (+ se 1) 0)]
           [authority? (and (<= (+ i 2) n)
                         (char=? (string-ref str i) #\/)
                         (char=? (string-ref str (+ i 1)) #\/))]
           [ae (if authority? (scan str (+ i 2) n '(#\/ #\? #\#)) i)]
           [authority (and authority? (substring str (+ i 2) ae))]
           [pe (scan str ae n '(#\? #\#))]
           [path (substring str ae pe)]
           [query? (and (< pe n) (char=? (string-ref str pe) #\?))]
           [qe (if query? (scan str (+ pe 1) n '(#\#)) pe)]
           [query (and query? (substring str (+ pe 1) qe))]
           [fragment (and (< qe n) (substring str (+ qe 1) n))])
      (make-uri scheme authority path query fragment)))

  (define (uri->string uri)
    (string-append
      (if (uri-scheme uri) (string-append (uri-scheme uri) ":") "")
      (if (uri-authority uri) (string-append "//" (uri-authority uri)) "")
      (uri-path uri)
      (if (uri-query uri) (string-append "?" (uri-query uri)) "")
      (if (uri-fragment uri) (string-append "#" (uri-fragment uri)) "")))

  (define hex-digits "0123456789ABCDEF")

  (define (unreserved? b)
    (or (and (>= b 97) (<= b 122)) (and (>= b 65) (<= b 90)) (and (>= b 48) (<= b 57))
      (memv b '(45 46 95 126))))

  ;; The UTF-8 bytes of STR with everything but unreserved characters (and the characters of
  ;; KEEP, a string) written as %XX.
  (define percent-encode
    (case-lambda
      [(str) (percent-encode str "")]
      [(str keep)
        (let ([keep (map char->integer (string->list keep))]
              [bv (string->utf8 str)])
          (call-with-string-output-port
            (lambda (out)
              (let loop ([i 0])
                (when (< i (bytevector-length bv))
                  (let ([b (bytevector-u8-ref bv i)])
                    (if (or (unreserved? b) (memv b keep))
                      (put-char out (integer->char b))
                      (begin
                        (put-char out #\%)
                        (put-char out (string-ref hex-digits (bitwise-arithmetic-shift-right b 4)))
                        (put-char out (string-ref hex-digits (bitwise-and b 15))))))
                  (loop (+ i 1)))))))]))

  (define (hex-value c)
    (cond
      [(and (char>=? c #\0) (char<=? c #\9)) (- (char->integer c) 48)]
      [(and (char>=? c #\a) (char<=? c #\f)) (- (char->integer c) 87)]
      [(and (char>=? c #\A) (char<=? c #\F)) (- (char->integer c) 55)]
      [else #f]))

  ;; Decodes %XX escapes (as UTF-8). A malformed escape stays as written.
  (define (percent-decode str)
    (let ([n (string-length str)])
      (let-values ([(out get) (open-bytevector-output-port)])
        (let loop ([i 0])
          (when (< i n)
            (let ([c (string-ref str i)])
              (cond
                [(and (char=? c #\%) (<= (+ i 3) n)
                   (hex-value (string-ref str (+ i 1)))
                   (hex-value (string-ref str (+ i 2))))
                  (put-u8 out (+ (* 16 (hex-value (string-ref str (+ i 1))))
                                (hex-value (string-ref str (+ i 2)))))
                  (loop (+ i 3))]
                [else
                  (put-bytevector out (string->utf8 (string c)))
                  (loop (+ i 1))]))))
        (utf8->string (get)))))

  (define (drive-path? path)
    (and (>= (string-length path) 2)
      (let ([c (string-ref path 0)])
        (or (and (char>=? c #\a) (char<=? c #\z)) (and (char>=? c #\A) (char<=? c #\Z))))
      (char=? (string-ref path 1) #\:)
      (or (= (string-length path) 2) (memv (string-ref path 2) '(#\/ #\\)))
      #t))

  (define (replace-backslashes str)
    (list->string (map (lambda (c) (if (char=? c #\\) #\/ c)) (string->list str))))

  ;; The `file://` URI of an absolute PATH.
  (define (path->file-uri path)
    (let ([path (if (drive-path? path) (replace-backslashes path) path)])
      (cond
        [(drive-path? path)
          (string-append "file:///" (substring path 0 1) ":"
            (percent-encode (substring path 2 (string-length path)) "/"))]
        [(and (> (string-length path) 0) (char=? (string-ref path 0) #\/))
          (string-append "file://" (percent-encode path "/"))]
        [else (assertion-violation 'path->file-uri "not an absolute path" path)])))

  (define (file-uri? str)
    (let ([scheme (uri-scheme (string->uri str))])
      (and scheme (string-ci=? scheme "file"))))

  ;; The path of a `file:` URI, or #f for another scheme or a remote host. A drive path
  ;; `/C:/x` (the colon may be written %3A) comes back as `C:/x`.
  (define (file-uri->path str)
    (let ([uri (string->uri str)])
      (and (uri-scheme uri)
        (string-ci=? (uri-scheme uri) "file")
        (let ([auth (uri-authority uri)])
          (or (not auth) (string=? auth "") (string-ci=? auth "localhost")))
        (let ([path (percent-decode (uri-path uri))])
          (if (and (>= (string-length path) 3)
               (char=? (string-ref path 0) #\/)
               (drive-path? (substring path 1 (string-length path))))
            (substring path 1 (string-length path))
            path)))))
  )
