;; Socket primitives for (capy net)
(library (capy net sys)
  (export
    AF_UNSPEC AF_UNIX AF_INET AF_INET6 SOCK_STREAM SOCK_DGRAM
    SOL_SOCKET SO_REUSEADDR SO_REUSEPORT SO_KEEPALIVE SO_BROADCAST
    SO_SNDBUF SO_RCVBUF SO_ERROR SO_NOSIGPIPE
    IPPROTO_TCP TCP_NODELAY
    SHUT_RD SHUT_WR SHUT_RDWR
    SEND-FLAGS BSD-SOCKETS? SOMAXCONN AI_PASSIVE AI_NUMERICHOST
    SOCKADDR-STORAGE-SIZE
    EINTR EBADF EAGAIN EWOULDBLOCK EINPROGRESS EALREADY ENOTSOCK
    EADDRINUSE EADDRNOTAVAIL ENETUNREACH ECONNABORTED ECONNRESET
    EISCONN ENOTCONN ETIMEDOUT ECONNREFUSED EPIPE EMSGSIZE

    c-socket c-bind c-listen c-accept c-connect c-read c-send c-sendto c-recvfrom
    c-shutdown c-setsockopt c-getsockopt c-getsockname c-getpeername
    c-getaddrinfo c-freeaddrinfo c-gai-strerror c-strerror

    make-net-address net-address? net-address-host net-address-port net-address-family
    net-address-family-of-host

    encode-sockaddr sockaddr-decode lookup-addresses
    parse-ipv4 parse-ipv6 format-ipv4 format-ipv6
    new-socket set-int-option! get-int-option errno->string
    family-number

    &net-error make-net-error-condition net-error? net-error-errno
    &net-timeout make-net-timeout-condition net-timeout?
    &net-closed make-net-closed-condition net-closed?
    raise-net-error raise-net-timeout raise-net-closed)

  (import
    (rnrs)
    (only (scheme base) cond-expand)
    (core foreign)
    (only (capy c) c-include)
    (only (capy) term/nonblocking! make-bytevector/nonmoving))

  ;; Generated bindings. Each header is generated separately, with only the
  ;; names listed, so each generated library stays small. Only getaddrinfo may
  ;; block; the other groups turn blocking off.

  (c-include "sys/socket.h"
    #:include '("AF_UNSPEC" "AF_UNIX" "AF_INET" "AF_INET6" "SOCK_STREAM" "SOCK_DGRAM"
                "SOL_SOCKET" "SO_REUSEADDR" "SO_REUSEPORT" "SO_KEEPALIVE" "SO_BROADCAST"
                "SO_SNDBUF" "SO_RCVBUF" "SO_ERROR" "SO_NOSIGPIPE" "SHUT_RD" "SHUT_WR"
                "SHUT_RDWR" "SOMAXCONN" "MSG_NOSIGNAL"
                "socket" "bind" "listen" "accept" "connect" "sendto" "recvfrom" "shutdown"
                "send" "setsockopt" "getsockopt" "getsockname" "getpeername"
                "sockaddr_storage")
    #:blocking #f
    #:prefix socket:)


  (cond-expand
    [linux
      (c-include "asm-generic/socket.h"
        #:include '("SOL_SOCKET" "SO_REUSEADDR" "SO_REUSEPORT" "SO_KEEPALIVE" "SO_BROADCAST"
                    "SO_SNDBUF" "SO_RCVBUF" "SO_ERROR")
        #:blocking #f
        #:prefix socket:)
      (c-include "asm-generic/errno-base.h"
        #:include '("EINTR" "EBADF" "EAGAIN" "EPIPE")
        #:blocking #f
        #:prefix errno:)]
    [else (begin)])

  (c-include "netinet/in.h"
    #:include '("sockaddr_in" "IPPROTO_TCP")
    #:blocking #f
    #:prefix inet:)

  (c-include "netinet/in.h"
    #:only-from '("netinet/in.h" "netinet6/in6.h")
    #:include '("sockaddr_in6")
    #:blocking #f
    #:prefix inet6:)

  (c-include "netinet/tcp.h"
    #:include '("TCP_NODELAY")
    #:blocking #f
    #:prefix tcp:)

  (c-include "sys/un.h"
    #:include '("sockaddr_un")
    #:blocking #f
    #:prefix unix:)

  (c-include "unistd.h"
    #:include '("read")
    #:blocking #f
    #:prefix unistd:)

  (c-include "netdb.h"
    #:include '("addrinfo" "getaddrinfo" "freeaddrinfo" "gai_strerror" "AI_PASSIVE" "AI_NUMERICHOST")
    #:blocking '("getaddrinfo")
    #:prefix netdb:)

  (c-include "errno.h"
    #:include '("EINTR" "EBADF" "EAGAIN" "EWOULDBLOCK" "EINPROGRESS" "EALREADY"
                "ENOTSOCK" "EMSGSIZE" "EPIPE" "EADDRINUSE" "EADDRNOTAVAIL"
                "ENETUNREACH" "ECONNABORTED" "ECONNRESET" "EISCONN" "ENOTCONN"
                "ETIMEDOUT" "ECONNREFUSED")
    #:blocking #f
    #:prefix errno:)

  (c-include "string.h"
    #:include '("strerror")
    #:blocking #f
    #:prefix strh:)

  ;; Constants and functions under the names the rest of (capy net) uses.

  (define BSD-SOCKETS? (cond-expand [macos #t] [else #f]))

  (define AF_UNSPEC socket:AF_UNSPEC)
  (define AF_UNIX socket:AF_UNIX)
  (define AF_INET socket:AF_INET)
  (define AF_INET6 socket:AF_INET6)
  (define SOCK_STREAM socket:SOCK_STREAM)
  (define SOCK_DGRAM socket:SOCK_DGRAM)
  (define SOL_SOCKET socket:SOL_SOCKET)
  (define SO_REUSEADDR socket:SO_REUSEADDR)
  (define SO_REUSEPORT socket:SO_REUSEPORT)
  (define SO_KEEPALIVE socket:SO_KEEPALIVE)
  (define SO_BROADCAST socket:SO_BROADCAST)
  (define SO_SNDBUF socket:SO_SNDBUF)
  (define SO_RCVBUF socket:SO_RCVBUF)
  (define SO_ERROR socket:SO_ERROR)
  ;; macOS has no MSG_NOSIGNAL, so sockets there are told not to raise SIGPIPE
  ;; (SO_NOSIGPIPE, set on every socket) and sends pass no flags. Elsewhere
  ;; sends pass MSG_NOSIGNAL. Only one of the two is ever used.
  (define SO_NOSIGPIPE (cond-expand [macos socket:SO_NOSIGPIPE] [else 0]))
  (define SEND-FLAGS (cond-expand [macos 0] [else socket:MSG_NOSIGNAL]))
  (define SHUT_RD socket:SHUT_RD)
  (define SHUT_WR socket:SHUT_WR)
  (define SHUT_RDWR socket:SHUT_RDWR)
  (define SOMAXCONN socket:SOMAXCONN)
  (define IPPROTO_TCP inet:IPPROTO_TCP)
  (define TCP_NODELAY tcp:TCP_NODELAY)
  (define AI_PASSIVE netdb:AI_PASSIVE)
  (define AI_NUMERICHOST netdb:AI_NUMERICHOST)
  (define SOCKADDR-STORAGE-SIZE socket:sizeof-sockaddr_storage)

  (define EINTR errno:EINTR)
  (define EBADF errno:EBADF)
  (define EAGAIN errno:EAGAIN)
  (define EWOULDBLOCK errno:EWOULDBLOCK)
  (define EINPROGRESS errno:EINPROGRESS)
  (define EALREADY errno:EALREADY)
  (define ENOTSOCK errno:ENOTSOCK)
  (define EMSGSIZE errno:EMSGSIZE)
  (define EPIPE errno:EPIPE)
  (define EADDRINUSE errno:EADDRINUSE)
  (define EADDRNOTAVAIL errno:EADDRNOTAVAIL)
  (define ENETUNREACH errno:ENETUNREACH)
  (define ECONNABORTED errno:ECONNABORTED)
  (define ECONNRESET errno:ECONNRESET)
  (define EISCONN errno:EISCONN)
  (define ENOTCONN errno:ENOTCONN)
  (define ETIMEDOUT errno:ETIMEDOUT)
  (define ECONNREFUSED errno:ECONNREFUSED)

  (define c-socket socket:socket)
  (define c-bind socket:bind)
  (define c-listen socket:listen)
  (define c-accept socket:accept)
  (define c-read unistd:read)
  (define c-send socket:send)
  (define c-connect socket:connect)
  (define c-sendto socket:sendto)
  (define c-recvfrom socket:recvfrom)
  (define c-shutdown socket:shutdown)
  (define c-setsockopt socket:setsockopt)
  (define c-getsockopt socket:getsockopt)
  (define c-getsockname socket:getsockname)
  (define c-getpeername socket:getpeername)
  (define c-getaddrinfo netdb:getaddrinfo)
  (define c-freeaddrinfo netdb:freeaddrinfo)
  (define c-gai-strerror netdb:gai_strerror)
  (define c-strerror strh:strerror)

  ;; Conditions. Each one carries the errno it came from, when there is one.
  (define-condition-type &net-error &error
    make-net-error-condition net-error?
    (errno net-error-errno))
  (define-condition-type &net-timeout &net-error
    make-net-timeout-condition net-timeout?)
  (define-condition-type &net-closed &net-error
    make-net-closed-condition net-closed?)

  (define (errno->string n)
    (pointer->string (c-strerror n)))

  (define (raise-net-error who errno . message)
    (raise
      (condition
        (make-net-error-condition errno)
        (make-who-condition who)
        (make-message-condition
          (if (null? message) (errno->string errno) (car message))))))

  (define (raise-net-timeout who)
    (raise
      (condition
        (make-net-timeout-condition ETIMEDOUT)
        (make-who-condition who)
        (make-message-condition "i/o timeout"))))

  (define (raise-net-closed who)
    (raise
      (condition
        (make-net-closed-condition #f)
        (make-who-condition who)
        (make-message-condition "use of closed network connection"))))

  (define-record-type (net-address make-net-address net-address?)
    (fields (immutable host net-address-host)
            (immutable port net-address-port)
            (immutable family net-address-family)))

  (define (net-address-family-of-host host)
    (cond
      [(parse-ipv4 host) 'ipv4]
      [(parse-ipv6 host) 'ipv6]
      [else 'unspec]))

  (define (family-number family)
    (case family
      [(ipv4) AF_INET]
      [(ipv6) AF_INET6]
      [(unix) AF_UNIX]
      [else AF_UNSPEC]))

  ;; Numeric IPv4: "a.b.c.d" to 4 bytes, or #f.
  (define (parse-ipv4 s)
    (define n (string-length s))
    (let loop ([i 0] [cur #f] [parts '()])
      (cond
        [(= i n)
         (and cur (= (length parts) 3)
              (let ([bv (make-bytevector 4 0)])
                (let fill ([ps (cons cur parts)] [k 3])
                  (cond
                    [(null? ps) bv]
                    [else
                      (bytevector-u8-set! bv k (car ps))
                      (fill (cdr ps) (- k 1))]))))]
        [else
          (let ([c (string-ref s i)])
            (cond
              [(char<=? #\0 c #\9)
               (let ([v (+ (* 10 (or cur 0)) (- (char->integer c) 48))])
                 (and (<= v 255) (loop (+ i 1) v parts)))]
              [(and (char=? c #\.) cur (< (length parts) 3))
               (loop (+ i 1) #f (cons cur parts))]
              [else #f]))])))

  (define (hex-digit c)
    (cond
      [(char<=? #\0 c #\9) (- (char->integer c) 48)]
      [(char<=? #\a (char-downcase c) #\f) (- (char->integer (char-downcase c)) 87)]
      [else #f]))

  (define (split-on s ch)
    (let loop ([i (- (string-length s) 1)] [end (string-length s)] [acc '()])
      (cond
        [(< i 0) (cons (substring s 0 end) acc)]
        [(char=? (string-ref s i) ch)
         (loop (- i 1) i (cons (substring s (+ i 1) end) acc))]
        [else (loop (- i 1) end acc)])))

  (define (find-double-colon s start)
    (let loop ([i start])
      (cond
        [(>= (+ i 1) (string-length s)) #f]
        [(and (char=? (string-ref s i) #\:) (char=? (string-ref s (+ i 1)) #\:)) i]
        [else (loop (+ i 1))])))

  ;; A list of 16-bit group values, or #f.
  (define (parse-ipv6-groups str)
    (cond
      [(string=? str "") '()]
      [else
        (let loop ([pieces (split-on str #\:)] [acc '()])
          (cond
            [(null? pieces) (reverse acc)]
            [else
              (let* ([p (car pieces)] [len (string-length p)])
                (and (<= 1 len 4)
                     (let hex ([i 0] [v 0])
                       (if (= i len)
                         (loop (cdr pieces) (cons v acc))
                         (let ([d (hex-digit (string-ref p i))])
                           (and d (hex (+ i 1) (+ (* v 16) d))))))))]))]))

  (define (zero-groups n)
    (if (<= n 0) '() (cons 0 (zero-groups (- n 1)))))

  ;; Numeric IPv6 text to 16 bytes, or #f. Scope ids are not accepted.
  (define (parse-ipv6 s)
    (define dc (find-double-colon s 0))
    (define groups
      (cond
        [(not dc)
         (let ([gs (parse-ipv6-groups s)])
           (and gs (= (length gs) 8) gs))]
        [(find-double-colon s (+ dc 1)) #f]
        [else
          (let ([head (parse-ipv6-groups (substring s 0 dc))]
                [tail (parse-ipv6-groups (substring s (+ dc 2) (string-length s)))])
            (and head tail
                 (<= (+ (length head) (length tail)) 7)
                 (append head
                         (zero-groups (- 8 (length head) (length tail)))
                         tail)))]))
    (and groups
         (let ([bv (make-bytevector 16 0)])
           (let fill ([gs groups] [i 0])
             (cond
               [(null? gs) bv]
               [else
                 (bytevector-u16-set! bv (* 2 i) (car gs) 'big)
                 (fill (cdr gs) (+ i 1))])))))

  (define (format-ipv4 bv off)
    (string-append
      (number->string (bytevector-u8-ref bv off)) "."
      (number->string (bytevector-u8-ref bv (+ off 1))) "."
      (number->string (bytevector-u8-ref bv (+ off 2))) "."
      (number->string (bytevector-u8-ref bv (+ off 3)))))

  (define (join-strings strs sep)
    (cond
      [(null? strs) ""]
      [else
        (let loop ([rest (cdr strs)] [acc (car strs)])
          (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc sep (car rest)))))]))

  ;; RFC 5952 text: lowercase, the longest run of two or more zero groups
  ;; becomes "::".
  (define (format-ipv6 bv off)
    (define (group i) (bytevector-u16-ref bv (+ off (* 2 i)) 'big))
    ;; The longest run of zero groups, as (start . length), or #f.
    (define (longest-zero-run)
      (let loop ([i 0] [run-start 0] [run-len 0] [best #f])
        (let ([best-len (if best (cdr best) 0)])
          (cond
            [(= i 8) (and best (>= best-len 2) best)]
            [(zero? (group i))
             (let ([start (if (zero? run-len) i run-start)]
                   [len (+ run-len 1)])
               (loop (+ i 1) start len
                     (if (> len best-len) (cons start len) best)))]
            [else (loop (+ i 1) 0 0 best)]))))
    (define (hexes from to)
      (let loop ([i (- to 1)] [acc '()])
        (if (< i from)
          acc
          (loop (- i 1) (cons (number->string (group i) 16) acc)))))
    (let ([run (longest-zero-run)])
      (if run
        (string-append
          (join-strings (hexes 0 (car run)) ":")
          "::"
          (join-strings (hexes (+ (car run) (cdr run)) 8) ":"))
        (join-strings (hexes 0 8) ":"))))

  (define (swap16 v)
    (let ([bv (make-bytevector 2 0)])
      (bytevector-u16-native-set! bv 0 v)
      (bytevector-u16-ref bv 0 'big)))

  (define (copy-bytes-into! dst src n)
    (let loop ([i 0])
      (when (< i n)
        (pointer-set! uint8 dst i (bytevector-u8-ref src i))
        (loop (+ i 1)))))

  ;; The sin_len and sun_len fields exist on BSD-style sockaddrs only.
  (define (set-sa-len! kind p n)
    (cond-expand
      [macos
        (case kind
          [(in) (inet:sockaddr_in-sin_len-set! p n)]
          [(in6) (inet6:sockaddr_in6-sin6_len-set! p n)]
          [else (unix:sockaddr_un-sun_len-set! p n)])]
      [else (if #f #f)]))

  (define SUN-PATH-OFFSET 2)
  (define SUN-PATH-MAX (- unix:sizeof-sockaddr_un SUN-PATH-OFFSET))

  (define (encode-ipv4 ip port)
    (define bv (make-bytevector/nonmoving inet:sizeof-sockaddr_in 0))
    (define p (bytevector->pointer bv))
    (set-sa-len! 'in p inet:sizeof-sockaddr_in)
    (inet:sockaddr_in-sin_family-set! p AF_INET)
    (inet:sockaddr_in-sin_port-set! p (swap16 port))
    (copy-bytes-into! (inet:sockaddr_in-sin_addr p) ip 4)
    bv)

  (define (encode-ipv6 ip port)
    (define bv (make-bytevector/nonmoving inet6:sizeof-sockaddr_in6 0))
    (define p (bytevector->pointer bv))
    (set-sa-len! 'in6 p inet6:sizeof-sockaddr_in6)
    (inet6:sockaddr_in6-sin6_family-set! p AF_INET6)
    (inet6:sockaddr_in6-sin6_port-set! p (swap16 port))
    (copy-bytes-into! (inet6:sockaddr_in6-sin6_addr p) ip 16)
    bv)

  (define (encode-unix path)
    (define bytes (string->utf8 path))
    (unless (< (bytevector-length bytes) SUN-PATH-MAX)
      (raise-net-error 'unix-address #f "unix socket path too long"))
    (define bv (make-bytevector/nonmoving unix:sizeof-sockaddr_un 0))
    (define p (bytevector->pointer bv))
    (set-sa-len! 'un p unix:sizeof-sockaddr_un)
    (unix:sockaddr_un-sun_family-set! p AF_UNIX)
    (unix:sockaddr_un-sun_path-string-set! p path)
    bv)

  (define (encode-sockaddr family host port)
    (cond
      [(eq? family 'unix) (encode-unix host)]
      [(parse-ipv4 host) => (lambda (ip) (encode-ipv4 ip port))]
      [(parse-ipv6 host) => (lambda (ip) (encode-ipv6 ip port))]
      [else #f]))

  (define (sockaddr-decode p)
    (define family (socket:sockaddr_storage-ss_family p))
    (cond
      [(= family AF_INET)
       (make-net-address
         (format-ipv4 (pointer->bytevector (inet:sockaddr_in-sin_addr p) 4) 0)
         (swap16 (inet:sockaddr_in-sin_port p))
         'ipv4)]
      [(= family AF_INET6)
       (make-net-address
         (format-ipv6 (pointer->bytevector (inet6:sockaddr_in6-sin6_addr p) 16) 0)
         (swap16 (inet6:sockaddr_in6-sin6_port p))
         'ipv6)]
      [(= family AF_UNIX)
       (make-net-address (unix:sockaddr_un-sun_path-string p) 0 'unix)]
      [else (raise-net-error 'sockaddr-decode #f "unknown address family")]))

  (define (gai-error who rc)
    (raise-net-error who #f (pointer->string (c-gai-strerror rc))))

  ;; Resolve `host` through getaddrinfo (blocking). Returns numeric
  ;; net-addresses in the order the resolver gives them. Only the Internet
  ;; families are returned.
  (define (lookup-addresses who host port socktype passive?)
    (define hints-bv (make-bytevector/nonmoving netdb:sizeof-addrinfo 0))
    (define hints (bytevector->pointer hints-bv))
    (define out (make-bytevector/nonmoving 8 0))
    (netdb:addrinfo-ai_flags-set! hints (if passive? AI_PASSIVE 0))
    (netdb:addrinfo-ai_family-set! hints AF_UNSPEC)
    (netdb:addrinfo-ai_socktype-set! hints socktype)
    (netdb:addrinfo-ai_protocol-set! hints 0)
    (let ([rc (c-getaddrinfo
                (if (string=? host "") %null-pointer (string->pointer host))
                (string->pointer (number->string port))
                hints
                (bytevector->pointer out))])
      (unless (zero? rc) (gai-error who rc))
      (let ([head (pointer-ref/pointer (bytevector->pointer out) 0)])
        (let loop ([p head] [acc '()])
          (cond
            [(null-pointer? p)
             (c-freeaddrinfo head)
             (reverse acc)]
            [else
              (let ([family (netdb:addrinfo-ai_family p)]
                    [addr (netdb:addrinfo-ai_addr p)])
                (loop (netdb:addrinfo-ai_next p)
                      (if (and (or (= family AF_INET) (= family AF_INET6))
                               (not (null-pointer? addr)))
                        (cons (sockaddr-decode addr) acc)
                        acc)))])))))

  (define (new-socket family socktype who)
    (define fd (c-socket (family-number family) socktype 0))
    (when (< fd 0) (raise-net-error who (errno)))
    (term/nonblocking! fd #t)
    (when BSD-SOCKETS?
      (set-int-option! fd SOL_SOCKET SO_NOSIGPIPE 1 who))
    fd)

  ;; int option values and the socklen_t that goes with them
  (define (set-int-option! fd level name value who)
    (define buf (make-bytevector/nonmoving 4 0))
    (define p (bytevector->pointer buf))
    (pointer-set! int p 0 value)
    (when (< (c-setsockopt fd level name p 4) 0)
      (raise-net-error who (errno))))

  (define (get-int-option fd level name who)
    (define buf (make-bytevector/nonmoving 4 0))
    (define len (make-bytevector/nonmoving 4 0))
    (define p (bytevector->pointer buf))
    (pointer-set! unsigned-int (bytevector->pointer len) 0 4)
    (when (< (c-getsockopt fd level name p (bytevector->pointer len)) 0)
      (raise-net-error who (errno)))
    (pointer-ref int p 0))
)
