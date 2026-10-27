;; Sockets
(library (capy net)
  (export
    parse-address
    address->string
    make-net-address
    net-address?
    net-address-host
    net-address-port
    net-address-family
    resolve-host

    tcp-listen
    listener?
    listener-accept
    listener-close
    listener-closed?
    listener-address
    listener-fd

    tcp-connect
    conn?
    conn-read!
    conn-read
    conn-write-all
    conn-close
    conn-closed?
    conn-fd
    conn-set-deadline!
    conn-set-read-deadline!
    conn-set-write-deadline!
    conn-local-address
    conn-remote-address
    conn-set-nodelay!
    conn-set-keepalive!
    conn-set-rcvbuf!
    conn-set-sndbuf!
    conn-shutdown
    conn->binary-input-port
    conn->binary-output-port

    make-conn-reader
    conn-reader?
    reader-read-line
    reader-read-exactly
    reader-read-until
    reader-read-some
    reader-peek-byte
    reader-read-byte

    udp-bind
    udp-connect
    udp-send-to
    udp-recv-from
    udp-send
    udp-recv
    packet-conn?
    packet-conn-close
    packet-conn-closed?
    packet-conn-local-address
    packet-conn-set-broadcast!
    packet-conn-set-deadline!
    packet-conn-set-read-deadline!
    packet-conn-set-write-deadline!

    unix-listen
    unix-connect

    &net-error
    net-error?
    net-error-errno
    &net-timeout
    net-timeout?
    &net-closed
    net-closed?)

  (import
    (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (scheme time)
    (only (core foreign) errno %null-pointer bytevector->pointer)
    (only (rnrs r5rs) inexact->exact)
    (only (capy) make-bytevector/nonmoving term/nonblocking! %await-fd-timeout syscall:read syscall:send
          syscall:close)
    (capy net sys))

  ;; Deadlines are absolute values of (current-jiffy), or #f for none.
  (define unspecified-value (if #f #f))

  (define (timeout->deadline secs)
    (and secs
         (+ (current-jiffy)
            (inexact->exact (round (* secs (jiffies-per-second)))))))

  ;; Waits until `fd` is ready in direction `dir` ('read or 'write), parking
  ;; only the calling thread. `deadline` is absolute (from timeout->deadline)
  ;; or #f. A wait that ends because the descriptor was closed raises
  ;; &net-closed; one that passes the deadline raises &net-timeout.
  (define (await-fd who fd dir deadline)
    (let* ([events (if (eq? dir 'read) 1 2)]
           [ms (and deadline
                    (let ([remaining (- deadline (current-jiffy))])
                      (when (<= remaining 0)
                        (raise-net-timeout who))
                      (min 2147483647
                           (inexact->exact
                             (ceiling (/ (* remaining 1000) (jiffies-per-second)))))))])
      (case (%await-fd-timeout fd events ms)
        [(0) unspecified-value]
        [(1) (raise-net-timeout who)]
        [(2) (raise-net-closed who)]
        [else (raise-net-error who #f "wait on descriptor failed")])))

  (define (numeric-address? host)
    (or (parse-ipv4 host) (parse-ipv6 host)))

  ;; Addresses for host and port. Numeric hosts are not looked up. An empty
  ;; host means the wildcard address when listening and the loopback address
  ;; when connecting.
  (define (resolve-addresses who host port socktype passive?)
    (cond
      [(string=? host "")
       (resolve-addresses who (if passive? "0.0.0.0" "127.0.0.1")
                          port socktype passive?)]
      [(numeric-address? host)
       (list (make-net-address host port (net-address-family-of-host host)))]
      [else (lookup-addresses who host port socktype passive?)]))

  (define (first-address who host port socktype passive?)
    (let ([addrs (resolve-addresses who host port socktype passive?)])
      (if (null? addrs)
        (raise-net-error who #f (string-append "no addresses for " host))
        (car addrs))))

  (define (sockaddr-of who addr)
    (or (encode-sockaddr (net-address-family addr)
                         (net-address-host addr)
                         (net-address-port addr))
        (let ([resolved (first-address who (net-address-host addr)
                                       (net-address-port addr) SOCK_DGRAM #f)])
          (encode-sockaddr (net-address-family resolved)
                           (net-address-host resolved)
                           (net-address-port resolved)))))

  ;; Addresses

  (define (parse-address s)
    (define n (string-length s))
    (define (decimal-port str)
      (and (> (string-length str) 0)
           (let loop ([i 0] [v 0])
             (cond
               [(= i (string-length str))
                (and (<= v 65535) v)]
               [(char<=? #\0 (string-ref str i) #\9)
                (loop (+ i 1) (+ (* v 10) (- (char->integer (string-ref str i)) 48)))]
               [else #f]))))
    (define (bad)
      (raise-net-error 'parse-address #f (string-append "invalid address: " s)))
    (define close-bracket
      (and (> n 0) (char=? (string-ref s 0) #\[)
           (let loop ([i 1])
             (cond
               [(>= i n) #f]
               [(char=? (string-ref s i) #\]) i]
               [else (loop (+ i 1))]))))
    (cond
      [close-bracket
        (let ([host (substring s 1 close-bracket)]
              [rest (substring s (+ close-bracket 1) n)])
          (unless (and (> (string-length rest) 0) (char=? (string-ref rest 0) #\:))
            (bad))
          (let ([port (decimal-port (substring rest 1 (string-length rest)))])
            (unless (and port (parse-ipv6 host)) (bad))
            (make-net-address host port 'ipv6)))]
      [else
        (let loop ([i (- n 1)])
          (cond
            [(< i 0) (bad)]
            [(char=? (string-ref s i) #\:)
             (let ([host (substring s 0 i)]
                   [port (decimal-port (substring s (+ i 1) n))])
               (unless (and port (not (string=? host ""))
                            (not (find-colon host)))
                 (bad))
               (make-net-address host port (net-address-family-of-host host)))]
            [else (loop (- i 1))]))]))

  (define (find-colon s)
    (let loop ([i 0])
      (cond
        [(= i (string-length s)) #f]
        [(char=? (string-ref s i) #\:) #t]
        [else (loop (+ i 1))])))

  (define (address->string a)
    (define host (net-address-host a))
    (case (net-address-family a)
      [(unix) host]
      [(ipv6) (string-append "[" host "]:" (number->string (net-address-port a)))]
      [else (string-append host ":" (number->string (net-address-port a)))]))

  (define (resolve-host name)
    (let loop ([addrs (resolve-addresses 'resolve-host name 0 SOCK_STREAM #f)]
               [acc '()])
      (cond
        [(null? addrs) (reverse acc)]
        [(member (net-address-host (car addrs)) acc) (loop (cdr addrs) acc)]
        [else (loop (cdr addrs) (cons (net-address-host (car addrs)) acc))])))

  ;; Socket setup

  (define (configure-accepted! fd)
    (term/nonblocking! fd #t)
    (when BSD-SOCKETS?
      (set-int-option! fd SOL_SOCKET SO_NOSIGPIPE 1 'accept)))

  ;; Connections

  (define-record-type (conn %make-conn conn?)
    (fields (immutable fd conn-fd)
            (mutable closed? %conn-closed? %set-conn-closed!)
            (mutable read-deadline %conn-rdl %set-conn-rdl!)
            (mutable write-deadline %conn-wdl %set-conn-wdl!)
            (immutable tcp? %conn-tcp?)))

  (define (make-conn fd tcp?)
    (%make-conn fd #f #f #f tcp?))

  (define (conn-closed? c) (%conn-closed? c))

  (define (check-open c who)
    (when (%conn-closed? c)
      (raise-net-closed who)))

  ;; Called when a syscall on `c` failed with errno `e`.
  (define (io-failed c who e)
    (if (%conn-closed? c)
      (raise-net-closed who)
      (raise-net-error who e)))

  (define (close-fd-on-failure fd thunk)
    (guard (e [#t (syscall:close fd) (raise e)])
      (thunk)))

  ;; Connects a nonblocking socket to `addr`. Returns the connected fd.
  (define (connect-socket addr socktype deadline who)
    (define family (net-address-family addr))
    (define sa (sockaddr-of who addr))
    (define fd (new-socket family socktype who))
    (close-fd-on-failure fd
      (lambda ()
        (let ([rc (c-connect fd (bytevector->pointer sa) (bytevector-length sa))])
          (unless (zero? rc)
            (let ([e (errno)])
              (cond
                [(or (= e EINPROGRESS) (= e EINTR) (= e EAGAIN))
                 (await-fd who fd 'write deadline)
                 (let ([err (get-int-option fd SOL_SOCKET SO_ERROR who)])
                   (unless (zero? err)
                     (raise-net-error who err)))]
                [else (raise-net-error who e)]))))))
    fd)

  (define (tcp-connect host port #:timeout [timeout #f])
    (define deadline (timeout->deadline timeout))
    (define addrs (resolve-addresses 'tcp-connect host port SOCK_STREAM #f))
    (let try ([addrs addrs] [last #f])
      (cond
        [(null? addrs)
         (if last
           (raise last)
           (raise-net-error 'tcp-connect #f (string-append "no addresses for " host)))]
        [else
          (let ([result
                  (guard (e [(net-error? e) e])
                    (connect-socket (car addrs) SOCK_STREAM deadline 'tcp-connect))])
            (cond
              [(net-timeout? result) (raise result)]
              [(net-error? result) (try (cdr addrs) result)]
              [else
                (let ([c (make-conn result #t)])
                  (set-int-option! result IPPROTO_TCP TCP_NODELAY 1 'tcp-connect)
                  c)]))])))

  (define (conn-read! c bv . range)
    (define start (if (pair? range) (car range) 0))
    (define end (if (and (pair? range) (pair? (cdr range))) (cadr range) (bytevector-length bv)))
    (check-open c 'conn-read!)
    (if (= start end)
      0
      (let loop ()
        ;; With a deadline, wait for readiness first. syscall:read parks the
        ;; thread itself, so the read after a successful wait does not block.
        (when (%conn-rdl c)
          (await-fd 'conn-read! (conn-fd c) 'read (%conn-rdl c))
          (check-open c 'conn-read!))
        (let ([n (syscall:read (conn-fd c) bv (- end start) start)])
          (cond
            [(>= n 0) n]
            [else
              (let ([e (errno)])
                (if (or (= e EINTR) (= e EAGAIN))
                  (loop)
                  (io-failed c 'conn-read! e)))])))))

  (define (conn-read c n)
    (define bv (make-bytevector n))
    (let ([k (conn-read! c bv 0 n)])
      (cond
        [(zero? n) bv]
        [(zero? k) (eof-object)]
        [(= k n) bv]
        [else
          (let ([out (make-bytevector k)])
            (bytevector-copy! bv 0 out 0 k)
            out)])))

  (define WRITE-CHUNK 65536)

  (define (conn-write-all c bv . range)
    (define start (if (pair? range) (car range) 0))
    (define end (if (and (pair? range) (pair? (cdr range))) (cadr range) (bytevector-length bv)))
    (check-open c 'conn-write-all)
    (let loop ([pos start])
      (when (< pos end)
        (let* ([k (min (- end pos) WRITE-CHUNK)]
               [chunk (if (and (= pos 0) (= k (bytevector-length bv)))
                        bv
                        (let ([piece (make-bytevector k)])
                          (bytevector-copy! bv pos piece 0 k)
                          piece))])
          (when (%conn-wdl c)
            (await-fd 'conn-write-all (conn-fd c) 'write (%conn-wdl c))
            (check-open c 'conn-write-all))
          (let ([n (syscall:send (conn-fd c) chunk k SEND-FLAGS)])
            (cond
              [(>= n 0) (loop (+ pos n))]
              [else
                (let ([e (errno)])
                  (if (or (= e EINTR) (= e EAGAIN))
                    (loop pos)
                    (io-failed c 'conn-write-all e)))]))))))

  (define (conn-close c)
    (unless (%conn-closed? c)
      (%set-conn-closed! c #t)
      (syscall:close (conn-fd c))
      unspecified-value))

  (define (conn-set-deadline! c secs)
    (let ([d (timeout->deadline secs)])
      (%set-conn-rdl! c d)
      (%set-conn-wdl! c d)))

  (define (conn-set-read-deadline! c secs)
    (%set-conn-rdl! c (timeout->deadline secs)))

  (define (conn-set-write-deadline! c secs)
    (%set-conn-wdl! c (timeout->deadline secs)))

  (define (conn-local-address c)
    (sockname 'conn-local-address (conn-fd c) c-getsockname))

  (define (conn-remote-address c)
    (sockname 'conn-remote-address (conn-fd c) c-getpeername))

  (define (conn-set-nodelay! c on?)
    (set-int-option! (conn-fd c) IPPROTO_TCP TCP_NODELAY (if on? 1 0) 'conn-set-nodelay!))

  (define (conn-set-keepalive! c on?)
    (set-int-option! (conn-fd c) SOL_SOCKET SO_KEEPALIVE (if on? 1 0) 'conn-set-keepalive!))

  (define (conn-set-rcvbuf! c n)
    (set-int-option! (conn-fd c) SOL_SOCKET SO_RCVBUF n 'conn-set-rcvbuf!))

  (define (conn-set-sndbuf! c n)
    (set-int-option! (conn-fd c) SOL_SOCKET SO_SNDBUF n 'conn-set-sndbuf!))

  (define (conn-shutdown c how)
    (define n
      (case how
        [(read) SHUT_RD]
        [(write) SHUT_WR]
        [(both) SHUT_RDWR]
        [else (assertion-violation 'conn-shutdown "expected read, write or both" how)]))
    (when (< (c-shutdown (conn-fd c) n) 0)
      (io-failed c 'conn-shutdown (errno))))

  (define (conn->binary-input-port c)
    (make-custom-binary-input-port
      "capy-net-conn"
      (lambda (bv start count)
        (conn-read! c bv start (+ start count)))
      #f #f
      (lambda () (conn-close c))))

  (define (conn->binary-output-port c)
    (make-custom-binary-output-port
      "capy-net-conn"
      (lambda (bv start count)
        (conn-write-all c bv start (+ start count))
        count)
      #f #f
      (lambda () (conn-close c))))

  ;; Buffered reader

  (define-record-type (conn-reader make-reader conn-reader?)
    (fields (immutable conn reader-conn)
            (immutable buf reader-buf)
            (mutable start reader-start set-reader-start!)
            (mutable end reader-end set-reader-end!)))

  (define (make-conn-reader c [bufsize 16384])
    (make-reader c (make-bytevector bufsize) 0 0))

  ;; Makes sure at least one byte is buffered. Returns #f at end of stream.
  (define (reader-fill! r)
    (cond
      [(< (reader-start r) (reader-end r)) #t]
      [else
        (let ([n (conn-read! (reader-conn r) (reader-buf r) 0 (bytevector-length (reader-buf r)))])
          (set-reader-start! r 0)
          (set-reader-end! r n)
          (> n 0))]))

  (define (reader-peek-byte r)
    (if (reader-fill! r)
      (bytevector-u8-ref (reader-buf r) (reader-start r))
      (eof-object)))

  (define (reader-read-byte r)
    (if (reader-fill! r)
      (let ([b (bytevector-u8-ref (reader-buf r) (reader-start r))])
        (set-reader-start! r (+ (reader-start r) 1))
        b)
      (eof-object)))

  (define (buffered-copy r)
    (let* ([s (reader-start r)]
           [e (reader-end r)]
           [out (make-bytevector (- e s))])
      (bytevector-copy! (reader-buf r) s out 0 (- e s))
      (set-reader-start! r e)
      out))

  (define (reader-read-some r)
    (if (reader-fill! r)
      (buffered-copy r)
      (eof-object)))

  (define (reader-read-exactly r n)
    (define out (make-bytevector n))
    (define bufsize (bytevector-length (reader-buf r)))
    (let loop ([got 0])
      (cond
        [(= got n) out]
        [(< (reader-start r) (reader-end r))
         (let* ([avail (- (reader-end r) (reader-start r))]
                [k (min avail (- n got))])
           (bytevector-copy! (reader-buf r) (reader-start r) out got k)
           (set-reader-start! r (+ (reader-start r) k))
           (loop (+ got k)))]
        [(>= (- n got) bufsize)
         ;; Large remainder: read straight into the result.
         (let ([k (conn-read! (reader-conn r) out got n)])
           (when (zero? k)
             (raise-net-error 'reader-read-exactly #f "unexpected end of stream"))
           (loop (+ got k)))]
        [(reader-fill! r) (loop got)]
        [else (raise-net-error 'reader-read-exactly #f "unexpected end of stream")])))

  ;; Bytes up to and including `delim`, or the rest of the stream when it
  ;; ends first. Returns the eof object when no bytes remain.
  (define (reader-read-until r delim)
    (let loop ([chunks '()])
      (cond
        [(not (reader-fill! r))
         (if (null? chunks)
           (eof-object)
           (concat-chunks (reverse chunks)))]
        [else
          (let* ([buf (reader-buf r)]
                 [s (reader-start r)]
                 [e (reader-end r)]
                 [found
                   (let scan ([i s])
                     (cond
                       [(= i e) #f]
                       [(= (bytevector-u8-ref buf i) delim) i]
                       [else (scan (+ i 1))]))])
            (cond
              [found
               (let ([piece (make-bytevector (- (+ found 1) s))])
                 (bytevector-copy! buf s piece 0 (- (+ found 1) s))
                 (set-reader-start! r (+ found 1))
                 (concat-chunks (reverse (cons piece chunks))))]
              [else
               (let ([piece (make-bytevector (- e s))])
                 (bytevector-copy! buf s piece 0 (- e s))
                 (set-reader-start! r e)
                 (loop (cons piece chunks)))]))])))

  (define (concat-chunks chunks)
    (define total (fold-left + 0 (map bytevector-length chunks)))
    (define out (make-bytevector total))
    (let loop ([cs chunks] [pos 0])
      (if (null? cs)
        out
        (begin
          (bytevector-copy! (car cs) 0 out pos (bytevector-length (car cs)))
          (loop (cdr cs) (+ pos (bytevector-length (car cs))))))))

  ;; One line without its terminator. LF and CRLF both end a line. A final
  ;; line without a terminator is returned as is.
  (define (reader-read-line r)
    (let ([bv (reader-read-until r 10)])
      (cond
        [(eof-object? bv) bv]
        [else
          (let* ([n (bytevector-length bv)]
                 [n (if (and (> n 0) (= (bytevector-u8-ref bv (- n 1)) 10)) (- n 1) n)]
                 [n (if (and (> n 0) (= (bytevector-u8-ref bv (- n 1)) 13)) (- n 1) n)]
                 [line (make-bytevector n)])
            (bytevector-copy! bv 0 line 0 n)
            (utf8->string line))])))

  ;; Listeners

  (define-record-type (listener make-listener listener?)
    (fields (immutable fd listener-fd)
            (mutable closed? %listener-closed? %set-listener-closed!)
            (immutable address listener-address)
            (immutable tcp? %listener-tcp?)))

  (define (listener-closed? l) (%listener-closed? l))

  (define (listener-accept l)
    (when (%listener-closed? l)
      (raise-net-closed 'listener-accept))
    (let loop ()
      (let ([fd (c-accept (listener-fd l) %null-pointer %null-pointer)])
        (cond
          [(>= fd 0)
           (configure-accepted! fd)
           (when (%listener-tcp? l)
             (set-int-option! fd IPPROTO_TCP TCP_NODELAY 1 'listener-accept))
           (make-conn fd (%listener-tcp? l))]
          [else
            (let ([e (errno)])
              (cond
                [(= e EINTR) (loop)]
                [(or (= e EAGAIN) (= e ECONNABORTED))
                 (await-fd 'listener-accept (listener-fd l) 'read #f)
                 (loop)]
                [else (if (%listener-closed? l)
                        (raise-net-closed 'listener-accept)
                        (raise-net-error 'listener-accept e))]))]))))

  (define (listener-close l)
    (unless (%listener-closed? l)
      (%set-listener-closed! l #t)
      (syscall:close (listener-fd l))
      unspecified-value))

  (define (tcp-listen host port #:backlog [backlog SOMAXCONN]
                      #:reuse-addr? [reuse-addr? #t]
                      #:reuse-port? [reuse-port? #f])
    (define addr (first-address 'tcp-listen host port SOCK_STREAM #t))
    (define sa (sockaddr-of 'tcp-listen addr))
    (define fd (new-socket (net-address-family addr) SOCK_STREAM 'tcp-listen))
    (close-fd-on-failure fd
      (lambda ()
        (when reuse-addr?
          (set-int-option! fd SOL_SOCKET SO_REUSEADDR 1 'tcp-listen))
        (when reuse-port?
          (set-int-option! fd SOL_SOCKET SO_REUSEPORT 1 'tcp-listen))
        (when (< (c-bind fd (bytevector->pointer sa) (bytevector-length sa)) 0)
          (raise-net-error 'tcp-listen (errno)))
        (when (< (c-listen fd backlog) 0)
          (raise-net-error 'tcp-listen (errno)))))
    (make-listener fd #f (sockname 'tcp-listen fd c-getsockname) #t))

  (define (sockname who fd getter)
    (define buf (make-bytevector/nonmoving SOCKADDR-STORAGE-SIZE 0))
    (define len (make-bytevector/nonmoving 4 0))
    (bytevector-u32-native-set! len 0 SOCKADDR-STORAGE-SIZE)
    (when (< (getter fd (bytevector->pointer buf) (bytevector->pointer len)) 0)
      (raise-net-error who (errno)))
    (sockaddr-decode (bytevector->pointer buf)))

  (define (unix-listen path #:backlog [backlog SOMAXCONN])
    (define addr (make-net-address path 0 'unix))
    (define sa (sockaddr-of 'unix-listen addr))
    (define fd (new-socket 'unix SOCK_STREAM 'unix-listen))
    (close-fd-on-failure fd
      (lambda ()
        (when (< (c-bind fd (bytevector->pointer sa) (bytevector-length sa)) 0)
          (raise-net-error 'unix-listen (errno)))
        (when (< (c-listen fd backlog) 0)
          (raise-net-error 'unix-listen (errno)))))
    (make-listener fd #f addr #f))

  (define (unix-connect path #:timeout [timeout #f])
    (define deadline (timeout->deadline timeout))
    (define fd
      (connect-socket (make-net-address path 0 'unix) SOCK_STREAM deadline 'unix-connect))
    (make-conn fd #f))

  ;; Datagram sockets

  (define-record-type (packet-conn make-packet-conn packet-conn?)
    (fields (immutable fd packet-conn-fd)
            (mutable closed? %pc-closed? %set-pc-closed!)
            (immutable local packet-conn-local-address)
            (mutable scratch %pc-scratch %set-pc-scratch!)
            (mutable read-deadline %pc-rdl %set-pc-rdl!)
            (mutable write-deadline %pc-wdl %set-pc-wdl!)))

  (define (packet-conn-closed? pc) (%pc-closed? pc))

  (define (packet-conn-set-deadline! pc secs)
    (let ([d (timeout->deadline secs)])
      (%set-pc-rdl! pc d)
      (%set-pc-wdl! pc d)))

  (define (packet-conn-set-read-deadline! pc secs)
    (%set-pc-rdl! pc (timeout->deadline secs)))

  (define (packet-conn-set-write-deadline! pc secs)
    (%set-pc-wdl! pc (timeout->deadline secs)))

  (define (pc-buffer pc n)
    (let ([s (%pc-scratch pc)])
      (if (>= (bytevector-length s) n)
        s
        (let ([new (make-bytevector/nonmoving (max n 2048) 0)])
          (%set-pc-scratch! pc new)
          new))))

  (define (packet-conn-check pc who)
    (when (%pc-closed? pc)
      (raise-net-closed who)))

  (define (make-packet fd)
    (make-packet-conn fd #f (sockname 'udp fd c-getsockname)
                      (make-bytevector/nonmoving 2048 0) #f #f))

  (define (udp-bind host port)
    (define addr (first-address 'udp-bind host port SOCK_DGRAM #t))
    (define sa (sockaddr-of 'udp-bind addr))
    (define fd (new-socket (net-address-family addr) SOCK_DGRAM 'udp-bind))
    (close-fd-on-failure fd
      (lambda ()
        (when (< (c-bind fd (bytevector->pointer sa) (bytevector-length sa)) 0)
          (raise-net-error 'udp-bind (errno)))))
    (make-packet fd))

  (define (udp-connect host port)
    (define addr (first-address 'udp-connect host port SOCK_DGRAM #f))
    (define sa (sockaddr-of 'udp-connect addr))
    (define fd (new-socket (net-address-family addr) SOCK_DGRAM 'udp-connect))
    (close-fd-on-failure fd
      (lambda ()
        (when (< (c-connect fd (bytevector->pointer sa) (bytevector-length sa)) 0)
          (raise-net-error 'udp-connect (errno)))))
    (make-packet fd))

  ;; Sends one datagram. `addr` may be #f on a connected socket.
  (define (send-datagram pc bv addr)
    (packet-conn-check pc 'udp-send-to)
    (define n (bytevector-length bv))
    (define buf (pc-buffer pc n))
    (bytevector-copy! bv 0 buf 0 n)
    (define sa (and addr (sockaddr-of 'udp-send-to addr)))
    (let loop ()
      ;; With a write deadline, wait for writability before sending.
      (when (%pc-wdl pc)
        (await-fd 'udp-send-to (packet-conn-fd pc) 'write (%pc-wdl pc))
        (packet-conn-check pc 'udp-send-to))
      (let ([rc (c-sendto (packet-conn-fd pc) (bytevector->pointer buf) n SEND-FLAGS
                          (if sa (bytevector->pointer sa) %null-pointer)
                          (if sa (bytevector-length sa) 0))])
        (cond
          [(>= rc 0) rc]
          [else
            (let ([e (errno)])
              (cond
                [(= e EINTR) (loop)]
                [(= e EAGAIN)
                 (await-fd 'udp-send-to (packet-conn-fd pc) 'write (%pc-wdl pc))
                 (loop)]
                [(%pc-closed? pc) (raise-net-closed 'udp-send-to)]
                [else (raise-net-error 'udp-send-to e)]))]))))

  (define (udp-send-to pc bv addr)
    (send-datagram pc bv addr))

  (define (udp-send pc bv)
    (send-datagram pc bv #f))

  ;; Receives one datagram into bv. Returns the byte count and the sender's
  ;; address, or #f for the address when `from?` is false.
  (define (receive-datagram pc bv from?)
    (packet-conn-check pc 'udp-recv-from)
    (define n (bytevector-length bv))
    (define buf (pc-buffer pc n))
    (define sa (make-bytevector/nonmoving SOCKADDR-STORAGE-SIZE 0))
    (define len (make-bytevector/nonmoving 4 0))
    (bytevector-u32-native-set! len 0 SOCKADDR-STORAGE-SIZE)
    (let loop ()
      ;; With a read deadline, wait for readability before receiving. The
      ;; flags are 0: MSG_NOSIGNAL is a send-only flag.
      (when (%pc-rdl pc)
        (await-fd 'udp-recv-from (packet-conn-fd pc) 'read (%pc-rdl pc))
        (packet-conn-check pc 'udp-recv-from))
      (let ([rc (c-recvfrom (packet-conn-fd pc) (bytevector->pointer buf) n 0
                            (if from? (bytevector->pointer sa) %null-pointer)
                            (if from? (bytevector->pointer len) %null-pointer))])
        (cond
          [(>= rc 0)
           (bytevector-copy! buf 0 bv 0 rc)
           (values rc (and from? (sockaddr-decode (bytevector->pointer sa))))]
          [else
            (let ([e (errno)])
              (cond
                [(= e EINTR) (loop)]
                [(= e EAGAIN)
                 (await-fd 'udp-recv-from (packet-conn-fd pc) 'read (%pc-rdl pc))
                 (loop)]
                [(%pc-closed? pc) (raise-net-closed 'udp-recv-from)]
                [else (raise-net-error 'udp-recv-from e)]))]))))

  (define (udp-recv-from pc bv)
    (receive-datagram pc bv #t))

  (define (udp-recv pc bv)
    (let-values ([(n addr) (receive-datagram pc bv #f)])
      n))

  (define (packet-conn-close pc)
    (unless (%pc-closed? pc)
      (%set-pc-closed! pc #t)
      (syscall:close (packet-conn-fd pc))
      unspecified-value))

  (define (packet-conn-set-broadcast! pc on?)
    (set-int-option! (packet-conn-fd pc) SOL_SOCKET SO_BROADCAST (if on? 1 0)
                     'packet-conn-set-broadcast!))
)
