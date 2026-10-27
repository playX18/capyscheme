# Networking

`(capy net)` provides TCP, UDP and Unix-domain sockets. All the procedures are non-blocking 
and will yield a coroutine if result is not awailable straight away. 

## TCP

```scheme
(import (rnrs) (capy concurrency) (capy net))

(define (echo conn)
  (let ([buf (make-bytevector 4096)])
    (let loop ()
      (let ([n (conn-read! conn buf)])
        (when (> n 0)
          (conn-write-all conn buf 0 n)
          (loop))))
    (conn-close conn)))

(define listener (tcp-listen "127.0.0.1" 7070))
(display "echoing on ") (display (address->string (listener-address listener))) (newline)
(flush-output-port (current-output-port))

(let loop ()
  (let ([conn (listener-accept listener)])
    (go (lambda () (guard (e [#t (if #f #f)]) (echo conn))))
    (loop)))
```

Run it with `capy -s echo.scm` and try `printf 'hello\n' | nc 127.0.0.1 7070`.

The main procedures:

| Procedure | Purpose |
|-----------|---------|
| `(tcp-listen host port #:backlog #:reuse-addr? #:reuse-port?)` | Bind and listen. Port 0 picks a free port. Read it with `listener-address`. |
| `(listener-accept l)` | Wait for the next connection, returns a `conn`. |
| `(listener-close l)`, `(listener-closed? l)` | Stop listening. Threads blocked in `listener-accept` see `&net-closed`. |
| `(tcp-connect host port #:timeout secs)` | Resolve, connect, return a `conn`. `#:timeout` bounds the whole connect. |
| `(conn-read! c bv [start end])` | Read into `bv`. Returns the byte count, `0` at end of stream. |
| `(conn-read c n)` | Read at most `n` bytes into a new bytevector, or `eof`. |
| `(conn-write-all c bv [start end])` | Write everything or raise. |
| `(conn-close c)`, `(conn-closed? c)` | Close. |
| `(conn-local-address c)`, `(conn-remote-address c)` | `net-address` records,`address->string` formats them. |
| `(conn-set-nodelay! c on?)`, `(conn-set-keepalive! c on?)` | TCP socket options. |
| `(conn-set-rcvbuf! c n)`, `(conn-set-sndbuf! c n)` | Socket buffer sizes. |
| `(conn-shutdown c how)` | Half-close: `how` is `'read`, `'write` or `'both`. |
| `(conn->binary-input-port c)`, `(conn->binary-output-port c)` | Standard Scheme binary ports over a connection. |

`conn-read!` returns as soon as some bytes are available. It does not fill the
buffer. Use `conn-write-all` on the writing side, since a single write may send
fewer bytes than asked.

## Deadlines

A deadline is an absolute limit on how long an operation may wait. Set one on a
connection with `conn-set-read-deadline!`, `conn-set-write-deadline!`, or
`conn-set-deadline!` (both directions). Each takes a number of seconds from now,
or `#f` to clear the deadline. The deadline applies to every later operation
until it is changed.

```scheme
(import (rnrs) (capy net))

(define listener (tcp-listen "127.0.0.1" 0))
(define port (net-address-port (listener-address listener)))

(define client (tcp-connect "127.0.0.1" port))
(define server-side (listener-accept listener))

;; The server never writes, so the client's read times out.
(conn-set-read-deadline! client 0.2)

(guard (e [(net-timeout? e) (display "read timed out\n")])
  (conn-read client 16)
  (display "unexpected data\n"))

(conn-close client)
(conn-close server-side)
(listener-close listener)
```

`#:timeout` on `tcp-connect` and `unix-connect` works the same way

## UDP

UDP sockets are packet connections. `udp-bind` gives a socket for receiving,
`udp-connect` one for sending to a fixed peer, and `udp-send-to` sends to an
address you pass.

```scheme
(import (rnrs) (capy net))

(define receiver (udp-bind "127.0.0.1" 0))
(define port (net-address-port (packet-conn-local-address receiver)))

(define sender (udp-bind "127.0.0.1" 0))
(define target (parse-address (string-append "127.0.0.1:" (number->string port))))
(udp-send-to sender (string->utf8 "ping") target)

(define buf (make-bytevector 64))
(let-values ([(n from) (udp-recv-from receiver buf)])
  (let ([got (make-bytevector n)])
    (bytevector-copy! buf 0 got 0 n)
    (display (utf8->string got))
    (newline)))

(packet-conn-close sender)
(packet-conn-close receiver)
```

`udp-recv-from` returns the byte count and the sender's address. `udp-recv`
returns only the count. A datagram larger than the buffer is truncated.

## Unix-domain sockets

`unix-listen` and `unix-connect` take a file path:

```scheme
(import (rnrs) (capy concurrency) (capy net))

(define path "demo.sock")
(when (file-exists? path) (delete-file path))

(define listener (unix-listen path))
(go (lambda ()
      (let ([conn (listener-accept listener)])
        (conn-write-all conn (string->utf8 "from the server\n"))
        (conn-close conn))))

(define client (unix-connect path))
(display (utf8->string (conn-read client 64)))
(conn-close client)
(listener-close listener)
(delete-file path)
```

## Errors

Socket failures raise a condition whose type says what happened. Test for the
type with the predicates below:

| Predicate | Raised when |
|-----------|-------------|
| `net-error?`, `net-error-errno` | Any other OS error. `errno` is the number. |
| `net-timeout?` | A deadline passed while waiting. |
| `net-closed?` | The connection or listener was closed, by you or by another thread, while an operation was waiting. |

Closing a connection wakes every thread waiting on it with `&net-closed`, so
a reader that is blocked in `conn-read!` in one thread can be stopped by
closing the connection from another.
