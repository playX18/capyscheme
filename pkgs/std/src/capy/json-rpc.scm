;;; JSON-RPC 2.0 messages with the language server protocol's base framing.
;;;
;;; A frame is `Content-Length: N` CRLF (other headers are accepted and ignored) CRLF and then N
;;; bytes of UTF-8 JSON. `read-frame` and `write-frame` work on binary ports, where N counts the
;;; bytes read, and on textual ports, where it counts the UTF-8 bytes of the characters read.
;;;
;;;   (write-json-rpc-message out (make-request 1 "initialize" '((processId . null))))
;;;   (read-json-rpc-message in)   => a request, notification, response or error-response, or eof
;;;
;;; JSON values are those of (capy json): an object is an alist with symbol keys (`()` is the
;;; empty object), an array is a vector, `null` is the symbol 'null; `json-ref` is the one of
;;; (capy json). An id is an integer or a
;;; string; params are an object or an array, or #f when absent. Malformed frames, JSON and
;;; messages raise a `&json-rpc-error` carrying the JSON-RPC error code, so a server can answer
;;; with `json-rpc-error->response`.
;;;
;;; Cancellation: a client cancels the request with id I by sending the notification
;;; `$/cancelRequest` with params {id: I} (`make-cancel-notification`); `cancel-notification-id`
;;; reads the id back, and `make-request-cancelled-response` is the answer for a request that
;;; was cancelled.

(library (capy json-rpc)
  (export read-frame
    write-frame
    read-json-rpc-message
    write-json-rpc-message
    json->bytes
    bytes->json
    json-ref
    ;; messages
    make-request request? request-id request-method request-params
    make-notification notification? notification-method notification-params
    make-response response? response-id response-result
    make-error-response error-response? error-response-id error-response-error
    json-rpc-message?
    message->json
    json->message
    ;; errors
    &json-rpc-error make-json-rpc-error json-rpc-error?
    json-rpc-error-code json-rpc-error-message json-rpc-error-data
    json-rpc-error->response
    json-rpc-parse-error
    json-rpc-invalid-request
    json-rpc-method-not-found
    json-rpc-invalid-params
    json-rpc-internal-error
    json-rpc-server-not-initialized
    json-rpc-unknown-error-code
    json-rpc-request-failed
    json-rpc-server-cancelled
    json-rpc-content-modified
    json-rpc-request-cancelled
    ;; cancellation and ids
    make-cancel-notification
    cancel-notification-id
    make-request-cancelled-response
    make-id-generator)
  (import (rnrs)
    (capy json))

  ;; Error codes: JSON-RPC 2.0 and the language server protocol.
  (define json-rpc-parse-error -32700)
  (define json-rpc-invalid-request -32600)
  (define json-rpc-method-not-found -32601)
  (define json-rpc-invalid-params -32602)
  (define json-rpc-internal-error -32603)
  (define json-rpc-server-not-initialized -32002)
  (define json-rpc-unknown-error-code -32001)
  (define json-rpc-request-failed -32803)
  (define json-rpc-server-cancelled -32802)
  (define json-rpc-content-modified -32801)
  (define json-rpc-request-cancelled -32800)

  (define-condition-type &json-rpc-error &error
    make-json-rpc-error json-rpc-error?
    (code json-rpc-error-code)
    (message json-rpc-error-message)
    (data json-rpc-error-data))

  (define (fail code message . data)
    (raise (make-json-rpc-error code message (if (null? data) #f (car data)))))

  ;; ---- Framing ----

  (define max-header-line 8192)

  ;; The next header line of PORT without its terminator, or eof.
  (define (read-header-line port)
    (if (binary-port? port)
      (let loop ([bytes '()] [n 0])
        (let ([b (get-u8 port)])
          (cond
            [(eof-object? b)
              (if (null? bytes) b (fail json-rpc-parse-error "end of input inside a header"))]
            [(= b 10) (strip-cr (utf8->string (u8-list->bytevector (reverse bytes))))]
            [(> n max-header-line) (fail json-rpc-parse-error "header line too long")]
            [else (loop (cons b bytes) (+ n 1))])))
      (let loop ([chars '()] [n 0])
        (let ([c (get-char port)])
          (cond
            [(eof-object? c)
              (if (null? chars) c (fail json-rpc-parse-error "end of input inside a header"))]
            [(char=? c #\newline) (strip-cr (list->string (reverse chars)))]
            [(> n max-header-line) (fail json-rpc-parse-error "header line too long")]
            [else (loop (cons c chars) (+ n 1))])))))

  (define (strip-cr line)
    (let ([n (string-length line)])
      (if (and (> n 0) (char=? (string-ref line (- n 1)) #\return))
        (substring line 0 (- n 1))
        line)))

  (define (string-trim-spaces s)
    (let loop ([i 0] [j (string-length s)])
      (cond
        [(and (< i j) (memv (string-ref s i) '(#\space #\tab))) (loop (+ i 1) j)]
        [(and (< i j) (memv (string-ref s (- j 1)) '(#\space #\tab))) (loop i (- j 1))]
        [else (substring s i j)])))

  ;; The Content-Length of a header block, read from PORT up to the empty line; eof when the port
  ;; is at its end.
  (define (read-headers port)
    (let loop ([length #f] [first? #t])
      (let ([line (read-header-line port)])
        (cond
          [(eof-object? line)
            (if first? line (fail json-rpc-parse-error "end of input inside the headers"))]
          [(string=? line "")
            (or length (fail json-rpc-parse-error "missing Content-Length header"))]
          [else
            (let ([colon (let find ([i 0])
                          (cond [(= i (string-length line)) #f]
                            [(char=? (string-ref line i) #\:) i]
                            [else (find (+ i 1))]))])
              (unless colon
                (fail json-rpc-parse-error "malformed header" line))
              (let ([name (string-trim-spaces (substring line 0 colon))]
                    [value (string-trim-spaces (substring line (+ colon 1) (string-length line)))])
                (if (string-ci=? name "content-length")
                  (let ([n (and (> (string-length value) 0)
                             (for-all char-numeric? (string->list value))
                             (string->number value))])
                    (unless n (fail json-rpc-parse-error "invalid Content-Length" value))
                    (loop n #f))
                  (loop length #f))))]))))

  (define (read-body-bytes port n)
    (let ([bv (make-bytevector n)])
      (let loop ([filled 0])
        (if (= filled n)
          bv
          (let ([chunk (get-bytevector-n port (- n filled))])
            (when (or (eof-object? chunk) (= (bytevector-length chunk) 0))
              (fail json-rpc-parse-error "end of input inside a message body"))
            (bytevector-copy! chunk 0 bv filled (bytevector-length chunk))
            (loop (+ filled (bytevector-length chunk))))))))

  (define (utf8-length ch)
    (let ([n (char->integer ch)])
      (cond [(<= n #x7F) 1] [(<= n #x7FF) 2] [(<= n #xFFFF) 3] [else 4])))

  (define (read-body-chars port n)
    (let loop ([bytes 0] [chars '()])
      (cond
        [(= bytes n) (string->utf8 (list->string (reverse chars)))]
        [(> bytes n) (fail json-rpc-parse-error "Content-Length ends inside a character")]
        [else
          (let ([c (get-char port)])
            (when (eof-object? c)
              (fail json-rpc-parse-error "end of input inside a message body"))
            (loop (+ bytes (utf8-length c)) (cons c chars)))])))

  ;; The body of the next frame of PORT as a bytevector, or eof at the end of the input.
  (define (read-frame port)
    (let ([n (read-headers port)])
      (if (eof-object? n)
        n
        (if (binary-port? port)
          (read-body-bytes port n)
          (read-body-chars port n)))))

  ;; Writes BODY (a bytevector, or a string that is written as its UTF-8 bytes) as a frame.
  (define (write-frame port body)
    (let* ([body (if (string? body) (string->utf8 body) body)]
           [header (string-append "Content-Length: " (number->string (bytevector-length body))
                    "\r\n\r\n")])
      (if (binary-port? port)
        (begin
          (put-bytevector port (string->utf8 header))
          (put-bytevector port body))
        (begin
          (put-string port header)
          (put-string port (utf8->string body))))
      (flush-output-port port)))

  ;; ---- JSON ----
  ;;
  ;; The data model is spelled out at each call, so that a program that sets the parameters of
  ;; (capy json) for its own use does not change what the messages are made of.

  ;; The UTF-8 text of the JSON value JSON.
  (define (json->bytes json)
    (json->bytevector json #:null 'null))

  ;; The JSON value of the UTF-8 text BV; raises a parse error when it is not JSON.
  (define (bytes->json bv)
    (guard (e [(json-error? e)
               (fail json-rpc-parse-error
                 (if (message-condition? e) (condition-message e) "invalid JSON"))])
      (bytevector->json bv #:object 'alist #:array 'vector #:key 'symbol #:null 'null)))

  ;; ---- Messages ----

  (define-record-type (<request> make-request request?)
    (fields (immutable id request-id)
      (immutable method request-method)
      (immutable params request-params)))

  (define-record-type (<notification> make-notification notification?)
    (fields (immutable method notification-method)
      (immutable params notification-params)))

  (define-record-type (<response> make-response response?)
    (fields (immutable id response-id)
      (immutable result response-result)))

  ;; ERROR is a JSON object ((code . n) (message . "...") [(data . v)]).
  (define-record-type (<error-response> %make-error-response error-response?)
    (fields (immutable id error-response-id)
      (immutable error error-response-error)))

  (define make-error-response
    (case-lambda
      [(id code message) (make-error-response id code message #f)]
      [(id code message data)
        (%make-error-response id
          (if data
            `((code . ,code) (message . ,message) (data . ,data))
            `((code . ,code) (message . ,message))))]))

  (define (json-rpc-message? x)
    (or (request? x) (notification? x) (response? x) (error-response? x)))

  (define (valid-id? id)
    (or (string? id) (and (integer? id) (exact? id)) (eq? id 'null)))

  (define (message->json m)
    (cond
      [(request? m)
        `((jsonrpc . "2.0") (id . ,(request-id m)) (method . ,(request-method m))
          ,@(if (request-params m) `((params . ,(request-params m))) '()))]
      [(notification? m)
        `((jsonrpc . "2.0") (method . ,(notification-method m))
          ,@(if (notification-params m) `((params . ,(notification-params m))) '()))]
      [(response? m)
        `((jsonrpc . "2.0") (id . ,(response-id m)) (result . ,(response-result m)))]
      [(error-response? m)
        `((jsonrpc . "2.0") (id . ,(error-response-id m)) (error . ,(error-response-error m)))]
      [else (assertion-violation 'message->json "not a JSON-RPC message" m)]))

  (define (valid-params? p)
    (or (not p) (vector? p) (null? p) (and (pair? p) (pair? (car p)))))

  ;; The message that the JSON value J encodes; raises an invalid-request error if it is none.
  (define (json->message j)
    (unless (or (null? j) (and (pair? j) (pair? (car j))))
      (fail json-rpc-invalid-request "a message must be a JSON object"))
    (unless (equal? (json-ref j 'jsonrpc) "2.0")
      (fail json-rpc-invalid-request "jsonrpc must be \"2.0\""))
    (let ([has-id? (assq 'id j)]
          [id (json-ref j 'id)]
          [method (json-ref j 'method)]
          [params (let ([p (json-ref j 'params)]) (if (eq? p 'null) #f p))])
      (when (and has-id? (not (valid-id? id)))
        (fail json-rpc-invalid-request "invalid id" id))
      (cond
        [method
          (unless (string? method) (fail json-rpc-invalid-request "method must be a string"))
          (unless (valid-params? params)
            (fail json-rpc-invalid-request "params must be an object or an array"))
          (if has-id?
            (make-request id method params)
            (make-notification method params))]
        [(assq 'error j)
          (let ([e (json-ref j 'error)])
            (unless (and (exact-int? (json-ref e 'code)) (string? (json-ref e 'message)))
              (fail json-rpc-invalid-request "invalid error object"))
            (%make-error-response (if has-id? id 'null) e))]
        [(assq 'result j)
          (unless has-id? (fail json-rpc-invalid-request "a response needs an id"))
          (make-response id (json-ref j 'result))]
        [else (fail json-rpc-invalid-request "neither a request, a notification nor a response")])))

  (define (exact-int? x) (and (integer? x) (exact? x)))

  ;; The next message of PORT, or eof at the end of the input.
  (define (read-json-rpc-message port)
    (let ([body (read-frame port)])
      (if (eof-object? body)
        body
        (json->message (bytes->json body)))))

  (define (write-json-rpc-message port message)
    (write-frame port (json->bytes (message->json message))))

  ;; The error response for a raised &json-rpc-error (ID is the request's id, 'null if unknown).
  (define (json-rpc-error->response id e)
    (make-error-response id (json-rpc-error-code e) (json-rpc-error-message e)
      (json-rpc-error-data e)))

  ;; ---- Cancellation and ids ----

  (define (make-cancel-notification id)
    (make-notification "$/cancelRequest" `((id . ,id))))

  ;; The id that the notification M cancels, or #f if M is not a `$/cancelRequest`.
  (define (cancel-notification-id m)
    (and (notification? m)
      (string=? (notification-method m) "$/cancelRequest")
      (let ([id (json-ref (notification-params m) 'id)])
        (and (valid-id? id) id))))

  (define (make-request-cancelled-response id)
    (make-error-response id json-rpc-request-cancelled "request cancelled"))

  ;; A thunk that returns 1, 2, 3, ... for the ids of requests sent.
  (define (make-id-generator)
    (let ([n 0])
      (lambda () (set! n (+ n 1)) n)))
  )
