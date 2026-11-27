(import (rnrs)
  (srfi 64)
  (capy json-rpc))

(test-begin "capy json-rpc")

(define (bytevector-append . bvs)
  (let-values ([(out get) (open-bytevector-output-port)])
    (for-each (lambda (b) (put-bytevector out b)) bvs)
    (get)))

(define (frame-bytes str)
  (let ([body (string->utf8 str)])
    (bytevector-append
      (string->utf8 (string-append "Content-Length: " (number->string (bytevector-length body)) "\r\n\r\n"))
      body)))

;; A binary port that hands out at most N bytes per read.
(define (trickle-port bv n)
  (let ([pos 0])
    (make-custom-binary-input-port "trickle"
      (lambda (buf start count)
        (let ([k (min n count (- (bytevector-length bv) pos))])
          (bytevector-copy! bv pos buf start k)
          (set! pos (+ pos k))
          k))
      #f #f #f)))

(define (bytes-port str) (open-bytevector-input-port (string->utf8 str)))

(define (raises-code? thunk)
  (guard (e [(json-rpc-error? e) (json-rpc-error-code e)] [#t 'other])
    (thunk)
    #f))

(test-group "framing"
  (test-equal "write-frame to a binary port"
    (string->utf8 "Content-Length: 2\r\n\r\n{}")
    (let-values ([(out get) (open-bytevector-output-port)])
      (write-frame out (string->utf8 "{}"))
      (get)))
  (test-equal "binary round trip"
    (string->utf8 "{\"a\":1}")
    (let-values ([(out get) (open-bytevector-output-port)])
      (write-frame out (string->utf8 "{\"a\":1}"))
      (read-frame (open-bytevector-input-port (get)))))
  (test-equal "length counts bytes, not characters"
    "Content-Length: 6\r\n\r\nh\xE9;llo"
    (call-with-string-output-port
      (lambda (out) (write-frame out "h\xE9;llo"))))
  (test-equal "textual round trip with multibyte characters"
    (string->utf8 "{\"s\":\"\x3bb;\x1F600;\"}")
    (let ([text (call-with-string-output-port
                 (lambda (out) (write-frame out "{\"s\":\"\x3bb;\x1F600;\"}")))])
      (read-frame (open-string-input-port text))))
  (test-equal "two frames, then eof"
    (list (string->utf8 "[1]") (string->utf8 "[2,3]") #t)
    (let* ([port (open-bytevector-input-port
                  (bytevector-append (frame-bytes "[1]") (frame-bytes "[2,3]")))]
           [a (read-frame port)]
           [b (read-frame port)])
      (list a b (eof-object? (read-frame port)))))
  (test-equal "partial reads, one byte at a time"
    (list (string->utf8 "{\"k\":\"\x3bb;\"}") (string->utf8 "[]"))
    (let* ([port (trickle-port (bytevector-append (frame-bytes "{\"k\":\"\x3bb;\"}") (frame-bytes "[]")) 1)]
           [a (read-frame port)])
      (list a (read-frame port))))
  (test-equal "partial reads, three bytes at a time"
    (string->utf8 "{\"key\":\"value\"}")
    (read-frame (trickle-port (frame-bytes "{\"key\":\"value\"}") 3)))
  (test-equal "other headers are ignored, name is case-insensitive"
    (string->utf8 "{}")
    (read-frame (bytes-port "content-length: 2\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}")))
  (test-assert "eof on empty input"
    (eof-object? (read-frame (open-bytevector-input-port (make-bytevector 0)))))
  (test-equal "missing Content-Length" json-rpc-parse-error
    (raises-code? (lambda () (read-frame (bytes-port "Foo: 1\r\n\r\n{}")))))
  (test-equal "bad Content-Length" json-rpc-parse-error
    (raises-code? (lambda () (read-frame (bytes-port "Content-Length: abc\r\n\r\n{}")))))
  (test-equal "header without a colon" json-rpc-parse-error
    (raises-code? (lambda () (read-frame (bytes-port "Content-Length 2\r\n\r\n{}")))))
  (test-equal "truncated body" json-rpc-parse-error
    (raises-code? (lambda () (read-frame (bytes-port "Content-Length: 10\r\n\r\n{}")))))
  (test-equal "truncated headers" json-rpc-parse-error
    (raises-code? (lambda () (read-frame (bytes-port "Content-Length: 2\r\n"))))))

(test-group "messages"
  (define (round-trip m)
    (let-values ([(out get) (open-bytevector-output-port)])
      (write-json-rpc-message out m)
      (read-json-rpc-message (open-bytevector-input-port (get)))))
  (let ([r (round-trip (make-request 7 "initialize" '((processId . 42) (rootUri . null))))])
    (test-assert "request" (request? r))
    (test-equal "request id" 7 (request-id r))
    (test-equal "request method" "initialize" (request-method r))
    (test-equal "request params" 42 (json-ref (request-params r) 'processId)))
  (let ([r (round-trip (make-request "abc" "shutdown" #f))])
    (test-equal "string id" "abc" (request-id r))
    (test-equal "no params" #f (request-params r)))
  (let ([n (round-trip (make-notification "textDocument/didOpen"
                        '((textDocument . ((uri . "file:///a") (text . "\x3bb;\x1F600;"))))))])
    (test-assert "notification" (and (notification? n) (not (request? n))))
    (test-equal "unicode survives"
      "\x3bb;\x1F600;"
      (json-ref (json-ref (notification-params n) 'textDocument) 'text)))
  (let ([r (round-trip (make-response 3 '((capabilities . ()))))])
    (test-assert "response" (response? r))
    (test-equal "response id" 3 (response-id r))
    (test-equal "empty object result" '((capabilities . ())) (response-result r)))
  (test-equal "null result" 'null (response-result (round-trip (make-response 4 'null))))
  (test-equal "array result" (vector 1 "two" 'null)
    (response-result (round-trip (make-response 5 (vector 1 "two" 'null)))))
  (let ([e (round-trip (make-error-response 9 json-rpc-method-not-found "no such method" '((method . "x"))))])
    (test-assert "error response" (error-response? e))
    (test-equal "error id" 9 (error-response-id e))
    (test-equal "error code" -32601 (json-ref (error-response-error e) 'code))
    (test-equal "error message" "no such method" (json-ref (error-response-error e) 'message))
    (test-equal "error data" "x" (json-ref (json-ref (error-response-error e) 'data) 'method)))
  (test-equal "standard codes"
    '(-32700 -32600 -32601 -32602 -32603 -32800)
    (list json-rpc-parse-error json-rpc-invalid-request json-rpc-method-not-found
      json-rpc-invalid-params json-rpc-internal-error json-rpc-request-cancelled))
  (test-assert "eof"
    (eof-object? (read-json-rpc-message (open-bytevector-input-port (make-bytevector 0)))))
  (test-equal "invalid JSON" json-rpc-parse-error
    (raises-code? (lambda () (read-json-rpc-message (open-bytevector-input-port (frame-bytes "{not json"))))))
  (test-equal "wrong version" json-rpc-invalid-request
    (raises-code? (lambda ()
                   (read-json-rpc-message
                     (open-bytevector-input-port (frame-bytes "{\"jsonrpc\":\"1.0\",\"method\":\"x\"}"))))))
  (test-equal "not an object" json-rpc-invalid-request
    (raises-code? (lambda () (read-json-rpc-message (open-bytevector-input-port (frame-bytes "[1,2]"))))))
  (test-equal "no method, result or error" json-rpc-invalid-request
    (raises-code? (lambda ()
                   (read-json-rpc-message
                     (open-bytevector-input-port (frame-bytes "{\"jsonrpc\":\"2.0\",\"id\":1}"))))))
  (test-equal "error to response"
    '(-32602 "bad")
    (let* ([e (guard (e [(json-rpc-error? e) e])
               (raise (make-json-rpc-error json-rpc-invalid-params "bad" #f)))]
           [r (json-rpc-error->response 1 e)])
      (list (json-ref (error-response-error r) 'code) (json-ref (error-response-error r) 'message)))))

(test-group "cancellation and ids"
  (test-equal "cancel id" 12 (cancel-notification-id (make-cancel-notification 12)))
  (test-equal "string cancel id" "r1" (cancel-notification-id (make-cancel-notification "r1")))
  (test-equal "cancel id survives the wire" 0
    (let-values ([(out get) (open-bytevector-output-port)])
      (write-json-rpc-message out (make-cancel-notification 0))
      (cancel-notification-id (read-json-rpc-message (open-bytevector-input-port (get))))))
  (test-equal "other notification" #f (cancel-notification-id (make-notification "exit" #f)))
  (test-equal "cancelled response code" -32800
    (json-ref (error-response-error (make-request-cancelled-response 3)) 'code))
  (let ([next (make-id-generator)])
    (test-equal "ids" '(1 2 3) (list (next) (next) (next)))))

(test-end)
