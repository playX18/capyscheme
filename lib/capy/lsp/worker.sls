#!r6rs

(library (capy lsp worker)
  (export dispatch-json)
  (import
    (capy)
    (rnrs)
    (rnrs io ports)
    (srfi 180)
    (capy lsp analysis))

  (define shutdown-requested? #f)

  (define key-text-document (string->symbol "textDocument"))

  (define (write-to-string obj)
    (let ((port (open-output-string)))
      (write obj port)
      (get-output-string port)))

  (define (condition->message exn)
    (cond
      [(and (condition? exn) (message-condition? exn))
       (condition-message exn)]
      [else (write-to-string exn)]))

  (define (json-object? value)
    (or (null? value)
        (and (list? value)
             (let loop ((value value))
               (or (null? value)
                   (and (pair? (car value))
                        (symbol? (caar value))
                        (loop (cdr value))))))))

  (define (json-ref object key default)
    (if (json-object? object)
        (cond
          [(assq key object) => cdr]
          [else default])
        default))

  (define (json-id value)
    (cond
      [(or (string? value) (number? value) (boolean? value)) value]
      [(eq? value 'null) 'null]
      [else 'null]))

  (define (json-version value)
    (cond
      [(or (string? value) (number? value)) value]
      [else 'null]))

  (define (first-string . values)
    (let loop ((values values))
      (cond
        [(null? values) #f]
        [(string? (car values)) (car values)]
        [else (loop (cdr values))])))

  (define (json->string obj)
    (let ((port (open-output-string)))
      (json-write obj port)
      (get-output-string port)))

  (define (json-escape-string text)
    (let ((port (open-output-string)))
      (let loop ((i 0))
        (when (< i (string-length text))
          (let ((ch (string-ref text i)))
            (cond
              [(char=? ch #\\) (display "\\\\" port)]
              [(char=? ch #\") (display "\\\"" port)]
              [(char=? ch #\newline) (display "\\n" port)]
              [(char=? ch #\return) (display "\\r" port)]
              [(char=? ch #\tab) (display "\\t" port)]
              [else (write-char ch port)])
            (loop (+ i 1)))))
      (get-output-string port)))

  (define (fallback-error-json message)
    (string-append
      "{\"ok\":false,\"id\":null,\"error\":{\"code\":\"internal-error\",\"message\":\"worker failed to serialize response: "
      (json-escape-string message)
      "\"}}"))

  (define (safe-json->string obj)
    (guard (exn [else (fallback-error-json (condition->message exn))])
      (json->string obj)))

  (define (success-response id method result)
    (safe-json->string
      `((ok . #t)
        (id . ,(json-id id))
        (method . ,method)
        (result . ,result))))

  (define (error-response id code message)
    (safe-json->string
      `((ok . #f)
        (id . ,(json-id id))
        (error . ((code . ,code)
                  (message . ,message))))))

  (define (invalid-params id message)
    (error-response id "invalid-params" message))

  (define (document-params params request)
    (let ((document (json-ref params 'document
                              (json-ref params key-text-document params))))
      (if (json-object? document) document params)))

  (define (analyze-document-request id params request)
    (let* ((document (document-params params request))
           (text (first-string
                   (json-ref document 'text #f)
                   (json-ref document 'source #f)
                   (json-ref params 'text #f)
                   (json-ref params 'source #f)
                   (json-ref request 'text #f)
                   (json-ref request 'source #f)))
           (uri (first-string
                  (json-ref document 'uri #f)
                  (json-ref params 'uri #f)
                  (json-ref request 'uri #f)))
           (path (first-string
                   (json-ref document 'path #f)
                   (json-ref params 'path #f)
                   (json-ref request 'path #f)))
           (version (json-version
                      (json-ref document 'version
                                (json-ref params 'version
                                          (json-ref request 'version 'null))))))
      (if (not text)
          (invalid-params id "analyze-document requires a string text or source field")
          (success-response
            id
            "analyze-document"
            (analyze-document uri text version path)))))

  (define (run-action-request id params request)
    (let* ((document (document-params params request))
           (text (first-string
                   (json-ref document 'text #f)
                   (json-ref document 'source #f)
                   (json-ref params 'text #f)
                   (json-ref params 'source #f)
                   (json-ref request 'text #f)
                   (json-ref request 'source #f)))
           (uri (first-string
                  (json-ref document 'uri #f)
                  (json-ref params 'uri #f)
                  (json-ref request 'uri #f)))
           (path (first-string
                   (json-ref document 'path #f)
                   (json-ref params 'path #f)
                   (json-ref request 'path #f)))
           (version (json-version
                      (json-ref document 'version
                                (json-ref params 'version
                                          (json-ref request 'version 'null)))))
           (action (first-string
                     (json-ref params 'action #f)
                     (json-ref request 'action #f)))
           (range (json-ref params 'range (json-ref request 'range #f))))
      (cond
        [(not text)
         (invalid-params id "run-action requires a string text or source field")]
        [(not action)
         (invalid-params id "run-action requires a string action field")]
        [else
         (success-response
           id
           "run-action"
           (run-document-action uri text version path action range))])))

  (define (shutdown id)
    (set! shutdown-requested? #t)
    (success-response
      id
      "shutdown"
      `((shutdown . #t))))

  (define (dispatch-request request)
    (if (not (json-object? request))
        (error-response 'null "invalid-request" "request must be a JSON object")
        (let* ((id (json-ref request 'id 'null))
               (method (json-ref request 'method #f))
               (params (json-ref request 'params '())))
          (cond
            [(not (string? method))
             (error-response id "invalid-request" "request method must be a string")]
            [(and (not (json-object? params)) (not (eq? params 'null)))
             (invalid-params id "request params must be an object when provided")]
            [(string=? method "analyze-document")
             (analyze-document-request id (if (json-object? params) params '()) request)]
            [(string=? method "run-action")
             (run-action-request id (if (json-object? params) params '()) request)]
            [(string=? method "shutdown")
             (shutdown id)]
            [else
             (error-response
               id
               "unknown-method"
               (string-append "unknown method: " method))]))))

  (define (parse-request input)
    (guard (exn
             [(json-error? exn)
              (error-response
                'null
                "invalid-json"
                (string-append "malformed JSON: " (json-error-reason exn)))]
             [else
              (error-response
                'null
                "invalid-json"
                (string-append "malformed JSON: " (condition->message exn)))])
      (dispatch-request (json-read (open-input-string input)))))

  (define (dispatch-json input)
    (guard (exn
             [else
              (error-response
                'null
                "internal-error"
                (condition->message exn))])
      (if (not (string? input))
          (error-response 'null "invalid-request" "dispatch-json expects one string argument")
          (parse-request input)))))
