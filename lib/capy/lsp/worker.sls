#!r6rs

(library (capy lsp worker)
  (export dispatch-json)
  (import
    (capy)
    (rnrs)
    (rnrs io ports)
    (srfi 180)
    (capy lsp analysis))

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

  (define (string-vector->list value)
    (cond
      [(vector? value)
       (let loop ((i 0) (out '()))
         (cond
           [(= i (vector-length value)) (reverse out)]
           [(string? (vector-ref value i))
            (loop (+ i 1) (cons (vector-ref value i) out))]
           [else #f]))]
      [(list? value)
       (let loop ((value value) (out '()))
         (cond
           [(null? value) (reverse out)]
           [(string? (car value)) (loop (cdr value) (cons (car value) out))]
           [else #f]))]
      [else #f]))

  (define (string-list->symbols value)
    (let ((strings (string-vector->list value)))
      (and strings
           (let loop ((strings strings) (out '()))
             (if (null? strings)
                 (reverse out)
                 (loop (cdr strings)
                       (cons (string->symbol (car strings)) out)))))))

  (define (read-module-name text)
    (guard (_ [else #f])
      (let ((datum (read (open-input-string text))))
        (cond
          [(and (pair? datum) (eq? (car datum) 'quote))
           (let ((name (cadr datum)))
             (and (symbol-list? name) name))]
          [(symbol-list? datum) datum]
          [else #f]))))

  (define (symbol-list? value)
    (and (list? value)
         (let loop ((value value))
           (or (null? value)
               (and (symbol? (car value))
                    (loop (cdr value)))))))

  (define (config-module-name value)
    (cond
      [(string? value) (read-module-name value)]
      [else (string-list->symbols value)]))

  (define (config-ref params request key)
    (json-ref params key (json-ref request key #f)))

  (define (result-with-config-echo result params request)
    (let ((workspace-epoch (config-ref params request 'workspaceEpoch))
          (config-fingerprint (config-ref params request 'configFingerprint)))
      (append result
              (if workspace-epoch
                  `((workspaceEpoch . ,workspace-epoch))
                  '())
              (if config-fingerprint
                  `((configFingerprint . ,config-fingerprint))
                  '()))))

  (define (string-list->vector values)
    (list->vector
      (let loop ((values values) (out '()))
        (cond
          [(null? values) (reverse out)]
          [(string? (car values)) (loop (cdr values) (cons (car values) out))]
          [else (loop (cdr values) out)]))))

  (define (call-with-worker-config params request thunk)
    (let ((old-load-path %load-path)
          (old-compiled-load-path %load-compiled-path)
          (old-load-extensions %load-extensions)
          (old-module (current-module))
          (load-path (string-vector->list (config-ref params request 'loadPath)))
          (compiled-load-path
            (string-vector->list (config-ref params request 'compiledLoadPath)))
          (extensions (string-vector->list (config-ref params request 'extensions)))
          (default-module (config-module-name (config-ref params request 'defaultModule))))
      (dynamic-wind
        (lambda ()
          (when load-path
            (set! %load-path (append load-path old-load-path)))
          (when compiled-load-path
            (set! %load-compiled-path
                  (append compiled-load-path old-compiled-load-path)))
          (when extensions
            (set! %load-extensions extensions))
          (when default-module
            (current-module (resolve-module default-module #f #t))))
        thunk
        (lambda ()
          (set! %load-path old-load-path)
          (set! %load-compiled-path old-compiled-load-path)
          (set! %load-extensions old-load-extensions)
          (current-module old-module)))))

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
            (call-with-worker-config
              params
              request
              (lambda ()
                (result-with-config-echo
                  (analyze-document uri text version path)
                  params
                  request)))))))

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
           (call-with-worker-config
             params
             request
             (lambda ()
               (result-with-config-echo
                 (run-document-action uri text version path action range)
                 params
                 request))))])))

  (define (load-path-request id params request)
    (success-response
      id
      "load-path"
      (call-with-worker-config
        params
        request
        (lambda ()
          `((loadPath . ,(string-list->vector %load-path))
            (extensions . ,(string-list->vector %load-extensions)))))))

  (define (shutdown id)
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
            [(string=? method "load-path")
             (load-path-request id (if (json-object? params) params '()) request)]
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
