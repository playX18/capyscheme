#!r6rs

(library (capy lsp worker)
  (export dispatch-json)
  (import
    (capy)
    (rnrs)
    (rnrs io ports)
    (srfi 180)
    (capy session)
    (capy lsp analysis))

  (define current-root #f)
  (define current-load-path '())
  (define shutdown-requested? #f)

  (define key-root-uri (string->symbol "rootUri"))
  (define key-root-path (string->symbol "rootPath"))
  (define key-workspace-root (string->symbol "workspaceRoot"))
  (define key-load-path (string->symbol "loadPath"))
  (define key-text-document (string->symbol "textDocument"))
  (define key-open-documents (string->symbol "openDocuments"))

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

  (define (json-string-or-null value)
    (if (string? value) value 'null))

  (define (json-id value)
    (cond
      [(or (string? value) (number? value) (boolean? value)) value]
      [(eq? value 'null) 'null]
      [else 'null]))

  (define (json-version value)
    (cond
      [(or (string? value) (number? value)) value]
      [else 'null]))

  (define (json-vector->string-list value)
    (cond
      [(vector? value)
       (let loop ((i 0) (out '()))
         (if (= i (vector-length value))
             (reverse out)
             (let ((item (vector-ref value i)))
               (loop (+ i 1)
                     (if (string? item) (cons item out) out)))))]
      [(string? value) (list value)]
      [else '()]))

  (define (json-open-documents->overrides value)
    (cond
      [(vector? value)
       (let loop ((i 0) (out '()))
         (if (= i (vector-length value))
             (reverse out)
             (let* ((item (vector-ref value i))
                    (path (and (json-object? item)
                               (first-string (json-ref item 'path #f))))
                    (text (and (json-object? item)
                               (first-string (json-ref item 'text #f)
                                             (json-ref item 'source #f)))))
               (loop (+ i 1)
                     (if (and path text)
                         (cons (cons path text) out)
                         out)))))]
      [else '()]))

  (define (first-string . values)
    (let loop ((values values))
      (cond
        [(null? values) #f]
        [(string? (car values)) (car values)]
        [else (loop (cdr values))])))

  (define (safe-module-filename name)
    (guard (exn [else #f])
      (module-name->filename name)))

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

  (define (current-session-info)
    `((root . ,(json-string-or-null current-root))
      (loadPath . ,(list->vector current-load-path))
      (sessionLibrary . ,(json-string-or-null (safe-module-filename '(capy session))))))

  (define (initialize-root id params request)
    (let* ((root (first-string
                   (json-ref params 'root #f)
                   (json-ref params key-root-uri #f)
                   (json-ref params key-root-path #f)
                   (json-ref params key-workspace-root #f)
                   (json-ref request 'root #f)
                   (json-ref request key-root-uri #f)))
           (load-path (json-vector->string-list
                        (json-ref params key-load-path
                                  (json-ref params 'load-path #f)))))
      (when root
        (set! current-root root))
      (set! current-load-path load-path)
      (success-response
        id
        "initialize-root"
        `((ready . #t)
          (analysisEngine . "syntax")
          (treeIlExpansion . #f)
          (session . ,(current-session-info))))))

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

  (define (invalidate-files-request id params request)
    (let* ((paths (json-vector->string-list
                    (json-ref params 'paths
                              (json-ref params 'files
                                        (json-ref request 'paths #())))))
           (overrides (json-open-documents->overrides
                        (json-ref params key-open-documents
                                  (json-ref params 'documents #())))))
      (success-response
        id
        "invalidate-files"
        (invalidate-files paths overrides))))

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
            [(string=? method "initialize-root")
             (initialize-root id (if (json-object? params) params '()) request)]
            [(string=? method "analyze-document")
             (analyze-document-request id (if (json-object? params) params '()) request)]
            [(string=? method "run-action")
             (run-action-request id (if (json-object? params) params '()) request)]
            [(string=? method "invalidate-files")
             (invalidate-files-request id (if (json-object? params) params '()) request)]
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
