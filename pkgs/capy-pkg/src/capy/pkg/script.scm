;;; Script mode
(library (capy pkg script)
  (export script-dependencies
    prepare-script!
    script-package-name)
  (import (rnrs)
    (only (capy) file-exists? create-directory* canonicalize-path-string)
    (capy crypto sha256)
    (capy path)
    (capy pkg build)
    (capy pkg git)
    (capy pkg manifest)
    (capy pkg ui)
    (capy pkg util))

  ;; The name of the package the frontmatter makes; it is the view the script runs in.
  (define script-package-name 'capy-script)

  ;;; Reading the frontmatter

  ;; The first 64 KiB of the file, or #f when it cannot be read as text.
  (define (read-head path)
    (guard (e [#t #f])
      (call-with-input-file path
        (lambda (port)
          (let ([text (get-string-n port 65536)])
            (if (eof-object? text) "" text))))))

  (define opener "#|capy")

  (define (starts-with? text prefix start)
    (let ([n (string-length prefix)])
      (and (<= (+ start n) (string-length text))
        (string=? (substring text start (+ start n)) prefix))))

  ;; The text between `#|capy` and its closing `|#`, or #f if the file does not start with the
  ;; block (a `#!` line may come first). Blocks nest, as in the reader.
  (define (frontmatter-text path text)
    (let* ([start (if (starts-with? text "#!" 0)
                   (let skip ([i 0])
                     (cond [(>= i (string-length text)) i]
                       [(char=? (string-ref text i) #\newline) (+ i 1)]
                       [else (skip (+ i 1))]))
                   0)]
           [n (string-length text)]
           [after (+ start (string-length opener))])
      (and (starts-with? text opener start)
        (< after n)
        (char-whitespace? (string-ref text after))
        (let loop ([i after] [depth 1])
          (cond
            [(>= (+ i 1) n)
              (fail (string-append path ": the #|capy block is not closed (or is longer than 64 KiB)"))]
            [(starts-with? text "|#" i)
              (if (= depth 1) (substring text after i) (loop (+ i 2) (- depth 1)))]
            [(starts-with? text "#|" i) (loop (+ i 2) (+ depth 1))]
            [else (loop (+ i 1) depth)])))))

  (define (read-datums text path)
    (guard (e [(and (message-condition? e) (not (driver-error? e)))
                (fail (string-append path ": the #|capy block cannot be read: " (condition-message e)))])
      (let ([port (open-string-input-port text)])
        (let loop ([out '()])
          (let ([x (read port)])
            (if (eof-object? x) (reverse out) (loop (cons x out))))))))

  ;; The dependency specs of the frontmatter of the script PATH, as `(NAME CLAUSE ...)` lists with
  ;; their paths made absolute; '() when the script has no frontmatter or no dependencies.
  (define (script-dependencies path)
    (let* ([head (read-head path)]
           [block (and head (frontmatter-text path head))])
      (if (not block)
        '()
        (let ([forms (read-datums block path)]
              [directory (path-directory (canonicalize-path-string path))])
          (for-each
            (lambda (form)
              (unless (and (pair? form) (eq? (car form) 'dependencies) (list? form))
                (fail (string-append path ": the #|capy block holds (dependencies SPEC ...), not")
                  (call-with-string-output-port (lambda (p) (write form p))))))
            forms)
          (when (and (pair? forms) (pair? (cdr forms)))
            (fail (string-append path ": the #|capy block has two (dependencies ...) forms")))
          (if (null? forms)
            '()
            (map (lambda (spec) (absolute-paths spec directory)) (cdr (car forms))))))))

  ;; A `(path "DIR")` clause of SPEC is relative to the script: it is made absolute.
  (define (absolute-paths spec directory)
    (if (and (pair? spec) (list? spec))
      (cons (car spec)
        (map (lambda (clause)
              (if (and (pair? clause) (eq? (car clause) 'path) (pair? (cdr clause)) (string? (cadr clause)))
                (list 'path (canonicalize-path-string (path-join directory (cadr clause))))
                clause))
          (cdr spec)))
      spec))

  ;;; The package of a script

  (define (written datum)
    (call-with-string-output-port (lambda (port) (write datum port))))

  (define (scripts-directory)
    (path-join (capy-cache-directory) "scripts"))

  (define (manifest-text dependencies)
    (string-append
      ";; Made by `capy` for a script with a #|capy block. Do not edit.\n"
      "(package (name " (symbol->string script-package-name) ") (version \"0.0.0\") (edition r6rs)\n"
      "  (provides (capy-script *)) (source \"src\")\n"
      "  (dependencies"
      (apply string-append (map (lambda (spec) (string-append "\n    " (written spec))) dependencies))
      "))\n"))

  (define (write-text! path text)
    (unless (and (file-exists? path) (string=? text (call-with-input-file path get-string-all)))
      (write-file-atomically! path (lambda (port) (put-string port text)))))

;; The manifest is checked on its own first, so that a mistake in the block is reported as one.
  (define (check-manifest! script manifest-path)
    (guard (e [(and (message-condition? e) (not (driver-error? e)))
                (let* ([message (condition-message e)]
                       [prefix (string-append manifest-path ": ")]
                       [message (if (starts-with? message prefix 0)
                                  (substring message (string-length prefix) (string-length message))
                                  message)])
                  (fail (string-append script ": the dependencies in the #|capy block are wrong: " message)))])
      (read-manifest manifest-path)))

  ;; Builds the dependencies of the script PATH (see `script-dependencies`: not '()) and returns the
  ;; `build-result`. SETTINGS-FOR is a procedure from the root directory to the build settings.
  (define (prepare-script! path dependencies build-settings-for)
    (let* ([text (manifest-text dependencies)]
           [hash (substring (bytevector->hex (sha256 (string->utf8 (written dependencies)))) 0 32)]
           [directory (path-join (scripts-directory) hash)])
      (create-directory* (path-join directory "src"))
      (write-text! (path-join directory "package.scm") text)
      (check-manifest! path (path-join directory "package.scm"))
      (let ([result (build! directory (build-settings-for directory))])
        (values result directory)))))
