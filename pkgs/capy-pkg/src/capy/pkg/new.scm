;;; `capy new` and `capy init`

(library (capy pkg new)
  (export create-package!
    valid-package-name?)
  (import (rnrs)
    (only (capy) create-directory* file-exists?)
    (capy path)
    (capy pkg ui))

  (define (name-char? c)
    (or (char<=? #\a c #\z) (char<=? #\0 c #\9) (memv c '(#\- #\_))))

  (define (valid-package-name? name)
    (and (> (string-length name) 0)
      (char<=? #\a (string-ref name 0) #\z)
      (for-all name-char? (string->list name))))

  (define (write-file! dir relative text)
    (let ([path (path-join dir relative)])
      (cond
        [(file-exists? path) #f]
        [else
          (create-directory* (path-directory path))
          (call-with-output-file path (lambda (port) (put-string port text)))
          #t])))

  (define (manifest-text name lib?)
    (string-append
      "(package\n"
      "  (name " name ")\n"
      "  (version \"0.1.0\")\n"
      "  (edition r6rs)\n"
      (if lib?
        (string-append "  (provides (" name "))\n  (source \"src\"))\n")
        (string-append "  (provides (" name " *))\n  (source \"src\")\n"
          "  (programs (" name " \"src/main.scm\")))\n"))))

  (define (library-text name)
    (string-append
      "(library (" name ")\n"
      "  (export greet)\n"
      "  (import (rnrs))\n"
      "\n"
      "  (define (greet who)\n"
      "    (string-append \"Hello, \" who \"!\")))\n"))

  (define main-text
    (string-append
      "(import (rnrs))\n"
      "\n"
      "(display \"Hello, world!\")\n"
      "(newline)\n"))

  (define (test-text name lib?)
    (if lib?
      (string-append
        "(import (rnrs) (srfi 64) (" name "))\n"
        "\n"
        "(test-begin \"" name "\")\n"
        "(test-equal \"greet\" \"Hello, world!\" (greet \"world\"))\n"
        "(test-end)\n")
      (string-append
        "(import (rnrs) (srfi 64))\n"
        "\n"
        "(test-begin \"" name "\")\n"
        "(test-equal \"arithmetic\" 4 (+ 2 2))\n"
        "(test-end)\n")))

  (define create-package!
    (case-lambda
      [(dir name lib?) (create-package! dir name lib? #t)]
      [(dir name lib? gitignore?) (create-package-files! dir name lib? gitignore?)]))

  (define (create-package-files! dir name lib? gitignore?)
    (unless (valid-package-name? name)
      (fail (string-append "invalid package name `" name
             "`: it starts with a lowercase letter and has lowercase letters, digits, - and _")))
    (when (file-exists? (path-join dir "package.scm"))
      (fail "package.scm exists already in" dir))
    (create-directory* dir)
    (let ([files (list
                  (cons "package.scm" (manifest-text name lib?))
                  (cons (if lib? (string-append "src/" name ".scm") "src/main.scm")
                    (if lib? (library-text name) main-text))
                  (cons "tests/example.scm" (test-text name lib?))
                  (cons ".gitignore" "target/\n"))])
      (let ([files (if gitignore? files (filter (lambda (f) (not (string=? (car f) ".gitignore"))) files))])
      (let loop ([files files] [written '()])
        (cond
          [(null? files) (reverse written)]
          [(write-file! dir (car (car files)) (cdr (car files)))
            (loop (cdr files) (cons (car (car files)) written))]
          [else (loop (cdr files) written)]))))))
