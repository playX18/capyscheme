;; A wrapper for easy invocation of C compiler.

(define-library (cc)
  (import (scheme base)
    (core io))

  (begin

    (define (parse-command-line-flags flags)
      (string-split flags #\space))

    (define (parse-command-line-flags-list strings)
      (apply append (map parse-command-line-flags strings)))

    (define *cc* (make-parameter #f))
    (define *cc-flags* (make-parameter '()))
    (define *ld* (make-parameter #f))
    (define *ld-flags* (make-parameter '()))
    (define *ld-exe-flags* (make-parameter '()))
    (define *ld-dll-flags* (make-parameter '()))
    (define *linkkit-start* (make-parameter '()))
    (define *linkkit-end* (make-parameter '()))

    (define (link-all-library lib)
      (if lib
        (cond-expand
          [macos (`("-Wl,-force-load" ,lib))]
          [else `("-Wl,--whole-archive" ,lib "-Wl,--no-whole-archive")])
        '()))

    (define (default-toolchain-parameters)
      (*cc* (or (getenv "CC") "gcc"))
      (*cc-flags* (parse-command-line-flags (or (getenv "CC_FLAGS") "")))
      (*ld* (*cc*))
      (*ld-flags* (parse-command-line-flags (or (getenv "LD_FLAGS") "")))
      (*ld-exe-flags* (parse-command-line-flags (or (getenv "LD_EXE_FLAGS") "")))
      (*ld-dll-flags*
        `(,(cond-expand
            [macos "-dynamiclib"]
            [else "-shared"])
          ,@(*cc-flags*))))

    (default-toolchain-parameters)

    (call-with-temporary-output
      "/tmp/cc-"
      (lambda (port tmp)
        (format #t "tmp at ~a~%" tmp)
        (rename-file tmp "file.c")))
    (rename-file "test" "test.c")))
