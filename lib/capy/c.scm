;; (c-include "stdio.h" option ...)
;;
;; Parses the header with the host C compiler's view of the world, generates a Scheme
;; library for it (cached, see (capy c driver)) and imports it at the point of use.
;;
;;   (import (capy c))
;;   (c-include "math.h" #:link '("m"))
;;   (sqrt 2.0)
;;
;; Option values are read at expansion time, so they must be literal data (a leading
;; quote is fine).
;;
;;   #:link '("m")            extra libraries to search, beyond the process itself
;;   #:include-dirs '(...)    extra -I directories
;;   #:defines '(("N" . "V") "FLAG")   -D definitions; a bare string defines it as empty
;;   #:std "c23"              language standard passed to the compiler probe
;;   #:quote-include #t       search like #include "x.h" instead of <x.h>
;;   #:only-from '("a.h")     export declarations from these headers instead of just HEADER
;;   #:include '("str*")      export only these names (a trailing * is a prefix match)
;;   #:exclude '("_*")        never export these names
;;   #:rename '(("old" . "new"))
;;   #:strip-prefix '("SDL_") / #:add-prefix "c:"
;;   #:blocking #t | #f | '("sleep")   which functions may block (run on a blocking-pool
;;                            thread). Default: a built-in list of well-known blocking libc
;;                            calls; everything else runs on the calling thread, so GUI
;;                            libraries work after (pin (main-worker)) or on the main thread
;;   #:prefix c:              import the library under this prefix; without it the
;;                            names are imported as-is, and a name already bound at top
;;                            level keeps its old binding
;;   #:frameworks '("CoreFoundation")   macOS frameworks to link
;;   #:pkg-config "sdl2"      take the include directories, libraries and macros from
;;                            `pkg-config --cflags --libs sdl2` (needs pkg-config)
;;   #:string-returns #t      `char *` results become strings (NULL -> #f); default #f
;;
;; Generated names, for a declaration NAME:
;;   NAME                    a function, a constant, an enumerator or a typedef
;;   NAME-address            (global variables) a thunk giving the address
;;   NAME, NAME-set!         (global scalar variables) read and write the value
;;   make-NAME               (function-pointer typedefs) a Scheme procedure -> callback
;;   NAME->procedure         (function-pointer typedefs) a function pointer -> procedure
;;   NAME-names, NAME->symbol, symbol->NAME, NAME-flags->list   (enums)
;;   NAME-string, NAME-string-set!                          (char arrays in structs)
;;
;; A function parameter of a function-pointer type takes a Scheme procedure. The callback
;; made for it lives only for that call and is freed afterwards, so a C function that keeps
;; the pointer (a registered signal handler, say) must be given a callback made with
;; make-NAME instead, and that callback freed with free-callback-pointer when no longer used.
;;
;; (with-c-out ((p int)) body ...) gives scratch memory for the body; (c-out-ref p int)
;; reads a value stored in it, so a C out-parameter can be filled and read back.
;;
;; (c-include-report) returns the diagnostics of the last generation: what was skipped and
;; why.

(library (capy c)
  (export c-include c-include-library c-cache-directory c-use-cache-directory! c-native-available?
    c-include-report with-c-out c-out-ref)
  (import (rnrs) (capy) (capy c driver) (capy c runtime))

  (define (unquote-literal d)
    (if (and (pair? d) (eq? (car d) 'quote) (pair? (cdr d)))
      (cadr d)
      d))

  ;; ((#:key value) ...) -> alist of (symbol . datum)
  (define (parse-options who options)
    (let loop ([rest options] [acc '()])
      (cond
        [(null? rest) (reverse acc)]
        [(and (keyword? (car rest)) (pair? (cdr rest)))
          (loop (cddr rest)
            (cons (cons (keyword->symbol (car rest)) (unquote-literal (cadr rest))) acc))]
        [else (syntax-violation who "expected #:keyword value pairs" options)])))

  (define-syntax c-include
    (lambda (stx)
      (syntax-case stx ()
        [(k header option ...)
          (let ([name (syntax->datum #'header)]
                [options (parse-options 'c-include (syntax->datum #'(option ...)))])
            (unless (string? name)
              (syntax-violation 'c-include "header name must be a string" stx #'header))
            (let* ([prefix (let ([p (assq 'prefix options)]) (and p (cdr p)))]
                   [config (filter (lambda (o) (not (eq? (car o) 'prefix))) options)]
                   [library (c-include-library name config)])
              (with-syntax ([cache (datum->syntax #'k (c-cache-directory))]
                            [spec (datum->syntax #'k (if prefix `(prefix ,library ,prefix) library))])
                #'(begin
                    (c-use-cache-directory! cache)
                    (import spec)
                    (if #f #f)))))]))))
