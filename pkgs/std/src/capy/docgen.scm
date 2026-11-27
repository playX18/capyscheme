(library (capy docgen)
  (export document-modules docgen-main)
  (import (core) (core sorting) (capy) (srfi 13) (srfi 180))

  ;; Extract structured documentation for libraries.
  ;;
  ;; Walks explicitly requested modules at runtime (so native procedures,
  ;; whose documentation is attached as closure metadata by capy-derive,
  ;; are covered) and merges in `;;` comment blocks read from the
  ;; libraries' source files (module headers and per-definition comments,
  ;; which never reach the runtime: the expander drops the library doc
  ;; string and comments are not values).

  (define (join-strings parts sep)
    (if (null? parts)
      ""
      (let loop ((out (car parts)) (rest (cdr parts)))
        (if (null? rest)
          out
          (loop (string-append out sep (car rest)) (cdr rest))))))

  (define (split-lines str)
    (let ((len (string-length str)))
      (let loop ((i 0) (start 0) (out '()))
        (if (>= i len)
          (reverse (cons (substring str start len) out))
          (if (char=? (string-ref str i) #\newline)
            (loop (+ i 1) (+ i 1) (cons (substring str start i) out))
            (loop (+ i 1) start out))))))

  (define (trailing-space? c)
    (or (char=? c #\space) (char=? c #\newline)
        (char=? c #\tab) (char=? c #\return)))

  (define (rtrim str)
    (let loop ((end (string-length str)))
      (if (zero? end)
        ""
        (if (trailing-space? (string-ref str (- end 1)))
          (loop (- end 1))
          (substring str 0 end)))))

  (define (trim str)
    (let ((len (string-length str)))
      (let left ((i 0))
        (if (or (>= i len) (not (trailing-space? (string-ref str i))))
          (rtrim (substring str i len))
          (left (+ i 1))))))


  ;; capy-derive appends the same GC-contract footer to every native
  ;; procedure's documentation. It is identical on all of them, so it
  ;; carries no per-procedure information and is dropped here. Returns
  ;; the user-written part, or the whole string when no footer is found.
  (define native-footer-markers
    (list "# GC contract (`#[scheme]`)" "# Safety (`#[scheme(unsafe)]`)"))

  (define (strip-native-footer doc)
    (let loop ((markers native-footer-markers))
      (if (null? markers)
        doc
        (let ((pos (string-contains doc (car markers))))
          (if pos
            (rtrim (substring doc 0 pos))
            (loop (cdr markers)))))))

  ;; Rust `///` lines keep one leading space in the doc attribute.
  (define (strip-one-leading-space line)
    (if (and (> (string-length line) 0)
             (char=? (string-ref line 0) #\space))
      (substring line 1 (string-length line))
      line))

  (define (clean-native-doc doc)
    (let ((cleaned (join-strings (map strip-one-leading-space
                                      (split-lines (strip-native-footer doc)))
                                 "\n")))
      (if (equal? (trim cleaned) "") 'null cleaned)))

  (define (segment? x)
    (or (symbol? x) (and (integer? x) (exact? x))))

  (define (segment->string x)
    (if (symbol? x) (symbol->string x) (number->string x)))

  ;; "(core lists)" -> (core lists); anything else -> #f.
  (define (parse-module-path str)
    (guard (c (#t #f))
      (let ((datum (read (open-input-string str))))
        (let loop ((rest datum))
          (cond ((not (list? datum)) #f)
                ((null? datum) #f)
                ((null? rest) datum)
                ((segment? (car rest)) (loop (cdr rest)))
                (else #f))))))

  (define (module-relpath mod-name)
    (string-append (join-strings (map segment->string mod-name) "/") ".scm"))

  ;; First load-path entry holding the module's file, or #f for
  ;; programmatically defined modules (e.g. (capy) itself).
  (define (module-file-path mod-name)
    (let ((rel (module-relpath mod-name)))
      (let loop ((dirs (if (list? %load-path) %load-path '())))
        (if (null? dirs)
          #f
          (let ((cand (string-append (car dirs) "/" rel)))
            (if (file-exists? cand)
              cand
              (loop (cdr dirs))))))))

  ;; A full-line `;;` comment's text, or #f when the line is not one.
  (define (comment-text line)
    (let ((len (string-length line)))
      (let skip ((i 0))
        (if (>= i len)
          #f
          (let ((c (string-ref line i)))
            (if (or (char=? c #\space) (char=? c #\tab))
              (skip (+ i 1))
              (if (and (char=? c #\;)
                       (< (+ i 1) len)
                       (char=? (string-ref line (+ i 1)) #\;))
                (let strip ((j (+ i 2)))
                  (cond ((and (< j len) (char=? (string-ref line j) #\;))
                         (strip (+ j 1)))
                        ((and (< j len) (char=? (string-ref line j) #\space))
                         (substring line (+ j 1) len))
                        (else (substring line j len))))
                #f)))))))

  ;; A single-line string literal's content, e.g. a library docstring.
  (define (string-literal-line line)
    (let ((t (trim line)))
      (let ((n (string-length t)))
        (if (and (>= n 2)
                 (char=? (string-ref t 0) #\")
                 (char=? (string-ref t (- n 1)) #\"))
          (substring t 1 (- n 1))
          #f))))

  (define (blank-line? line)
    (let ((len (string-length line)))
      (let loop ((i 0))
        (if (>= i len)
          #t
          (let ((c (string-ref line i)))
            (if (or (char=? c #\space) (char=? c #\tab) (char=? c #\return))
              (loop (+ i 1))
              #f))))))

  ;; `  (define (name ...`, `  (define name ...`, or
  ;; `  (define-syntax name ...` -> name, else #f.
  ;; Only `define` and `define-syntax` are covered: other defining forms
  ;; (record types, define-values) create bindings the runtime already
  ;; reports, and guessing their generated names from text would
  ;; misattribute comments.
  (define (define-target-name line)
    (let ((len (string-length line)))
      ;; Read a token starting at/after i; returns (token . end) or #f.
      (define (token-at i)
        (let blanks ((k i))
          (if (and (< k len)
                   (let ((c (string-ref line k)))
                     (or (char=? c #\space) (char=? c #\tab))))
            (blanks (+ k 1))
            (let chars ((k k) (buf '()))
              (if (or (>= k len)
                      (let ((ch (string-ref line k)))
                        (or (char=? ch #\space) (char=? ch #\tab)
                            (char=? ch #\() (char=? ch #\)))))
                (and (pair? buf) (cons (list->string (reverse buf)) k))
                (chars (+ k 1) (cons (string-ref line k) buf)))))))
      (let skip ((i 0))
        (if (>= i len)
          #f
          (let ((c (string-ref line i)))
            (cond ((or (char=? c #\space) (char=? c #\tab)) (skip (+ i 1)))
                  ((not (char=? c #\()) #f)
                  (else
                   (let ((kw (token-at (+ i 1))))
                     (and kw
                          (or (equal? (car kw) "define")
                              (equal? (car kw) "define-syntax"))
                          (let ((nm (if (and (< (cdr kw) len)
                                             (char=? (string-ref line (cdr kw))
                                                     #\())
                                        ;; `(define (name ...`
                                        (token-at (+ (cdr kw) 1))
                                        ;; `(define name ...`
                                        (token-at (cdr kw)))))
                            (and nm (string->symbol (car nm)))))))))))))

  ;; Returns ((module-doc . string-or-#f) (defs . ((name . doc) ...))).
  ;; The first comment block (or single-line docstring) before the first
  ;; form documents the module; a comment block directly above a
  ;; `(define ...)` line documents that binding. Blank lines end a block.
  (define (scan-file path)
    (define first-block #f)
    (define docstring #f)
    (define defs '())
    (define pending '())
    (define seen-form? #f)
    ;; Comments preceding the first form document the module (the
    ;; usual header style, blank lines included). Later blocks belong to
    ;; the definition beneath them.
    (define (note-block!)
      (when (and (not seen-form?) (pair? pending))
        (set! first-block (join-strings (reverse pending) "\n"))))
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (let ((line (read-line in)))
            (if (eof-object? line)
              (begin (note-block!) (set! pending '()))
              (let ((text (comment-text line)))
                (cond (text
                       (set! pending (cons text pending))
                       (loop))
                      ;; Blank lines never break a block: a comment
                      ;; paragraph, a blank line, then the definition is
                      ;; the usual style. A non-comment line still ends
                      ;; the block (attaching it or dropping it).
                      ((blank-line? line)
                       (note-block!)
                       (loop))
                      (else
                       (let ((lit (and (not seen-form?)
                                       (string-literal-line line)))
                             (name (define-target-name line)))
                         (note-block!)
                         (when (and lit (not docstring))
                           (set! docstring lit))
                         (when (and name
                                    (pair? pending)
                                    (not (assq name defs)))
                           (set! defs
                                 (cons (cons name
                                             (join-strings
                                              (reverse pending) "\n"))
                                       defs)))
                         (set! seen-form? #t)
                         (set! pending '())
                         (loop))))))))))
    (list (cons 'module-doc (or docstring first-block)) (cons 'defs defs)))

  (define (visited? mod-name visited)
    (let loop ((rest visited))
      (cond ((null? rest) #f)
            ((equal? (car rest) mod-name) #t)
            (else (loop (cdr rest))))))

  ;; A value that no exported binding can have, so a real #f is not
  ;; mistaken for a missing export.
  (define unbound-sentinel (list 'unbound))

  ;; Exported (name . value) pairs of the public interface of ENV, walked
  ;; directly. R6RS and R7RS libraries keep their exports in the
  ;; environment; only some libraries expose them through the instance.
  (define (env-binding-values env)
    (let ((out '()))
      (let ((public (guard (c (#t #f)) (env-public-interface env))))
        (when public
          (env-for-each
           (lambda (name var)
             (when (and (symbol? name)
                        (guard (c (#t #f)) (variable-bound? var)))
               (let ((value (guard (c (#t unbound-sentinel))
                              (variable-ref var))))
                 (unless (eq? value unbound-sentinel)
                   (set! out (cons (cons name value) out))))))
           public)))
      out))

  (define (instance-binding-values inst)
    (let ((out '()))
      (for-each
       (lambda (name)
         (when (symbol? name)
           (let ((value (guard (c (#t unbound-sentinel))
                          (instance-ref inst name unbound-sentinel))))
             (unless (eq? value unbound-sentinel)
               (set! out (cons (cons name value) out))))))
       (instance-symbols inst))
      out))

  (define (procedure-source-alist proc)
    (let ((src (and (procedure? proc) (procedure-sourcev proc))))
      (if (and (vector? src) (>= (vector-length src) 3)
               (string? (vector-ref src 0)))
        (list (cons 'file (vector-ref src 0))
              (cons 'line (vector-ref src 1))
              (cons 'column (vector-ref src 2)))
        'null)))

  (define (procedure-kind proc src-alist)
    (cond ((macro? proc) "syntax")
          ((not (procedure? proc)) "value")
          (else
           (let ((file (and (pair? src-alist) (cdr (assq 'file src-alist)))))
             (if (and (string? file)
                      (let ((n (string-length file)))
                        (and (>= n 3) (equal? (substring file (- n 3) n) ".rs"))))
               "native-procedure"
               "scheme-procedure")))))

  (define (export-doc kind value static-defs name)
    (let ((rt-doc (and (procedure? value) (procedure-documentation value))))
      (cond ((and (string? rt-doc) (equal? kind "native-procedure"))
             (clean-native-doc rt-doc))
            ((string? rt-doc) rt-doc)
            (else
             (let ((found (assq name static-defs)))
               (if found (cdr found) 'null))))))

  ;; --- Signatures -------------------------------------------------------
  ;;
  ;; The runtime does not store lambda lists, so a signature is read from
  ;; the defining form at the source location capy already records. Scheme
  ;; definitions keep their keyword and optional entries; native
  ;; procedures get the parameter names of the Rust function. Both paths
  ;; fall back to (name ...) when the source cannot be parsed.

  (define source-lines-cache '())

  (define (reset-source-cache!)
    (set! source-lines-cache '()))

  (define (file-lines path)
    (let ((hit (assoc path source-lines-cache)))
      (if hit
        (cdr hit)
        (let ((lines (guard (c (#t '()))
                      (call-with-input-file path
                        (lambda (in)
                          (let loop ((out '()))
                            (let ((line (read-line in)))
                              (if (eof-object? line)
                                (reverse out)
                                (loop (cons line out))))))))))
          (set! source-lines-cache (cons (cons path lines) source-lines-cache))
          lines))))

  (define (lines-from n lines)
    (let loop ((i 1) (rest lines))
      (cond ((null? rest) '())
            ((>= i n) rest)
            (else (loop (+ i 1) (cdr rest))))))

  ;; A definition and its parameter list fit in a few lines. Capping the
  ;; read keeps signature extraction linear on files with many exports.
  (define (take-lines n lines)
    (let loop ((i 0) (rest lines) (out '()))
      (cond ((or (>= i n) (null? rest)) (reverse out))
            (else (loop (+ i 1) (cdr rest) (cons (car rest) out))))))

  (define (write-string x)
    (call-with-string-output-port (lambda (p) (write x p))))

  (define (default-signature name)
    (string-append "(" name " ...)"))

  ;; --- A small reader for definition headers ---------------------------
  ;;
  ;; Reading the whole `(define ...)` with `read` would pull in the body,
  ;; which can be hundreds of lines. These helpers read only the tokens
  ;; of the header: the name and the parameter list, including keyword
  ;; entries like #:greeting [g "hello"].

  (define (ws-char? c)
    (or (char=? c #\space) (char=? c #\newline)
        (char=? c #\tab) (char=? c #\return)))

  (define (token-delimiter? c)
    (or (ws-char? c)
        (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\|))))

  (define (skip-ws text i)
    (let loop ((i i))
      (cond
        ((>= i (string-length text)) i)
        ((ws-char? (string-ref text i)) (loop (+ i 1)))
        ((char=? (string-ref text i) #\;)
         (let comment ((j (+ i 1)))
           (if (or (>= j (string-length text))
                   (char=? (string-ref text j) #\newline))
             (loop j)
             (comment (+ j 1)))))
        (else i))))

  (define (read-atom text i)
    (let loop ((j i))
      (cond
        ((>= j (string-length text)) (cons (substring text i j) j))
        ((token-delimiter? (string-ref text j)) (cons (substring text i j) j))
        (else (loop (+ j 1))))))

  (define (read-quoted text i quote-char)
    (let loop ((j (+ i 1)))
      (cond
        ((>= j (string-length text)) (cons (substring text i j) j))
        ((char=? (string-ref text j) #\\) (loop (+ j 2)))
        ((char=? (string-ref text j) quote-char)
         (cons (substring text i (+ j 1)) (+ j 1)))
        (else (loop (+ j 1))))))

  (define (read-list text i)
    (let loop ((j (+ i 1)) (items '()))
      (let ((j (skip-ws text j)))
        (cond
          ((>= j (string-length text)) #f)
          (else
           (let ((c (string-ref text j)))
             (if (or (char=? c #\)) (char=? c #\]) (char=? c #\}))
               (cons (reverse items) (+ j 1))
               (let ((el (read-element text j)))
                 (if el
                   (loop (cdr el) (cons (car el) items))
                   #f)))))))))

  (define (read-element text i)
    (let ((i (skip-ws text i)))
      (cond
        ((>= i (string-length text)) #f)
        (else
         (let ((c (string-ref text i)))
           (cond
             ((or (char=? c #\() (char=? c #\[) (char=? c #\{))
              (read-list text i))
             ((or (char=? c #\") (char=? c #\|))
              (read-quoted text i c))
             (else (read-atom text i))))))))

  (define (keyword-token? x)
    (and (string? x) (starts-with? x "#:")))

  (define (starts-with? str prefix)
    (let ((pos (string-contains str prefix)))
      (and pos (= pos 0))))

  (define (define-form-line? line)
    (let ((t (trim line)))
      (or (starts-with? t "(define-syntax")
          (and (starts-with? t "(define")
               (let ((n (string-length t)))
                 (or (= n 7)
                     (let ((c (string-ref t 7)))
                       (or (char=? c #\space)
                           (char=? c #\tab)
                           (char=? c #\()))))))))

  ;; A procedure's recorded line is usually the lambda or the first body
  ;; line, not the `(define ...` that owns it. Walk back a short window
  ;; to find the enclosing definition.
  (define (define-line-before line lines)
    (let* ((start (max 1 (- line 40)))
           (window (take-lines (+ (- line start) 1) (lines-from start lines))))
      (let loop ((ls (reverse window)) (n line))
        (cond ((null? ls) #f)
              ((define-form-line? (car ls)) n)
              (else (loop (cdr ls) (- n 1)))))))

  (define (render-token x after-keyword?)
    (cond
      ((string? x) x)
      ((list? x)
       (string-append (if after-keyword? "[" "(")
                      (render-tokens x)
                      (if after-keyword? "]" ")")))
      (else (write-string x))))

  (define (render-tokens tokens)
    (let loop ((rest tokens) (prev #f) (out '()))
      (cond
        ((null? rest) (join-strings (reverse out) " "))
        (else
         (let ((x (car rest)))
           (loop (cdr rest)
                 x
                 (cons (render-token x (and prev (keyword-token? prev))) out)))))))

  ;; The (name . signature) of the form at LINE, parsed from its header.
  (define (scheme-signature-at file line)
    (let* ((text (join-strings (take-lines 12 (lines-from line (file-lines file)))
                               "\n"))
           (i (skip-ws text 0)))
      (if (or (>= i (string-length text))
              (not (char=? (string-ref text i) #\()))
        #f
        (let* ((kw (read-atom text (skip-ws text (+ i 1))))
               (keyword (if kw (car kw) ""))
               (j (if kw (skip-ws text (cdr kw)) 0)))
          (if (not (or (string=? keyword "define")
                       (string=? keyword "define-syntax")))
            #f
            (let ((target (read-element text j)))
              (if (not target)
                #f
                (let ((value (car target))
                      (after (if (list? (car target)) j (skip-ws text (cdr target)))))
                  (cond
                    ((list? value)
                     (if (and (pair? value) (string? (car value)))
                       (cons (car value)
                             (string-append "(" (render-tokens value) ")"))
                       #f))
                    ((not (string? value)) #f)
                    ((string=? keyword "define-syntax")
                     (cons value (default-signature value)))
                    ((and (< after (string-length text))
                          (char=? (string-ref text after) #\())
                     (let* ((head (read-atom text (skip-ws text (+ after 1))))
                            (lambda-name (if head (car head) "")))
                       (if (string=? lambda-name "lambda")
                         (let ((params (read-element
                                        text (skip-ws text (cdr head)))))
                           (if (and params (list? (car params)))
                             (cons value
                                   (string-append
                                    "(" (render-tokens
                                         (cons value (car params)))
                                    ")"))
                             (cons value (default-signature value))))
                         (cons value (default-signature value)))))
                    (else (cons value (default-signature value))))))))))))

  (define signature-map-cache '())

  ;; All signatures defined in FILE, keyed by name. Macros have no runtime
  ;; source vector, so this is how their keyword signatures are found.
  (define (module-signature-map path)
    (let ((hit (assoc path signature-map-cache)))
      (if hit
        (cdr hit)
        (let ((sig (let loop ((ls (file-lines path)) (n 1) (out '()))
                     (cond ((null? ls) (reverse out))
                           ((define-form-line? (car ls))
                            (let ((entry (scheme-signature-at path n)))
                              (loop (cdr ls) (+ n 1)
                                    (if entry (cons entry out) out))))
                           (else (loop (cdr ls) (+ n 1) out))))))
          (set! signature-map-cache
                (cons (cons path sig) signature-map-cache))
          sig))))

  (define (scheme-signature name file line)
    (let* ((define-line (or (define-line-before line (file-lines file)) line))
           (entry (and define-line (scheme-signature-at file define-line))))
      (if (and entry (string=? (car entry) name))
        (cdr entry)
        (default-signature name))))

  (define (string-find-char str char start)
    (let loop ((i start) (len (string-length str)))
      (cond ((>= i len) #f)
            ((char=? (string-ref str i) char) i)
            (else (loop (+ i 1) len)))))

  (define (matching-paren text open)
    (let loop ((i (+ open 1)) (depth 1))
      (cond
        ((>= i (string-length text)) #f)
        ((char=? (string-ref text i) #\()
         (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref text i) #\))
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth)))))

  (define (split-top-level str sep)
    (let loop ((i 0) (start 0) (depth 0) (out '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (trim (substring str start i)) out)))
        (else
         (let ((c (string-ref str i)))
           (cond
             ((memv c '(#\( #\[ #\{ #\<)) (loop (+ i 1) start (+ depth 1) out))
             ((memv c '(#\) #\] #\} #\>)) (loop (+ i 1) start (max 0 (- depth 1)) out))
             ((and (char=? c sep) (= depth 0))
              (loop (+ i 1) (+ i 1) depth
                    (cons (trim (substring str start i)) out)))
             (else (loop (+ i 1) start depth out))))))))

  (define (last-token str)
    (let loop ((parts (reverse (map (lambda (x) x)
                                    (split-top-level str #\space))))
               (fallback ""))
      (if (null? parts)
        fallback
        (if (string=? (car parts) "")
          (loop (cdr parts) fallback)
          (car parts)))))

  (define (rust-param-name raw index)
    (let* ((colon (string-find-char raw #\: 0))
           (before (trim (if colon (substring raw 0 colon) raw)))
           (name (last-token before)))
      (cond
        ((or (string=? name "") (string=? name "_"))
         (string-append "arg" (number->string index)))
        (else (join-strings (split-on-char name #\_) "-")))))

  (define (split-on-char str char)
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (substring str start i) out)))
        ((char=? (string-ref str i) char)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) out)))
        (else (loop (+ i 1) start out)))))

  (define (rust-param->signature raw index)
    (let* ((colon (string-find-char raw #\: 0))
           (type (if colon
                   (substring raw (+ colon 1) (string-length raw))
                   ""))
           (name (rust-param-name raw index)))
      (cond
        ((or (string-contains type "NativeCall")
             (string-contains type "MutationContext"))
         #f)
        ((or (string-contains type "RestOf")
             (string-contains type "[Value")
             (string-contains type "&["))
         (string-append ". " name))
        ((string-contains type "Option<")
         (string-append "[" name "]"))
        (else name))))

  (define (rust-signature-text name text)
    (let* ((fn (string-contains text "fn "))
           (open (and fn (string-find-char text #\( fn))))
      (if (not open)
        (default-signature name)
        (let* ((close (matching-paren text open))
               (inside (and close (substring text (+ open 1) close))))
          (if (not inside)
            (default-signature name)
            (let* ((raw (split-top-level inside #\,))
                   (params (let loop ((rest raw) (i 1) (out '()))
                             (if (null? rest)
                               (reverse out)
                               (let ((p (rust-param->signature (car rest) i)))
                                 (loop (cdr rest)
                                       (+ i 1)
                                       (if p (cons p out) out)))))))
              (string-append "(" name
                             (if (null? params)
                               ""
                               (string-append " " (join-strings params " ")))
                             ")")))))))

  (define (fn-line? line)
    (let ((t (trim line)))
      (and (not (starts-with? t "//"))
           (not (starts-with? t "#"))
           (string-contains t "fn "))))

  ;; The line number of the first `fn` line at or after N.
  (define (next-fn-line n lines)
    (let loop ((ls lines) (line-no n))
      (cond ((null? ls) #f)
            ((fn-line? (car ls)) (cons line-no ls))
            (else (loop (cdr ls) (+ line-no 1))))))

  (define (rust-name name)
    (join-strings (split-on-char name #\-) "_"))

  (define (matches-any? needles line)
    (cond ((null? needles) #f)
          ((string-contains line (car needles)) #t)
          (else (matches-any? (cdr needles) line))))

  ;; The signature and defining line of the native procedure NAME in FILE.
  ;; The runtime's source vector points at the containing module, so the
  ;; file is searched for the #[scheme(name = ...)] attribute instead.
  (define (native-info file name)
    (let* ((lines (file-lines file))
           (needles (list (string-append "name = \"" name "\"")
                          (string-append "name=\"" name "\"")))
           (attr (let loop ((ls lines) (n 1))
                   (cond ((null? ls) #f)
                         ((matches-any? needles (car ls)) (cons n ls))
                         (else (loop (cdr ls) (+ n 1)))))))
      (cond
        (attr
         (let ((found (next-fn-line (car attr) (cdr attr))))
           (if found
             (list (rust-signature-text
                    name
                    (join-strings (take-lines 24 (cdr found)) "\n"))
                   (car found))
             #f)))
        (else
         (let ((rust (rust-name name)))
           (let loop ((ls lines) (n 1))
             (cond
               ((null? ls) #f)
               ((and (string-contains (car ls) "fn ")
                     (string-contains (car ls) (string-append rust "(")))
                (list (rust-signature-text
                       name
                       (join-strings (take-lines 24 ls) "\n"))
                      n))
               (else (loop (cdr ls) (+ n 1))))))))))

  ;; Returns (signature source-alist); the source line is corrected for
  ;; native definitions, where the runtime records the module instead.
  (define (export-signature kind src name module-path)
    (cond
      ((equal? kind "value") (list 'null src))
      ((and (pair? src) (string? (cdr (assq 'file src))))
       (let ((file (cdr (assq 'file src)))
             (line (cdr (assq 'line src)))
             (column (cdr (assq 'column src))))
         (if (string-suffix? ".rs" file)
           (let ((info (native-info file name)))
             (if info
               (list (car info)
                     (list (cons 'file file)
                           (cons 'line (cadr info))
                           (cons 'column (if (integer? column) column 1))))
               (list (default-signature name) src)))
           (let ((entry (and module-path
                             (assoc name (module-signature-map module-path)))))
             (list (if entry
                     (cdr entry)
                     (scheme-signature name file (if (integer? line) line 1)))
                   src)))))
      (else
       (let ((entry (and module-path
                         (assoc name (module-signature-map module-path)))))
         (list (if entry (cdr entry) (default-signature name)) src)))))

  (define (document-bindings env inst static-defs module-path)
    (let* ((from-env (if env (env-binding-values env) '()))
           (pairs (if (pair? from-env)
                    from-env
                    (if inst (instance-binding-values inst) '())))
           (out '()))
      (for-each
       (lambda (pair)
         (let* ((name (car pair))
                (value (cdr pair))
                (src (procedure-source-alist value))
                (kind (procedure-kind value src))
                (sig-info (export-signature kind src (symbol->string name)
                                            module-path))
                (signature (car sig-info))
                (source (cadr sig-info)))
           (set! out (cons (list (cons 'name (symbol->string name))
                                 (cons 'kind kind)
                                 (cons 'doc (export-doc kind value
                                                        static-defs name))
                                 (cons 'signature signature)
                                 (cons 'source source))
                           out))))
       pairs)
      (list-sort (lambda (a b)
                   (string<? (cdr (assq 'name a)) (cdr (assq 'name b))))
                 out)))

  (define (module-display-name doc)
    (join-strings (vector->list (cdr (assq 'name doc))) " "))

  ;; Scan results keyed by file path, so several modules sharing one
  ;; file (or repeated walks) scan it only once.
  (define (cached-scan path static-cache-box)
    (let ((hit (assoc path (car static-cache-box))))
      (if hit
        (cdr hit)
        (let ((s (guard (c (#t #f)) (scan-file path))))
          (when s
            (set-car! static-cache-box
                      (cons (cons path s) (car static-cache-box))))
          s))))

  (define (submodule-keys mod-name)
    (let lp ([keys (registered-library-keys (current-library-registry))] [out '()])
      (cond
        [(null? keys) out]
        [(and (= (length (car keys)) (+ 1 (length mod-name)))
              (let prefix? ([a mod-name] [b (car keys)])
                (or (null? a)
                    (and (equal? (car a) (car b)) (prefix? (cdr a) (cdr b))))))
         (lp (cdr keys) (cons (car keys) out))]
        [else (lp (cdr keys) out)])))

  (define (document-submodules mod-name visited static-cache-box)
    (let ((subs '()))
      (for-each
       (lambda (child-key)
         (let ((child (document-module child-key visited static-cache-box)))
           (when child
             (set! subs (cons child subs)))))
       (submodule-keys mod-name))
      (list-sort (lambda (a b)
                   (string<? (module-display-name a)
                             (module-display-name b)))
                 subs)))

  (define (document-module mod-name visited static-cache-box)
    ;; Autoload brings in requested libraries that are not loaded yet,
    ;; so every resolvable module documents on demand.
    (let* ((env (guard (c (#t #f)) (resolve-env mod-name)))
           (inst (guard (c (#t #f))
                   (library-instance (current-library-registry) mod-name))))
      (if (or (and (not env) (not inst)) (visited? mod-name visited))
        #f
        (let* ((seen (cons mod-name visited))
               (path (module-file-path mod-name))
               (scan (and path (cached-scan path static-cache-box)))
               (mod-doc (let ((d (and scan (cdr (assq 'module-doc scan)))))
                          (if (string? d) d 'null)))
               (static-defs (if scan (cdr (assq 'defs scan)) '()))
               (exports (document-bindings env inst static-defs path))
               (subs (document-submodules mod-name seen static-cache-box)))
          (list (cons 'name (list->vector (map segment->string mod-name)))
                (cons 'doc mod-doc)
                (cons 'exports (list->vector exports))
                (cons 'submodules (list->vector subs)))))))

  (define (document-modules mod-names)
    (reset-source-cache!)
    (set! signature-map-cache '())
    (let ((static-cache-box (list '()))
          (modules '()))
      (for-each
       (lambda (mod-name)
         (let ((doc (document-module mod-name '() static-cache-box)))
           (when doc (set! modules (cons doc modules)))))
       mod-names)
      (list (cons 'generator "capy-docgen")
            (cons 'version (implementation-version))
            (cons 'modules (list->vector (reverse modules))))))

  (define (module-path-string mod-name)
    (string-append "(" (join-strings (map segment->string mod-name) " ") ")"))

  ;; Requested modules that produced no documentation entry, e.g.
  ;; because the name does not resolve. Containers that resolved are
  ;; never reported even when empty.
  (define (missing-modules mod-names doc)
    (let ((found (map (lambda (m)
                        (join-strings (vector->list (cdr (assq 'name m))) " "))
                      (vector->list (cdr (assq 'modules doc))))))
      (let loop ((rest mod-names) (out '()))
        (if (null? rest)
          (reverse out)
          (let ((want (join-strings (map segment->string (car rest)) " ")))
            (loop (cdr rest)
                  (if (member want found) out (cons (car rest) out))))))))

  ;; module-paths: list of strings like "(core lists)".
  ;; output-path: file to write, or #f for stdout.
  (define (docgen-main module-paths output-path)
    (let ((mods (map parse-module-path module-paths)))
      (if (and (pair? mods) (not (memq #f mods)))
        (let ((doc (document-modules mods)))
          (let ((missing (missing-modules mods doc)))
            (when (pair? missing)
              (format (current-error-port)
                      "capy-docgen: could not resolve module~a: ~a~%"
                      (if (null? (cdr missing)) "" "s")
                      (join-strings (map module-path-string missing) " "))
              (exit 1)))
          (if (and (string? output-path) (not (equal? output-path "")))
            (call-with-output-file output-path
              (lambda (out) (json-write doc out) (newline out)))
            (begin (json-write doc) (newline)))
          (exit 0))
        (begin
          (format (current-error-port)
                  "capy-docgen: each module must look like '(core lists)', got: ~a~%"
                  module-paths)
          (exit 1))))))
