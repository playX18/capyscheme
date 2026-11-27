;;; Paths as strings, with `/` as the separator.
;;;
;;;   (path-join "a" "b/c")        => "a/b/c"
;;;   (path-join "a" "/b")         => "/b"       an absolute part starts over
;;;   (path-directory "a/b/c.scm") => "a/b"
;;;   (path-file-name "a/b/c.scm") => "c.scm"
;;;   (path-relative "/x/y/z" "/x/w") => "../y/z"

(library (capy path)
  (export path-join
    path-directory
    path-file-name
    path-absolute?
    path-components
    path-relative)
  (import (rnrs))

  (define (path-absolute? path)
    (and (> (string-length path) 0) (char=? (string-ref path 0) #\/)))

  ;; The parts joined with `/`; a part that is absolute makes the ones before it irrelevant.
  (define (path-join first . rest)
    (let loop ([out first] [rest rest])
      (cond
        [(null? rest) out]
        [(path-absolute? (car rest)) (loop (car rest) (cdr rest))]
        [(or (string=? out "") (char=? (string-ref out (- (string-length out) 1)) #\/))
          (loop (string-append out (car rest)) (cdr rest))]
        [else (loop (string-append out "/" (car rest)) (cdr rest))])))

  ;; The directory part of PATH: "." when it has none, "/" for a file in the root.
  (define (path-directory path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) "."]
        [(char=? (string-ref path i) #\/) (if (= i 0) "/" (substring path 0 i))]
        [else (loop (- i 1))])))

  ;; The last component of PATH.
  (define (path-file-name path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) path]
        [(char=? (string-ref path i) #\/) (substring path (+ i 1) (string-length path))]
        [else (loop (- i 1))])))

  ;; The non-empty components of PATH.
  (define (path-components path)
    (let loop ([chars (string->list path)] [current '()] [out '()])
      (define (flush) (if (null? current) out (cons (list->string (reverse current)) out)))
      (cond
        [(null? chars) (reverse (flush))]
        [(char=? (car chars) #\/) (loop (cdr chars) '() (flush))]
        [else (loop (cdr chars) (cons (car chars) current) out)])))

  ;; PATH relative to DIRECTORY, with `..` where it has to leave the directory. Both are
  ;; absolute paths, compared as written.
  (define (path-relative path directory)
    (let loop ([path (path-components path)]
               [base (path-components directory)])
      (cond
        [(and (pair? path) (pair? base) (string=? (car path) (car base)))
          (loop (cdr path) (cdr base))]
        [else
          (let ([parts (append (map (lambda (_) "..") base) path)])
            (if (null? parts)
              "."
              (let join ([parts parts])
                (if (null? (cdr parts))
                  (car parts)
                  (string-append (car parts) "/" (join (cdr parts)))))))])))
)
