;;; Text rendering of diagnostics, in the style of rustc:
;;;
;;;   error[unbound-variable]: unbound variable `lenght`
;;;     --> src/app/main.scm:12:4
;;;      |
;;;   12 |   (lenght xs)
;;;      |    ^^^^^^ not bound in this scope
;;;      |
;;;      = help: a binding with a similar name exists: `length`
;;;
;;; Primary labels are underlined with `^`, secondary ones with `-`. A label that spans several
;;; lines is drawn with `/` and `|` in a margin left of the source. Columns are code points and
;;; tabs are expanded to four spaces. With a painter that has colors off, the output is
;;; deterministic and plain.
;;;
;;; The source text comes from the SOURCE argument: a string (the text of every file), a
;;; procedure from a file name to its text or #f, or #f to read the files from disk. A file
;;; without text is shown as its `-->` line only.

(library (capy diagnostics render)
  (export render-diagnostic
    diagnostic->string
    diagnostics-summary-line
    render-diagnostics-summary)
  (import (rnrs)
    (core diagnostics)
    (capy term paint))

  (define tab-width 4)

  ;; ---------------------------------------------------------------------------------------
  ;; Small helpers
  ;; ---------------------------------------------------------------------------------------

  (define (spaces n) (make-string (max n 0) #\space))

  (define (pad-left s width)
    (string-append (spaces (- width (string-length s))) s))

  (define (digits n) (string-length (number->string n)))

  (define (string-rtrim s)
    (let loop ([n (string-length s)])
      (if (and (> n 0) (char=? (string-ref s (- n 1)) #\space))
        (loop (- n 1))
        (substring s 0 n))))

  (define (list-sort-by less? l) (list-sort less? l))

  (define (split-lines text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text))
          (list->vector (reverse (cons (substring text start i) out)))]
        [(char=? (string-ref text i) #\newline)
          (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (strip-cr s)
    (let ([n (string-length s)])
      (if (and (> n 0) (char=? (string-ref s (- n 1)) #\return))
        (substring s 0 (- n 1))
        s)))

  ;; The line with tabs expanded, and a vector mapping each code point column (0 .. length) to
  ;; its display column.
  (define (expand-tabs line)
    (let* ([n (string-length line)]
           [cmap (make-vector (+ n 1) 0)])
      (let loop ([i 0] [col 0] [out '()])
        (vector-set! cmap i col)
        (if (= i n)
          (values (list->string (reverse out)) cmap)
          (let ([c (string-ref line i)])
            (if (char=? c #\tab)
              (loop (+ i 1) (+ col tab-width) (append (string->list (spaces tab-width)) out))
              (loop (+ i 1) (+ col 1) (cons c out))))))))

  (define (read-file-text file)
    (guard (e [#t #f])
      (and (string? file)
        (file-exists? file)
        (call-with-input-file file get-string-all))))

  ;; A procedure file -> vector of lines (or #f), caching per file.
  (define (make-line-source source)
    (define cache '())
    (lambda (file)
      (cond
        [(assoc file cache) => cdr]
        [else
          (let* ([text (cond
                         [(string? source) source]
                         [(procedure? source) (source file)]
                         [else (read-file-text file)])]
                 [lines (and (string? text)
                          (vector-map strip-cr (split-lines text)))])
            (set! cache (cons (cons file lines) cache))
            lines)])))

  ;; ---------------------------------------------------------------------------------------
  ;; Labels
  ;; ---------------------------------------------------------------------------------------

  ;; The rendering form of a label: #(line col end-line end-col style text), with the end
  ;; inclusive of its last line and exclusive in its column.
  (define (rl-line l) (vector-ref l 0))
  (define (rl-col l) (vector-ref l 1))
  (define (rl-end-line l) (vector-ref l 2))
  (define (rl-end-col l) (vector-ref l 3))
  (define (rl-style l) (vector-ref l 4))
  (define (rl-text l) (vector-ref l 5))
  (define (rl-multi? l) (> (rl-end-line l) (rl-line l)))

  (define (span->rl span style text)
    (let* ([line (span-line span)]
           [end-line (max line (span-end-line span))]
           [col (span-col span)]
           [end-col (if (and (= end-line line) (<= (span-end-col span) col))
                      (+ col 1)
                      (span-end-col span))])
      (vector line col end-line end-col style text)))

  (define (diagnostic-render-labels d)
    (let ([labels (diagnostic-labels d)]
          [primary (diagnostic-primary d)])
      (cond
        [(pair? labels) labels]
        [primary (list (make-label primary 'primary ""))]
        [else '()])))

  ;; The files mentioned by the labels, in order of first appearance; the primary file first.
  (define (label-files labels primary)
    (let loop ([ls labels] [out (if (and primary (span-file primary)) (list (span-file primary)) '())])
      (cond
        [(null? ls) (reverse out)]
        [(member (span-file (label-span (car ls))) out) (loop (cdr ls) out)]
        [else (loop (cdr ls) (cons (span-file (label-span (car ls))) out))])))

  ;; ---------------------------------------------------------------------------------------
  ;; Rendering
  ;; ---------------------------------------------------------------------------------------

  (define (severity-name s)
    (case s
      [(error) "error"]
      [(warning) "warning"]
      [(note) "note"]
      [(hint) "help"]
      [else (symbol->string s)]))

  (define (severity-style s)
    (case s
      [(error) '(bold red)]
      [(warning) '(bold yellow)]
      [(note) '(bold green)]
      [else '(bold cyan)]))

  (define (label-style-of severity style)
    (if (eq? style 'primary) (severity-style severity) '(bold blue)))

  ;; Writes the lines of one file's snippet. LABELS are rendering labels of that file, LINES the
  ;; vector of the file's lines. W is the gutter width.
  (define (render-snippet painter port severity lines labels w)
    (define blue '(bold blue))
    (define (gutter-blank) (string-append (spaces w) " |"))
    (define (emit-gutter text-painted)
      (put-string port (paint painter blue (string-append (spaces w) " | ")))
      (put-string port text-painted)
      (newline port))
    (define (emit-blank)
      (put-string port (paint painter blue (gutter-blank)))
      (newline port))
    (define multis
      (list-sort (lambda (a b)
                   (or (< (rl-line a) (rl-line b))
                     (and (= (rl-line a) (rl-line b)) (> (rl-end-line a) (rl-end-line b)))))
        (filter rl-multi? labels)))
    (define k (length multis))
    (define margin-width (* 2 k))
    (define (level-of l)
      (let loop ([ms multis] [i 0])
        (if (eq? (car ms) l) i (loop (cdr ms) (+ i 1)))))
    (define (line-text n)
      (and (<= 1 n (vector-length lines)) (vector-ref lines (- n 1))))
    (define (first-nonblank-col text)
      (let loop ([i 0])
        (cond [(= i (string-length text)) i]
              [(memv (string-ref text i) '(#\space #\tab)) (loop (+ i 1))]
              [else i])))
    ;; Multi-line labels starting at the first non-blank character are drawn with a `/`.
    (define (slash-style? l)
      (let ([t (line-text (rl-line l))])
        (and t (<= (rl-col l) (first-nonblank-col t)))))
    (define (display-col cmap col)
      (vector-ref cmap (min col (- (vector-length cmap) 1))))
    ;; Lines to show.
    (define shown
      (let ([set '()])
        (define (add! n) (unless (memv n set) (set! set (cons n set))))
        (for-each
          (lambda (l)
            (cond
              [(rl-multi? l)
                (let ([a (rl-line l)] [b (rl-end-line l)])
                  (if (> (- b a) 4)
                    (begin (add! a) (add! (+ a 1)) (add! b))
                    (do ([n a (+ n 1)]) ((> n b)) (add! n))))]
              [else (add! (rl-line l))]))
          labels)
        (let ([sorted (list-sort < set)])
          ;; fill a gap of exactly one line
          (let loop ([ls sorted] [out '()])
            (cond
              [(null? ls) (reverse out)]
              [(and (pair? out) (= (- (car ls) (car out)) 2))
                (loop (cdr ls) (cons (car ls) (cons (+ (car out) 1) out)))]
              [else (loop (cdr ls) (cons (car ls) out))])))))
    ;; The margin cells for a row on line N. KIND is `source` or `under`.
    (define (margin-string n kind)
      (let ([buf (make-string margin-width #\space)])
        (for-each
          (lambda (l)
            (let* ([i (level-of l)]
                   [a (rl-line l)]
                   [b (rl-end-line l)]
                   [slash? (slash-style? l)])
              (cond
                [(and (eq? kind 'source) slash? (= n a))
                  (string-set! buf (* 2 i) #\/)]
                [(and (if slash? (<= a n) (< a n)) (<= n b)
                      (not (and (eq? kind 'source) (= n a))))
                  (string-set! buf (* 2 i) #\|)]
                [(and (eq? kind 'under) slash? (= n a))
                  (string-set! buf (* 2 i) #\|)])))
          multis)
        buf))
    (define (paint-margin buf)
      ;; Paint each non-space margin cell in the style of its label.
      (let loop ([i 0] [out ""])
        (if (= i (string-length buf))
          out
          (let ([c (string-ref buf i)])
            (loop (+ i 1)
              (string-append out
                (if (char=? c #\space)
                  " "
                  (paint painter
                    (label-style-of severity (rl-style (list-ref multis (quotient i 2))))
                    (string c)))))))))
    ;; A row drawn on a character buffer: CELLS is a list of (position char style).
    (define (paint-cells buf cells)
      (let loop ([i 0] [out ""])
        (if (= i (string-length buf))
          out
          (let ([cell (find (lambda (c) (= (car c) i)) cells)])
            (loop (+ i 1)
              (string-append out
                (if cell
                  (paint painter (caddr cell) (string (cadr cell)))
                  (string (string-ref buf i)))))))))
    ;; The underline rows of the single-line labels of line N.
    (define (render-single-rows n cmap margin)
      (define singles
        (list-sort (lambda (a b) (< (rl-col a) (rl-col b)))
          (filter (lambda (l) (and (not (rl-multi? l)) (= (rl-line l) n))) labels)))
      (unless (null? singles)
        (let* ([total (+ margin-width
                       (apply max
                         (map (lambda (l)
                                (+ (display-col cmap (rl-end-col l)) 1
                                  (string-length (rl-text l))))
                           singles))
                       2)]
               [base (lambda () (make-string total #\space))])
          (define (row-prefix) (paint-margin margin))
          (define (start-of l) (+ margin-width (display-col cmap (rl-col l))))
          (define (end-of l) (+ margin-width (display-col cmap (max (rl-end-col l) (+ (rl-col l) 1)))))
          (define (emit buf cells)
            ;; buf: plain template; the margin cells are painted separately
            (let* ([plain (paint-cells buf cells)]
                   [tail (substring-painted plain buf cells)])
              (emit-gutter (string-append (row-prefix) tail))))
          ;; the part of the row after the margin, trailing blanks removed
          (define (substring-painted plain buf cells)
            (let* ([used (let loop ([n (string-length buf)])
                           (if (and (> n margin-width) (char=? (string-ref buf (- n 1)) #\space)
                                 (not (find (lambda (c) (= (car c) (- n 1))) cells)))
                             (loop (- n 1))
                             n))])
              (paint-cells (substring buf margin-width used)
                (map (lambda (c) (list (- (car c) margin-width) (cadr c) (caddr c)))
                  (filter (lambda (c) (and (>= (car c) margin-width) (< (car c) used))) cells)))))
          (let* ([rightmost (car (reverse singles))]
                 [buf (base)]
                 [cells '()])
            ;; marks of every label
            (for-each
              (lambda (l)
                (let ([ch (if (eq? (rl-style l) 'primary) #\^ #\-)]
                      [style (label-style-of severity (rl-style l))])
                  (do ([p (start-of l) (+ p 1)]) ((>= p (end-of l)))
                    (set! cells (cons (list p ch style) cells))
                    (string-set! buf p ch))))
              singles)
            ;; the text of the rightmost label goes on the same row
            (unless (string=? (rl-text rightmost) "")
              (let ([p (+ (end-of rightmost) 1)]
                    [style (label-style-of severity (rl-style rightmost))])
                (let loop ([i 0])
                  (when (< i (string-length (rl-text rightmost)))
                    (set! cells (cons (list (+ p i) (string-ref (rl-text rightmost) i) style) cells))
                    (string-set! buf (+ p i) (string-ref (rl-text rightmost) i))
                    (loop (+ i 1))))))
            (emit buf cells)
            ;; the texts of the other labels, right to left, joined by connectors
            (let loop ([rest (reverse (cdr (reverse singles)))])
              (let ([with-text (filter (lambda (l) (not (string=? (rl-text l) ""))) rest)])
                (unless (null? with-text)
                  (let ([connectors
                          (lambda (ls)
                            (map (lambda (l)
                                   (list (start-of l) #\| (label-style-of severity (rl-style l))))
                              ls))])
                    ;; connector row
                    (let ([b (base)])
                      (for-each (lambda (c) (string-set! b (car c) #\|)) (connectors with-text))
                      (emit b (connectors with-text)))
                    ;; text row for the rightmost remaining label
                    (let* ([l (car (reverse with-text))]
                           [others (reverse (cdr (reverse with-text)))]
                           [b (base)]
                           [cs (connectors others)]
                           [style (label-style-of severity (rl-style l))])
                      (for-each (lambda (c) (string-set! b (car c) #\|)) cs)
                      (let ([p (start-of l)])
                        (let tl ([i 0] [cs cs])
                          (if (< i (string-length (rl-text l)))
                            (begin
                              (string-set! b (+ p i) (string-ref (rl-text l) i))
                              (tl (+ i 1) (cons (list (+ p i) (string-ref (rl-text l) i) style) cs)))
                            (emit b cs))))
                      (loop (reverse (cdr (reverse with-text)))))))))))))
    ;; The rows that close and open multi-line labels on line N.
    (define (render-ml-start-rows n cmap)
      (for-each
        (lambda (l)
          (when (and (= (rl-line l) n) (not (slash-style? l)))
            (let* ([i (level-of l)]
                   [style (label-style-of severity (rl-style l))]
                   [target (+ margin-width (display-col cmap (rl-col l)))]
                   [buf (make-string (+ target 1) #\space)]
                   [margin (margin-string n 'under)])
              ;; `_` from the cell right of the margin character to the label column, then `^`
              (do ([p (+ (* 2 i) 1) (+ p 1)]) ((>= p target))
                (string-set! buf p #\_))
              (string-set! buf target #\^)
              (let ([cells (let loop ([p (+ (* 2 i) 1)] [out (list (list target #\^ style))])
                             (if (>= p target) out (loop (+ p 1) (cons (list p #\_ style) out))))])
                (emit-gutter (paint-cells buf cells))))))
        multis))
    (define (render-ml-end-rows n cmap)
      (for-each
        (lambda (l)
          (when (= (rl-end-line l) n)
            (let* ([i (level-of l)]
                   [style (label-style-of severity (rl-style l))]
                   [last (+ margin-width (max 0 (- (display-col cmap (rl-end-col l)) 1)))]
                   [buf (make-string (+ last 1) #\space)]
                   [text (rl-text l)]
                   [margin (margin-string n 'under)])
              (do ([q 0 (+ q 1)]) ((>= q (min (string-length margin) (string-length buf))))
                (string-set! buf q (string-ref margin q)))
              (do ([p (+ (* 2 i) 1) (+ p 1)]) ((>= p last))
                (string-set! buf p #\_))
              (string-set! buf last #\^)
              (let* ([cells (let loop ([p (* 2 i)] [out (list (list last #\^ style))])
                              (if (>= p last)
                                out
                                (loop (+ p 1) (cons (list p (if (= p (* 2 i)) #\| #\_) style) out))))]
                     [row (paint-cells buf cells)])
                (emit-gutter
                  (string-append row
                    (if (string=? text "") "" (string-append " " (paint painter style text)))))))))
        multis))
    ;; The rows themselves.
    (emit-blank)
    (let loop ([ls shown] [prev #f])
      (unless (null? ls)
        (let* ([n (car ls)]
               [raw (line-text n)])
          (when (and prev (> n (+ prev 1)))
            (put-string port (paint painter blue "..."))
            (newline port))
          (when raw
            (let-values ([(text cmap) (expand-tabs raw)])
              (put-string port
                (paint painter blue (string-append (pad-left (number->string n) w) " | ")))
              (put-string port (paint-margin (margin-string n 'source)))
              (put-string port text)
              (newline port)
              (render-ml-start-rows n cmap)
              (render-single-rows n cmap (margin-string n 'under))
              (render-ml-end-rows n cmap)))
          (loop (cdr ls) n)))))

  ;; ---------------------------------------------------------------------------------------
  ;; Diagnostics
  ;; ---------------------------------------------------------------------------------------

  ;; Writes D to PORT, followed by a blank line. See the top of this file for SOURCE.
  (define (render-diagnostic d painter port . maybe-source)
    (define source (if (pair? maybe-source) (car maybe-source) #f))
    (define lines-of (make-line-source source))
    (define severity (diagnostic-severity d))
    (define primary (diagnostic-primary d))
    (define labels (diagnostic-render-labels d))
    (define files (label-files labels primary))
    ;; per-file rendering labels, only those with a usable span
    (define (file-labels f)
      (filter values
        (map (lambda (l)
               (and (equal? (span-file (label-span l)) f)
                 (span->rl (label-span l) (label-style l) (label-text l))))
          labels)))
    (define max-line
      (apply max 1
        (map (lambda (l) (max (span-line (label-span l)) (span-end-line (label-span l))))
          labels)))
    (define w (digits max-line))
    ;; header
    (let ([style (severity-style severity)])
      (put-string port (paint painter style (severity-name severity)))
      (when (diagnostic-code d)
        (put-string port
          (paint painter style (string-append "[" (symbol->string (diagnostic-code d)) "]"))))
      (put-string port (paint painter '(bold) (string-append ": " (diagnostic-message d))))
      (newline port))
    ;; snippets
    (let loop ([fs files] [first? #t])
      (unless (null? fs)
        (let* ([f (car fs)]
               [lines (lines-of f)]
               [ls (file-labels f)]
               [anchor (cond
                         [(and first? primary) primary]
                         [(pair? ls) #f]
                         [else #f])]
               [first-label (and (pair? ls) (car ls))])
          (put-string port (paint painter '(bold blue) (string-append (spaces w) (if first? "--> " "::: "))))
          (put-string port
            (string-append (if (string? f) f "<unknown>")
              (let ([p (or anchor first-label)])
                (cond
                  [(not p) ""]
                  [(vector? p)
                    (if (= (vector-length p) 7)
                      (string-append ":" (number->string (span-line p)) ":"
                        (number->string (+ 1 (span-col p))))
                      (string-append ":" (number->string (rl-line p)) ":"
                        (number->string (+ 1 (rl-col p)))))]
                  [else ""]))))
          (newline port)
          (when lines
            (render-snippet painter port severity lines ls w))
          (loop (cdr fs) #f))))
    ;; notes, helps, suggestions
    (let* ([notes (diagnostic-notes d)]
           [helps (diagnostic-helps d)]
           [suggestions (diagnostic-suggestions d)]
           [any? (or (pair? notes) (pair? helps) (pair? suggestions))]
           [w* (if (pair? files) w 1)])
      (define (emit-extra kind text)
        (put-string port (paint painter '(bold blue) (string-append (spaces w*) " = ")))
        (put-string port (paint painter '(bold) (string-append kind ":")))
        (put-string port (string-append " " text))
        (newline port))
      (when (and any? (pair? files))
        (put-string port (paint painter '(bold blue) (string-append (spaces w*) " |")))
        (newline port))
      (for-each (lambda (h) (emit-extra "help" h)) helps)
      (for-each
        (lambda (s)
          (emit-extra "help"
            (string-append (suggestion-message s) ": `" (suggestion-replacement s) "`")))
        suggestions)
      (for-each (lambda (n) (emit-extra "note" n)) notes))
    (newline port)
    (for-each (lambda (r) (render-diagnostic r painter port source))
      (diagnostic-related d)))

  ;; D as a string without colors.
  (define (diagnostic->string d . maybe-source)
    (call-with-string-output-port
      (lambda (out)
        (apply render-diagnostic d (make-painter 'never out) out maybe-source))))

  (define (plural n one many)
    (string-append (number->string n) " " (if (= n 1) one many)))

  ;; The closing line of a build: NAME is what was being compiled, such as "(app main)".
  ;;   error: could not compile (app main) due to 3 previous errors; 1 warning emitted
  ;; Without errors it is a warning line, and without either it is #f.
  (define (diagnostics-summary-line name errors warnings)
    (cond
      [(> errors 0)
        (string-append "error: could not compile " name " due to "
          (plural errors "previous error" "previous errors")
          (if (> warnings 0)
            (string-append "; " (plural warnings "warning" "warnings") " emitted")
            ""))]
      [(> warnings 0)
        (string-append "warning: " name " generated " (plural warnings "warning" "warnings"))]
      [else #f]))

  ;; Writes the summary line, painted, to PORT; returns #f when there is nothing to report.
  (define (render-diagnostics-summary name errors warnings painter port)
    (let ([line (diagnostics-summary-line name errors warnings)])
      (and line
        (let* ([split (if (> errors 0) 5 7)])
          (put-string port (paint painter (if (> errors 0) '(bold red) '(bold yellow))
                             (substring line 0 split)))
          (put-string port (substring line split (string-length line)))
          (newline port)
          #t)))))
