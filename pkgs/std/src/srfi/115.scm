;;; SRFI 115: Scheme Regular Expressions.
;;;
;;; A pure Scheme regex engine. An SRE is parsed into a small tree, the tree is compiled to
;;; a Thompson NFA (three parallel vectors: opcode, operand, operand), and two machines run
;;; that program:
;;;
;;; - A lazy DFA, built one state at a time from the NFA and cached in the regexp. A state is
;;;   the ordered list of NFA threads, so the DFA keeps the leftmost-first priorities of the
;;;   NFA: `regexp-search` runs it forward to find where the match ends, then runs a DFA of
;;;   the reversed pattern backward from there to find where it starts. A Latin-1 character
;;;   takes one `vector-ref` per step; other characters a small hash probe. Patterns with
;;;   only `bos`/`eos` assertions use it.
;;; - A Pike VM (an NFA simulation with one thread per program counter) for the submatch
;;;   positions, run only over the span the DFA found, and for every search of a pattern
;;;   with line, word or grapheme assertions. It is linear in the input for a fixed pattern;
;;;   there is no backtracking.
;;;
;;; Matching is leftmost-first (Perl and PCRE semantics): the leftmost match wins, and among
;;; matches at that position the alternatives of `or` and the greedy/non-greedy repetitions
;;; are tried in order. Character sets are bitmaps for U+0000..U+00FF plus sorted ranges or
;;; a predicate above, with full Unicode classes (`alpha` is `char-alphabetic?`, `punct` the
;;; P* categories, and so on).
;;;
;;; A compiled regexp may be shared between threads: the DFA cache is read without locking
;;; and filled under a per-regexp mutex.
;;;
;;; (capy pregexp) keeps its own backtracking matcher, since it offers backreferences and
;;; look-around, which a DFA or Pike VM cannot run.
;;;
;;; Deviations from the specification:
;;;
;;; - `or` is leftmost-first, not POSIX leftmost-longest: (or "a" "ab") matches "a" in "ab"
;;;   when searching. `regexp-matches` still matches the whole string whenever any
;;;   alternative can.
;;; - `backref` and the look-around patterns are not supported (the SRFI makes them
;;;   optional); they raise an error.
;;; - `w/nocase` uses the simple (one-character) case mappings: "ß" does not match "SS".
;;; - `grapheme`, `bog` and `eog` approximate extended grapheme clusters: a cluster is CR LF,
;;;   or a character followed by any marks (Mn, Mc, Me) and zero-width joiners.
;;; - The START and END arguments delimit the string for the assertions too: `bos` matches
;;;   at START and `eos` at END.
;;; - SRFI 14 char-sets (Latin-1 in this implementation) may appear in an SRE; they are
;;;   converted when the SRE is compiled.
(library (srfi 115)
  (export
    regexp rx regexp->sre char-set->sre valid-sre? regexp?
    regexp-matches regexp-matches? regexp-search
    regexp-replace regexp-replace-all regexp-fold regexp-extract regexp-split regexp-partition
    regexp-match? regexp-match-count regexp-match-submatch
    regexp-match-submatch-start regexp-match-submatch-end regexp-match->list)
  (import
    (rnrs)
    (only (srfi 14) char-set? char-set->list char-set->string)
    (only (core thread) make-mutex with-mutex))

  ;;; ------------------------------------------------------------------------------------
  ;;; Character sets
  ;;;
  ;;; LO is a bytevector of 256 bytes, 1 for a member. HI says which characters from U+0100
  ;;; up are members: #f for none, #t for all, a vector #(start0 end0 start1 end1 ...) of
  ;;; sorted, disjoint, inclusive ranges, or a predicate on code points.

  (define-record-type (cset make-cset cset?)
    (fields lo hi)
    (sealed #t))

  (define max-code #x10FFFF)

  ;; Guards the lazily computed tables of character sets.
  (define cset-lock (make-mutex))

  (define (cset-contains? cs c)
    (if (fx<? c 256)
      (fx=? (bytevector-u8-ref (cset-lo cs) c) 1)
      (let ([hi (cset-hi cs)])
        (cond
          [(not hi) #f]
          [(eq? hi #t) #t]
          [(vector? hi) (ranges-contain? hi c)]
          [else (hi c)]))))

  (define (ranges-contain? v c)
    (let loop ([lo 0] [hi (fxarithmetic-shift-right (vector-length v) 1)])
      (and (fx<? lo hi)
           (let* ([mid (fxarithmetic-shift-right (fx+ lo hi) 1)]
                  [k (fx* 2 mid)])
             (cond
               [(fx<? c (vector-ref v k)) (loop lo mid)]
               [(fx>? c (vector-ref v (fx+ k 1))) (loop (fx+ mid 1) hi)]
               [else #t])))))

  (define cset:empty (make-cset (make-bytevector 256 0) #f))
  (define cset:full (make-cset (make-bytevector 256 1) #t))

  ;; RANGES is a list of (start . end) pairs of code points, in any order.
  (define (ranges->cset ranges)
    (let ([lo (make-bytevector 256 0)])
      (for-each
        (lambda (r)
          (let loop ([c (car r)])
            (when (and (fx<=? c (cdr r)) (fx<? c 256))
              (bytevector-u8-set! lo c 1)
              (loop (fx+ c 1)))))
        ranges)
      (make-cset lo
        (normalize-ranges
          (let loop ([rs ranges] [acc '()])
            (cond
              [(null? rs) acc]
              [(fx>? (cdar rs) 255)
               (loop (cdr rs) (cons (cons (fxmax 256 (caar rs)) (cdar rs)) acc))]
              [else (loop (cdr rs) acc)]))))))

  ;; Sorted, merged ranges above U+00FF as a HI value.
  (define (normalize-ranges ranges)
    (let ([sorted (list-sort (lambda (a b) (fx<? (car a) (car b))) ranges)])
      (let loop ([rs sorted] [acc '()])
        (cond
          [(null? rs)
           (let ([merged (reverse acc)])
             (cond
               [(null? merged) #f]
               [(and (null? (cdr merged)) (fx<=? (caar merged) 256) (fx>=? (cdar merged) max-code)) #t]
               [else
                (let ([v (make-vector (fx* 2 (length merged)))])
                  (let fill ([m merged] [i 0])
                    (unless (null? m)
                      (vector-set! v i (caar m))
                      (vector-set! v (fx+ i 1) (cdar m))
                      (fill (cdr m) (fx+ i 2))))
                  v)]))]
          [(and (pair? acc) (fx<=? (caar rs) (fx+ (cdar acc) 1)))
           (loop (cdr rs) (cons (cons (caar acc) (fxmax (cdar acc) (cdar rs))) (cdr acc)))]
          [else (loop (cdr rs) (cons (car rs) acc))]))))

  (define (hi->ranges hi)
    (let loop ([i (fx- (vector-length hi) 2)] [acc '()])
      (if (fx<? i 0) acc (loop (fx- i 2) (cons (cons (vector-ref hi i) (vector-ref hi (fx+ i 1))) acc)))))

  (define (hi->pred hi)
    (cond
      [(not hi) (lambda (c) #f)]
      [(eq? hi #t) (lambda (c) #t)]
      [(vector? hi) (lambda (c) (ranges-contain? hi c))]
      [else hi]))

  ;; PRED on code points. With HI? #f the set has no member above U+00FF.
  (define (pred->cset pred hi?)
    (let ([lo (make-bytevector 256 0)])
      (let loop ([c 0])
        (when (fx<? c 256)
          (when (pred c) (bytevector-u8-set! lo c 1))
          (loop (fx+ c 1))))
      (make-cset lo (and hi? pred))))

  (define (codes->cset codes)
    (ranges->cset (map (lambda (c) (cons c c)) codes)))

  (define (lo-map f a b)
    (let ([r (make-bytevector 256 0)] [la (cset-lo a)] [lb (cset-lo b)])
      (let loop ([i 0])
        (when (fx<? i 256)
          (bytevector-u8-set! r i (f (bytevector-u8-ref la i) (bytevector-u8-ref lb i)))
          (loop (fx+ i 1))))
      r))

  (define (hi-complement h)
    (cond
      [(not h) #t]
      [(eq? h #t) #f]
      [(vector? h)
       (let loop ([rs (hi->ranges h)] [from 256] [acc '()])
         (cond
           [(null? rs)
            (normalize-ranges (reverse (if (fx<=? from max-code) (cons (cons from max-code) acc) acc)))]
           [(fx<? from (caar rs))
            (loop (cdr rs) (fx+ (cdar rs) 1) (cons (cons from (fx- (caar rs) 1)) acc))]
           [else (loop (cdr rs) (fx+ (cdar rs) 1) acc)]))]
      [else (lambda (c) (not (h c)))]))

  (define (hi-union a b)
    (cond
      [(not a) b]
      [(not b) a]
      [(or (eq? a #t) (eq? b #t)) #t]
      [(and (vector? a) (vector? b)) (normalize-ranges (append (hi->ranges a) (hi->ranges b)))]
      [else (let ([pa (hi->pred a)] [pb (hi->pred b)]) (lambda (c) (or (pa c) (pb c))))]))

  (define (hi-intersection a b)
    (cond
      [(or (not a) (not b)) #f]
      [(eq? a #t) b]
      [(eq? b #t) a]
      [(and (vector? a) (vector? b))
       (hi-complement (hi-union (hi-complement a) (hi-complement b)))]
      [else (let ([pa (hi->pred a)] [pb (hi->pred b)]) (lambda (c) (and (pa c) (pb c))))]))

  (define (cset-union a b)
    (make-cset (lo-map fxior a b) (hi-union (cset-hi a) (cset-hi b))))

  (define (cset-intersection a b)
    (make-cset (lo-map fxand a b) (hi-intersection (cset-hi a) (cset-hi b))))

  (define (cset-complement a)
    (make-cset (lo-map (lambda (x y) (fx- 1 x)) a a) (hi-complement (cset-hi a))))

  (define (cset-difference a b)
    (cset-intersection a (cset-complement b)))

  ;; The single member of CS, or #f.
  (define (cset-singleton cs)
    (and (not (cset-hi cs))
         (let loop ([c 0] [found #f])
           (cond
             [(fx=? c 256) found]
             [(fx=? (bytevector-u8-ref (cset-lo cs) c) 1) (and (not found) (loop (fx+ c 1) c))]
             [else (loop (fx+ c 1) found)]))))

  (define (cset-full? cs)
    (and (eq? (cset-hi cs) #t)
         (let loop ([c 0])
           (or (fx=? c 256) (and (fx=? (bytevector-u8-ref (cset-lo cs) c) 1) (loop (fx+ c 1)))))))

  (define (lo-members cs)
    (let loop ([c 255] [acc '()])
      (if (fx<? c 0)
        acc
        (loop (fx- c 1) (if (fx=? (bytevector-u8-ref (cset-lo cs) c) 1) (cons c acc) acc)))))

  ;;; Case folding
  ;;;
  ;;; Two characters are equal ignoring case when they have the same simple case fold. The
  ;;; classes of cased characters (fold -> members) are computed once, on the first use of
  ;;; `w/nocase`, over the planes that have cased letters.

  (define fold-classes #f)

  (define (fold-code c) (char->integer (char-foldcase (integer->char c))))

  (define (build-fold-classes!)
    (with-mutex cset-lock
      (unless fold-classes
        (let ([table (make-eqv-hashtable)])
          (let loop ([c 0])
            (when (fx<? c #x20000)
              (if (fx=? c #xD800)
                (loop #xE000)
                (let* ([ch (integer->char c)] [f (char->integer (char-foldcase ch))])
                  (when (or (not (fx=? f c))
                            (not (char=? (char-upcase ch) ch))
                            (not (char=? (char-downcase ch) ch)))
                    (let ([members (hashtable-ref table f '())])
                      (hashtable-set! table f
                        (let* ([m (if (memv f members) members (cons f members))])
                          (if (memv c m) m (cons c m))))))
                  (loop (fx+ c 1))))))
          (set! fold-classes table)))))

  ;; The characters equal to C ignoring case, C included.
  (define (fold-class c)
    (unless fold-classes (build-fold-classes!))
    (hashtable-ref fold-classes (fold-code c) (list c)))

  ;; CODES closed under case.
  (define (case-closure codes)
    (let loop ([cs codes] [acc '()])
      (if (null? cs)
        acc
        (loop (cdr cs)
              (fold-left (lambda (acc x) (if (memv x acc) acc (cons x acc))) acc (fold-class (car cs)))))))

  ;; CS closed under case: exact below U+0100 and for the case partners of those members, a
  ;; predicate above when CS has members there.
  (define (cset-fold-case cs)
    (let ([exact (codes->cset (case-closure (lo-members cs)))]
          [hi (cset-hi cs)])
      (cond
        [(not hi) exact]
        [(eq? hi #t) (cset-union exact (make-cset (make-bytevector 256 0) #t))]
        [else
         (let* ([mem? (lambda (v) (cset-contains? cs v))]
                [pred (lambda (c) (or (mem? c) (exists mem? (fold-class c))))])
           (cset-union exact (pred->cset pred #t)))])))

  ;;; Named character sets, computed once per name and mode.

  (define (category c) (char-general-category (integer->char c)))

  (define (category-in cats)
    (lambda (c) (and (memq (category c) cats) #t)))

  (define punct-cats '(Pc Pd Ps Pe Pi Pf Po))
  (define symbol-cats '(Sm Sc Sk So))
  (define graphic-cats '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po Sm Sc Sk So))

  (define (between lo hi) (lambda (c) (and (fx>=? c lo) (fx<=? c hi))))

  (define (ascii-only pred) (lambda (c) (and (fx<? c 128) (pred c))))

  (define unicode-sets (make-eq-hashtable))
  (define ascii-sets (make-eq-hashtable))

  (define cset-names
    '(any nonl ascii lower-case lower upper-case upper title-case title alphabetic alpha
      alphanumeric alphanum alnum numeric num punctuation punct symbol graphic graph
      whitespace white space printing print control cntrl hex-digit xdigit))

  (define (named-cset name ascii?)
    (and (memq name cset-names)
         (let ([table (if ascii? ascii-sets unicode-sets)])
           (with-mutex cset-lock
             (or (hashtable-ref table name #f)
                 (let ([cs (compute-named-cset name ascii?)])
                   (hashtable-set! table name cs)
                   cs))))))

  (define (compute-named-cset name ascii?)
    (define (class ascii-pred unicode-pred)
      (if ascii?
        (pred->cset (ascii-only ascii-pred) #f)
        (pred->cset unicode-pred #t)))
    (define (ch-pred p) (lambda (c) (p (integer->char c))))
    (define lower? (between 97 122))
    (define upper? (between 65 90))
    (define digit? (between 48 57))
    (define (alpha? c) (or (lower? c) (upper? c)))
    (define (ascii-punct? c) (memv c '(33 34 35 37 38 39 40 41 42 44 45 46 47 58 59 63 64 91 92 93 95 123 125)))
    (define (ascii-symbol? c) (memv c '(36 43 60 61 62 94 96 124 126)))
    (define (ascii-white? c) (or (fx=? c 32) (and (fx>=? c 9) (fx<=? c 13))))
    (define graphic (category-in graphic-cats))
    (case name
      [(any) cset:full]
      [(nonl) (cset-complement (codes->cset '(10)))]
      [(ascii) (ranges->cset '((0 . 127)))]
      [(lower-case lower) (class lower? (ch-pred char-lower-case?))]
      [(upper-case upper) (class upper? (ch-pred char-upper-case?))]
      [(title-case title) (class (lambda (c) #f) (ch-pred char-title-case?))]
      [(alphabetic alpha) (class alpha? (ch-pred char-alphabetic?))]
      [(numeric num) (class digit? (lambda (c) (eq? (category c) 'Nd)))]
      [(alphanumeric alphanum alnum)
       (class (lambda (c) (or (alpha? c) (digit? c)))
              (lambda (c) (or (char-alphabetic? (integer->char c)) (eq? (category c) 'Nd))))]
      [(punctuation punct) (class ascii-punct? (category-in punct-cats))]
      [(symbol) (class ascii-symbol? (category-in symbol-cats))]
      [(graphic graph) (class (between 33 126) graphic)]
      [(whitespace white space) (class ascii-white? (ch-pred char-whitespace?))]
      [(printing print)
       (class (between 32 126) (lambda (c) (or (graphic c) (char-whitespace? (integer->char c)))))]
      [(control cntrl) (class (lambda (c) (or (fx<? c 32) (fx=? c 127))) (lambda (c) (eq? (category c) 'Cc)))]
      [(hex-digit xdigit) (ranges->cset '((48 . 57) (65 . 70) (97 . 102)))]))

  ;; Grapheme extenders: marks and the zero-width joiner.
  (define extend-cset
    (pred->cset (lambda (c) (or (fx=? c #x200D) (and (memq (category c) '(Mn Mc Me)) #t))) #t))

  (define (extend-code? c) (cset-contains? extend-cset c))

  ;;; ------------------------------------------------------------------------------------
  ;;; Parsing SREs
  ;;;
  ;;; The tree: (char code) (set cset) (seq node ...) (alt node ...)
  ;;; (rep min max greedy? node) (group index node) (assert kind).

  (define A-BOS 0) (define A-EOS 1) (define A-BOL 2) (define A-EOL 3)
  (define A-BOW 4) (define A-EOW 5) (define A-NWB 6) (define A-BOG 7) (define A-EOG 8)
  (define A-BOW-ASCII 9) (define A-EOW-ASCII 10) (define A-NWB-ASCII 11)

  (define bar (string->symbol "|"))

  (define (bad-sre x)
    (assertion-violation 'regexp "invalid SRE" x))

  (define (cset-sre? x)
    (cond
      [(char? x) #t]
      [(string? x) (fx=? (string-length x) 1)]
      [(symbol? x) (and (memq x cset-names) #t)]
      [(char-set? x) #t]
      [(pair? x)
       (let ([op (car x)])
         (cond
           [(string? op) (null? (cdr x))]
           [(memq op '(/ char-range char-set)) #t]
           [(or (eq? op bar)
                (memq op '(or and & - difference ~ complement w/case w/nocase w/ascii w/unicode)))
            (and (list? (cdr x)) (for-all cset-sre? (cdr x)))]
           [else #f]))]
      [else #f]))

  (define (char-node ch ci?)
    (let ([c (char->integer ch)])
      (if ci?
        (let ([codes (case-closure (list c))])
          (if (null? (cdr codes)) (list 'char c) (list 'set (codes->cset codes))))
        (list 'char c))))

  (define (seq-node nodes)
    (if (and (pair? nodes) (null? (cdr nodes))) (car nodes) (cons 'seq nodes)))

  (define (string->cset s)
    (codes->cset (map char->integer (string->list s))))

  (define (srfi14->cset cs)
    (codes->cset (map char->integer (char-set->list cs))))

  ;; (/ "az" #\0 #\9) and friends: the characters in order, taken in pairs.
  (define (range-specs who specs)
    (let ([chars (apply append (map (lambda (s)
                                      (cond
                                        [(char? s) (list s)]
                                        [(string? s) (string->list s)]
                                        [else (bad-sre (cons who specs))]))
                                    specs))])
      (let loop ([cs chars] [acc '()])
        (cond
          [(null? cs) acc]
          [(null? (cdr cs)) (bad-sre (cons who specs))]
          [else (loop (cddr cs) (cons (cons (char->integer (car cs)) (char->integer (cadr cs))) acc))]))))

  (define (parse-cset x ci? ascii?)
    (define (leaf cs) (if ci? (cset-fold-case cs) cs))
    (define (sub y) (parse-cset y ci? ascii?))
    (define (union ys) (fold-left (lambda (acc y) (cset-union acc (sub y))) cset:empty ys))
    (cond
      [(char? x) (leaf (codes->cset (list (char->integer x))))]
      [(string? x) (leaf (string->cset x))]
      [(symbol? x) (leaf (or (named-cset x ascii?) (bad-sre x)))]
      [(char-set? x) (leaf (srfi14->cset x))]
      [(pair? x)
       (let ([op (car x)] [args (cdr x)])
         (cond
           [(string? op) (leaf (string->cset op))]
           [(eq? op 'char-set) (leaf (string->cset (car args)))]
           [(memq op '(/ char-range)) (leaf (ranges->cset (range-specs op args)))]
           [(or (eq? op 'or) (eq? op bar)) (union args)]
           [(memq op '(and &))
            (if (null? args)
              cset:full
              (fold-left (lambda (acc y) (cset-intersection acc (sub y))) (sub (car args)) (cdr args)))]
           [(memq op '(- difference))
            (if (null? args) (bad-sre x) (cset-difference (sub (car args)) (union (cdr args))))]
           [(memq op '(~ complement)) (cset-complement (union args))]
           [(eq? op 'w/case) (parse-cset (cons 'or args) #f ascii?)]
           [(eq? op 'w/nocase) (parse-cset (cons 'or args) #t ascii?)]
           [(eq? op 'w/ascii) (parse-cset (cons 'or args) ci? #t)]
           [(eq? op 'w/unicode) (parse-cset (cons 'or args) ci? #f)]
           [else (bad-sre x)]))]
      [else (bad-sre x)]))

  (define grapheme-node
    (list 'alt
      (list 'seq (list 'char 13) (list 'char 10))
      (list 'seq (list 'set cset:full) (list 'rep 0 #f #t (list 'set extend-cset)))))

  ;; Parses SRE into (values tree group-count names), NAMES an alist (name . index) in
  ;; order of appearance.
  (define (parse-sre sre)
    (let ([groups 0] [names '()])
      (define (new-group!)
        (set! groups (fx+ groups 1))
        groups)
      (define (count x)
        (unless (and (fixnum? x) (fx>=? x 0)) (bad-sre x))
        x)
      (define (parse x ci? ascii? cap?)
        (define (all ys) (map (lambda (y) (parse y ci? ascii? cap?)) ys))
        (define (body ys) (seq-node (all ys)))
        (cond
          [(char? x) (char-node x ci?)]
          [(string? x) (seq-node (map (lambda (ch) (char-node ch ci?)) (string->list x)))]
          [(symbol? x)
           (case x
             [(bos) (list 'assert A-BOS)]
             [(eos) (list 'assert A-EOS)]
             [(bol) (list 'assert A-BOL)]
             [(eol) (list 'assert A-EOL)]
             [(bow) (list 'assert (if ascii? A-BOW-ASCII A-BOW))]
             [(eow) (list 'assert (if ascii? A-EOW-ASCII A-EOW))]
             [(nwb) (list 'assert (if ascii? A-NWB-ASCII A-NWB))]
             [(bog) (list 'assert A-BOG)]
             [(eog) (list 'assert A-EOG)]
             [(word) (parse '(word+ any) ci? ascii? cap?)]
             [(grapheme) grapheme-node]
             [(epsilon) (list 'seq)]
             [else (list 'set (parse-cset x ci? ascii?))])]
          [(char-set? x) (list 'set (parse-cset x ci? ascii?))]
          [(regexp? x) (parse (regexp-sre x) ci? ascii? cap?)]
          [(pair? x)
           (let ([op (car x)] [args (cdr x)])
             (unless (list? args) (bad-sre x))
             (cond
               [(memq op '(: seq)) (body args)]
               [(or (eq? op 'or) (eq? op bar))
                (if (for-all cset-sre? args)
                  (list 'set (parse-cset x ci? ascii?))
                  (cons 'alt (all args)))]
               [(eq? op '*) (list 'rep 0 #f #t (body args))]
               [(eq? op '+) (list 'rep 1 #f #t (body args))]
               [(eq? op '?) (list 'rep 0 1 #t (body args))]
               [(eq? op '*?) (list 'rep 0 #f #f (body args))]
               [(eq? op '+?) (list 'rep 1 #f #f (body args))]
               [(eq? op '??) (list 'rep 0 1 #f (body args))]
               [(eq? op '=)
                (when (null? args) (bad-sre x))
                (let ([n (count (car args))]) (list 'rep n n #t (body (cdr args))))]
               [(eq? op '>=)
                (when (null? args) (bad-sre x))
                (list 'rep (count (car args)) #f #t (body (cdr args)))]
               [(memq op '(** **?))
                (when (or (null? args) (null? (cdr args))) (bad-sre x))
                (let ([n (count (car args))] [m (count (cadr args))])
                  (when (fx>? n m) (bad-sre x))
                  (list 'rep n m (eq? op '**) (body (cddr args))))]
               [(memq op '($ submatch))
                (if cap?
                  (let ([i (new-group!)]) (list 'group i (body args)))
                  (body args))]
               [(memq op '(-> submatch-named))
                (when (or (null? args) (not (symbol? (car args)))) (bad-sre x))
                (if cap?
                  (let ([i (new-group!)])
                    (set! names (cons (cons (car args) i) names))
                    (list 'group i (body (cdr args))))
                  (body (cdr args)))]
               [(eq? op 'w/case) (seq-node (map (lambda (y) (parse y #f ascii? cap?)) args))]
               [(eq? op 'w/nocase) (seq-node (map (lambda (y) (parse y #t ascii? cap?)) args))]
               [(eq? op 'w/ascii) (seq-node (map (lambda (y) (parse y ci? #t cap?)) args))]
               [(eq? op 'w/unicode) (seq-node (map (lambda (y) (parse y ci? #f cap?)) args))]
               [(eq? op 'w/nocapture) (seq-node (map (lambda (y) (parse y ci? ascii? #f)) args))]
               [(eq? op 'word)
                (cons 'seq (append (list (parse 'bow ci? ascii? cap?))
                                   (all args)
                                   (list (parse 'eow ci? ascii? cap?))))]
               [(eq? op 'word+)
                (parse `(word (+ (and (or alphanumeric "_") (or ,@args)))) ci? ascii? cap?)]
               [(memq op '(backref look-ahead look-behind neg-look-ahead neg-look-behind))
                (assertion-violation 'regexp "unsupported SRE" x)]
               [(cset-sre? x) (list 'set (parse-cset x ci? ascii?))]
               [else (bad-sre x)]))]
          [else (bad-sre x)]))
      (let ([tree (parse sre #f #f #t)])
        (values tree groups (reverse names)))))

  ;;; ------------------------------------------------------------------------------------
  ;;; The NFA program

  (define OP-CHAR 0)   ; A1: a code point
  (define OP-SET 1)    ; A1: a cset
  (define OP-ANY 2)
  (define OP-SPLIT 3)  ; A1: the preferred target, A2: the other
  (define OP-JMP 4)    ; A1: the target
  (define OP-SAVE 5)   ; A1: a capture slot
  (define OP-ASSERT 6) ; A1: an assertion kind
  (define OP-MATCH 7)

  (define-record-type (prog make-prog prog?)
    (fields ops a1 a2)
    (sealed #t))

  (define (prog-size p) (vector-length (prog-ops p)))

  ;; Compiles TREE. REVERSE? compiles the mirror image, which matches the reversed strings,
  ;; without captures; it is used to find where a match starts.
  (define (compile-tree tree reverse?)
    (let ([ops (make-vector 16 0)] [a1 (make-vector 16 #f)] [a2 (make-vector 16 #f)] [n 0])
      (define (emit! op x y)
        (when (fx=? n (vector-length ops))
          (let ([grow (lambda (v fill)
                        (let ([w (make-vector (fx* 2 (vector-length v)) fill)])
                          (let loop ([i 0])
                            (when (fx<? i n) (vector-set! w i (vector-ref v i)) (loop (fx+ i 1))))
                          w))])
            (set! ops (grow ops 0))
            (set! a1 (grow a1 #f))
            (set! a2 (grow a2 #f))))
        (vector-set! ops n op)
        (vector-set! a1 n x)
        (vector-set! a2 n y)
        (set! n (fx+ n 1))
        (fx- n 1))
      (define (flip kind)
        (cond
          [(fx=? kind A-BOS) A-EOS] [(fx=? kind A-EOS) A-BOS]
          [(fx=? kind A-BOL) A-EOL] [(fx=? kind A-EOL) A-BOL]
          [(fx=? kind A-BOW) A-EOW] [(fx=? kind A-EOW) A-BOW]
          [(fx=? kind A-BOW-ASCII) A-EOW-ASCII] [(fx=? kind A-EOW-ASCII) A-BOW-ASCII]
          [(fx=? kind A-BOG) A-EOG] [(fx=? kind A-EOG) A-BOG]
          [else kind]))
      (define (gen node)
        (case (car node)
          [(char) (emit! OP-CHAR (cadr node) #f)]
          [(set)
           (let ([cs (cadr node)])
             (cond
               [(cset-full? cs) (emit! OP-ANY #f #f)]
               [(cset-singleton cs) => (lambda (c) (emit! OP-CHAR c #f))]
               [else (emit! OP-SET cs #f)]))]
          [(seq) (for-each gen (if reverse? (reverse (cdr node)) (cdr node)))]
          [(alt)
           (let ([alts (cdr node)])
             (if (null? alts)
               (emit! OP-SET cset:empty #f)
               (let loop ([alts alts] [jumps '()])
                 (if (null? (cdr alts))
                   (begin
                     (gen (car alts))
                     (for-each (lambda (j) (vector-set! a1 j n)) jumps))
                   (let ([split (emit! OP-SPLIT #f #f)])
                     (vector-set! a1 split n)
                     (gen (car alts))
                     (let ([j (emit! OP-JMP #f #f)])
                       (vector-set! a2 split n)
                       (loop (cdr alts) (cons j jumps))))))))]
          [(rep)
           (let ([lo (list-ref node 1)] [hi (list-ref node 2)]
                 [greedy? (list-ref node 3)] [body (list-ref node 4)])
             (cond
               [(not hi)
                (if (fx>? lo 0)
                  ;; body{lo-1} L: body; split L, exit
                  (begin
                    (let loop ([k 1]) (when (fx<? k lo) (gen body) (loop (fx+ k 1))))
                    (let ([top n])
                      (gen body)
                      (let ([split (emit! OP-SPLIT #f #f)])
                        (if greedy?
                          (begin (vector-set! a1 split top) (vector-set! a2 split n))
                          (begin (vector-set! a1 split n) (vector-set! a2 split top))))))
                  ;; L: split body, exit; body; jmp L
                  (let* ([split (emit! OP-SPLIT #f #f)] [start n])
                    (gen body)
                    (emit! OP-JMP split #f)
                    (if greedy?
                      (begin (vector-set! a1 split start) (vector-set! a2 split n))
                      (begin (vector-set! a1 split n) (vector-set! a2 split start)))))]
               [else
                (let loop ([k 0]) (when (fx<? k lo) (gen body) (loop (fx+ k 1))))
                (let loop ([k lo] [splits '()])
                  (if (fx<? k hi)
                    (let ([split (emit! OP-SPLIT #f #f)])
                      (gen body)
                      (loop (fx+ k 1) (cons split splits)))
                    (for-each
                      (lambda (split)
                        (if greedy?
                          (begin (vector-set! a1 split (fx+ split 1)) (vector-set! a2 split n))
                          (begin (vector-set! a1 split n) (vector-set! a2 split (fx+ split 1)))))
                      splits)))]))]
          [(group)
           (if reverse?
             (gen (caddr node))
             (let ([i (cadr node)])
               (emit! OP-SAVE (fx* 2 i) #f)
               (gen (caddr node))
               (emit! OP-SAVE (fx+ (fx* 2 i) 1) #f)))]
          [(assert) (emit! OP-ASSERT (if reverse? (flip (cadr node)) (cadr node)) #f)]))
      (unless reverse? (emit! OP-SAVE 0 #f))
      (gen tree)
      (unless reverse? (emit! OP-SAVE 1 #f))
      (emit! OP-MATCH #f #f)
      (let ([shrink (lambda (v)
                      (let ([w (make-vector n)])
                        (let loop ([i 0])
                          (when (fx<? i n) (vector-set! w i (vector-ref v i)) (loop (fx+ i 1))))
                        w))])
        (make-prog (shrink ops) (shrink a1) (shrink a2)))))

  ;; Whether every assertion of P is `bos` or `eos`, so that the DFA can run it.
  (define (dfa-able? p)
    (let ([ops (prog-ops p)] [a1 (prog-a1 p)])
      (let loop ([i 0])
        (or (fx=? i (vector-length ops))
            (and (or (not (fx=? (vector-ref ops i) OP-ASSERT))
                     (let ([k (vector-ref a1 i)]) (or (fx=? k A-BOS) (fx=? k A-EOS))))
                 (loop (fx+ i 1)))))))

  ;; The characters a match can start with, as a cset, or #f when a match can be empty or
  ;; starts with an assertion.
  (define (first-chars p)
    (let ([ops (prog-ops p)] [a1 (prog-a1 p)] [a2 (prog-a2 p)]
          [seen (make-vector (prog-size p) #f)])
      (call/cc
        (lambda (give-up)
          (let visit ([pc 0] [acc cset:empty])
            (if (vector-ref seen pc)
              acc
              (let ([op (vector-ref ops pc)])
                (vector-set! seen pc #t)
                (cond
                  [(fx=? op OP-SPLIT) (visit (vector-ref a2 pc) (visit (vector-ref a1 pc) acc))]
                  [(fx=? op OP-JMP) (visit (vector-ref a1 pc) acc)]
                  [(fx=? op OP-SAVE) (visit (fx+ pc 1) acc)]
                  [(fx=? op OP-CHAR) (cset-union acc (codes->cset (list (vector-ref a1 pc))))]
                  [(fx=? op OP-SET) (cset-union acc (vector-ref a1 pc))]
                  [else (give-up #f)]))))))))

  ;;; ------------------------------------------------------------------------------------
  ;;; Assertions

  (define (word-code? c ascii?)
    (or (fx=? c 95)
        (if ascii?
          (or (and (fx>=? c 48) (fx<=? c 57)) (and (fx>=? c 65) (fx<=? c 90)) (and (fx>=? c 97) (fx<=? c 122)))
          (let ([ch (integer->char c)])
            (or (char-alphabetic? ch) (eq? (char-general-category ch) 'Nd))))))

  (define (code-at str i) (char->integer (string-ref str i)))

  (define (word-before? str i start ascii?)
    (and (fx>? i start) (word-code? (code-at str (fx- i 1)) ascii?)))

  (define (word-after? str i end ascii?)
    (and (fx<? i end) (word-code? (code-at str i) ascii?)))

  (define (grapheme-boundary? str i start end)
    (or (fx=? i start) (fx=? i end)
        (let ([c (code-at str i)])
          (not (or (extend-code? c)
                   (and (fx=? c 10) (fx=? (code-at str (fx- i 1)) 13)))))))

  (define (assert-ok? kind str i start end)
    (cond
      [(fx=? kind A-BOS) (fx=? i start)]
      [(fx=? kind A-EOS) (fx=? i end)]
      [(fx=? kind A-BOL) (or (fx=? i start) (fx=? (code-at str (fx- i 1)) 10))]
      [(fx=? kind A-EOL) (or (fx=? i end) (fx=? (code-at str i) 10))]
      [(or (fx=? kind A-BOW) (fx=? kind A-BOW-ASCII))
       (let ([ascii? (fx=? kind A-BOW-ASCII)])
         (and (word-after? str i end ascii?) (not (word-before? str i start ascii?))))]
      [(or (fx=? kind A-EOW) (fx=? kind A-EOW-ASCII))
       (let ([ascii? (fx=? kind A-EOW-ASCII)])
         (and (word-before? str i start ascii?) (not (word-after? str i end ascii?))))]
      [(or (fx=? kind A-NWB) (fx=? kind A-NWB-ASCII))
       (let ([ascii? (fx=? kind A-NWB-ASCII)])
         (eq? (word-before? str i start ascii?) (word-after? str i end ascii?)))]
      [(fx=? kind A-BOG) (and (fx<? i end) (grapheme-boundary? str i start end))]
      [(fx=? kind A-EOG) (and (fx>? i start) (grapheme-boundary? str i start end))]
      [else #f]))

  ;;; ------------------------------------------------------------------------------------
  ;;; The Pike VM
  ;;;
  ;;; Runs RX on STR from I0. START and END delimit the string for the assertions; no
  ;;; character at or after LIMIT is consumed. ANCHORED? tries a match at I0 only; FULL?
  ;;; accepts only a match that ends at END. Returns the capture vector of the match
  ;;; (start0 end0 start1 end1 ...), or #f.

  (define (pike rx str i0 start end anchored? limit full?)
    (let* ([p (regexp-prog rx)]
           [ops (prog-ops p)] [a1 (prog-a1 p)] [a2 (prog-a2 p)]
           [n (vector-length ops)]
           [marks (make-vector n -1)]
           [init (make-vector (regexp-slots rx) #f)]
           [first (regexp-first rx)])
      ;; Adds the thread at PC, with captures CAPS at position I, to the list PCS/CAPSV that
      ;; holds CNT threads; returns the new count. Threads are added in priority order.
      (define (add! pcs capsv cnt pc caps gen i)
        (let add ([pc pc] [caps caps] [cnt cnt])
          (if (fx=? (vector-ref marks pc) gen)
            cnt
            (let ([op (vector-ref ops pc)])
              (vector-set! marks pc gen)
              (cond
                [(fx=? op OP-SPLIT)
                 (add (vector-ref a2 pc) caps (add (vector-ref a1 pc) caps cnt))]
                [(fx=? op OP-JMP) (add (vector-ref a1 pc) caps cnt)]
                [(fx=? op OP-SAVE)
                 (let ([c (copy-vector caps)])
                   (vector-set! c (vector-ref a1 pc) i)
                   (add (fx+ pc 1) c cnt))]
                [(fx=? op OP-ASSERT)
                 (if (assert-ok? (vector-ref a1 pc) str i start end)
                   (add (fx+ pc 1) caps cnt)
                   cnt)]
                [else
                 (vector-set! pcs cnt pc)
                 (vector-set! capsv cnt caps)
                 (fx+ cnt 1)])))))
      ;; The next position from J where a match can start, or #f.
      (define (next-start j)
        (if first
          (let scan ([j j])
            (cond
              [(fx>=? j limit) #f]
              [(cset-contains? first (code-at str j)) j]
              [else (scan (fx+ j 1))]))
          (and (fx<=? j limit) j)))
      (let ([cpcs (make-vector n 0)] [ccaps (make-vector n #f)]
            [npcs (make-vector n 0)] [ncaps (make-vector n #f)])
        (let run ([i i0] [cnt -1] [gen 0] [matched #f]
                  [cpcs cpcs] [ccaps ccaps] [npcs npcs] [ncaps ncaps])
          (define (restart j gen)
            (let ([j (next-start j)])
              (if j
                (let ([gen (fx+ gen 1)])
                  (run j (add! cpcs ccaps 0 0 init gen j) gen matched cpcs ccaps npcs ncaps))
                matched)))
          (cond
            [(fx=? cnt -1)
             ;; The first call.
             (if anchored?
               (run i (add! cpcs ccaps 0 0 init 1 i) 1 #f cpcs ccaps npcs ncaps)
               (restart i gen))]
            [(fx=? cnt 0)
             (if (or anchored? matched (fx>=? i limit)) matched (restart (fx+ i 1) gen))]
            [else
             (let ([c (if (fx<? i limit) (code-at str i) -1)]
                   [g (fx+ gen 1)]
                   [i+1 (fx+ i 1)])
               (let step ([k 0] [ncnt 0] [matched matched])
                 (define (finish ncnt matched)
                   (cond
                     [(fx>=? i limit) matched]
                     [(or anchored? matched)
                      (run i+1 ncnt g matched npcs ncaps cpcs ccaps)]
                     [(fx=? ncnt 0) (restart i+1 g)]
                     [else
                      (run i+1 (add! npcs ncaps ncnt 0 init g i+1) g matched npcs ncaps cpcs ccaps)]))
                 (if (fx=? k cnt)
                   (finish ncnt matched)
                   (let* ([pc (vector-ref cpcs k)] [op (vector-ref ops pc)])
                     (cond
                       [(fx=? op OP-MATCH)
                        (if (and full? (not (fx=? i end)))
                          (step (fx+ k 1) ncnt matched)
                          ;; The lower-priority threads are dropped.
                          (finish ncnt (vector-ref ccaps k)))]
                       [(and (fx>=? c 0)
                             (cond
                               [(fx=? op OP-CHAR) (fx=? c (vector-ref a1 pc))]
                               [(fx=? op OP-SET) (cset-contains? (vector-ref a1 pc) c)]
                               [else (fx=? op OP-ANY)]))
                        (step (fx+ k 1)
                              (add! npcs ncaps ncnt (fx+ pc 1) (vector-ref ccaps k) g i+1)
                              matched)]
                       [else (step (fx+ k 1) ncnt matched)])))))])))))

  (define (copy-vector v)
    (let* ([n (vector-length v)] [w (make-vector n)])
      (let loop ([i 0])
        (when (fx<? i n) (vector-set! w i (vector-ref v i)) (loop (fx+ i 1))))
      w))

  ;;; ------------------------------------------------------------------------------------
  ;;; The lazy DFA
  ;;;
  ;;; A state is the ordered list of the NFA threads at one position: the program counters of
  ;;; consuming instructions, MATCH, and the `eos` assertions still pending, plus -1 for the
  ;;; restart thread of an unanchored search (a new match may begin at the next position; it
  ;;; is the lowest priority). In FIRST mode the threads after a MATCH are dropped, which is
  ;;; the leftmost-first rule; in ALL mode they are kept, so a state accepts if any thread
  ;;; does. Transitions are cached: a 256-slot vector for Latin-1 and an open-addressing table
  ;;; for the other characters. Lookups take no lock; misses are computed under the DFA's
  ;;; mutex. A DFA that would need more than `max-states` states gives up, and the regexp
  ;;; falls back to the Pike VM.

  (define max-states 2000)

  (define-record-type (dstate make-dstate dstate?)
    (fields pcs match? eos (mutable trans) (mutable hi) (mutable end0) (mutable end1))
    (sealed #t))

  (define (dead? s) (fx=? (vector-length (dstate-pcs s)) 0))

  (define-record-type (dfa make-dfa dfa?)
    (fields prog first? unanchored? table (mutable count) (mutable init0) (mutable init1)
            marks out (mutable gen) lock (mutable broken?))
    (sealed #t))

  (define (new-dfa p first? unanchored?)
    (let ([n (prog-size p)])
      (make-dfa p first? unanchored? (make-hashtable equal-hash equal?) 0 #f #f
                (make-vector n -1) (make-vector (fx+ n 1) 0) 0 (make-mutex) #f)))

  (define (next-gen! d)
    (let ([g (fx+ (dfa-gen d) 1)]) (dfa-gen-set! d g) g))

  ;; Adds the closure of PC to the output buffer, which holds CNT entries.
  (define (closure! d pc at-begin? cnt gen)
    (let* ([p (dfa-prog d)] [ops (prog-ops p)] [a1 (prog-a1 p)] [a2 (prog-a2 p)]
           [marks (dfa-marks d)] [out (dfa-out d)])
      (let visit ([pc pc] [cnt cnt])
        (if (fx=? (vector-ref marks pc) gen)
          cnt
          (let ([op (vector-ref ops pc)])
            (vector-set! marks pc gen)
            (cond
              [(fx=? op OP-SPLIT) (visit (vector-ref a2 pc) (visit (vector-ref a1 pc) cnt))]
              [(fx=? op OP-JMP) (visit (vector-ref a1 pc) cnt)]
              [(fx=? op OP-SAVE) (visit (fx+ pc 1) cnt)]
              [(and (fx=? op OP-ASSERT) (fx=? (vector-ref a1 pc) A-BOS))
               (if at-begin? (visit (fx+ pc 1) cnt) cnt)]
              [else (vector-set! out cnt pc) (fx+ cnt 1)]))))))

  ;; The state of the CNT threads in the output buffer, then the restart thread if
  ;; RESTART?. Returns #f when there are too many states.
  (define (intern! d cnt restart?)
    (let* ([p (dfa-prog d)] [ops (prog-ops p)] [out (dfa-out d)]
           [cut (if (dfa-first? d)
                  (let loop ([k 0])
                    (cond
                      [(fx=? k cnt) #f]
                      [(fx=? (vector-ref ops (vector-ref out k)) OP-MATCH) (fx+ k 1)]
                      [else (loop (fx+ k 1))]))
                  #f)]
           [len (or cut cnt)]
           [key (let loop ([k (fx- len 1)] [acc (if (and restart? (not cut)) '(-1) '())])
                  (if (fx<? k 0) acc (loop (fx- k 1) (cons (vector-ref out k) acc))))])
      (or (hashtable-ref (dfa-table d) key #f)
          (and (fx<? (dfa-count d) max-states)
               (let* ([pcs (list->vector key)]
                      [match? (exists (lambda (pc) (and (fx>=? pc 0) (fx=? (vector-ref ops pc) OP-MATCH))) key)]
                      [eos (filter (lambda (pc) (and (fx>=? pc 0) (fx=? (vector-ref ops pc) OP-ASSERT))) key)]
                      [s (make-dstate pcs match? eos (make-vector 256 #f) #f 'unknown 'unknown)])
                 (hashtable-set! (dfa-table d) key s)
                 (dfa-count-set! d (fx+ (dfa-count d) 1))
                 s)))))

  (define (dfa-initial d at-begin?)
    (or (if at-begin? (dfa-init1 d) (dfa-init0 d))
        (with-mutex (dfa-lock d)
          (let* ([gen (next-gen! d)]
                 [cnt (closure! d 0 at-begin? 0 gen)]
                 [s (intern! d cnt (dfa-unanchored? d))])
            (if s
              (if at-begin? (dfa-init1-set! d s) (dfa-init0-set! d s))
              (dfa-broken?-set! d #t))
            s))))

  (define (step-state! d s c)
    (let* ([p (dfa-prog d)] [ops (prog-ops p)] [a1 (prog-a1 p)]
           [pcs (dstate-pcs s)] [n (vector-length pcs)] [gen (next-gen! d)])
      (let loop ([k 0] [cnt 0] [restart? #f])
        (if (fx=? k n)
          (intern! d cnt restart?)
          (let ([pc (vector-ref pcs k)])
            (if (fx=? pc -1)
              (loop (fx+ k 1) (closure! d 0 #f cnt gen) #t)
              (let ([op (vector-ref ops pc)])
                (if (cond
                      [(fx=? op OP-CHAR) (fx=? c (vector-ref a1 pc))]
                      [(fx=? op OP-SET) (cset-contains? (vector-ref a1 pc) c)]
                      [else (fx=? op OP-ANY)])
                  (loop (fx+ k 1) (closure! d (fx+ pc 1) #f cnt gen) restart?)
                  (loop (fx+ k 1) cnt restart?)))))))))

  ;; The transition of S on code point C, or #f when the DFA has given up.
  (define (dfa-next d s c)
    (if (fx<? c 256)
      (or (vector-ref (dstate-trans s) c) (dfa-compute! d s c))
      (or (hi-lookup (dstate-hi s) c) (dfa-compute! d s c))))

  (define (dfa-compute! d s c)
    (with-mutex (dfa-lock d)
      (or (if (fx<? c 256) (vector-ref (dstate-trans s) c) (hi-lookup (dstate-hi s) c))
          (let ([t (step-state! d s c)])
            (cond
              [(not t) (dfa-broken?-set! d #t) #f]
              [(fx<? c 256) (vector-set! (dstate-trans s) c t) t]
              [else (hi-insert! s c t) t])))))

  ;; The table for characters above U+00FF: a vector #(key0 value0 key1 value1 ...) of a
  ;; power-of-two number of slots, probed linearly. A slot is written value first, so a
  ;; reader never sees a key without its value.
  (define (hi-lookup table c)
    (and table
         (let* ([slots (fxarithmetic-shift-right (vector-length table) 1)]
                [mask (fx- slots 1)])
           (let probe ([i (fxand (fx* c 40503) mask)])
             (let ([k (vector-ref table (fx* 2 i))])
               (cond
                 [(not k) #f]
                 [(fx=? k c) (vector-ref table (fx+ (fx* 2 i) 1))]
                 [else (probe (fxand (fx+ i 1) mask))]))))))

  (define (hi-insert! s c t)
    (let* ([table (dstate-hi s)]
           [table (if (and table (fx<? (fx* 4 (hi-count table)) (vector-length table)))
                    table
                    (let ([new (make-vector (if table (fx* 2 (vector-length table)) 32) #f)])
                      (when table
                        (let loop ([i 0])
                          (when (fx<? i (vector-length table))
                            (let ([k (vector-ref table i)])
                              (when k (hi-put! new k (vector-ref table (fx+ i 1)))))
                            (loop (fx+ i 2)))))
                      (dstate-hi-set! s new)
                      new))])
      (hi-put! table c t)))

  (define (hi-count table)
    (let loop ([i 0] [n 0])
      (if (fx>=? i (vector-length table))
        n
        (loop (fx+ i 2) (if (vector-ref table i) (fx+ n 1) n)))))

  (define (hi-put! table c t)
    (let* ([slots (fxarithmetic-shift-right (vector-length table) 1)]
           [mask (fx- slots 1)])
      (let probe ([i (fxand (fx* c 40503) mask)])
        (if (vector-ref table (fx* 2 i))
          (probe (fxand (fx+ i 1) mask))
          (begin
            (vector-set! table (fx+ (fx* 2 i) 1) t)
            (vector-set! table (fx* 2 i) c))))))

  ;; Whether S accepts at the end of the input: a MATCH, or a pending `eos` that leads to one.
  (define (end-accept? d s at-begin?)
    (let ([cached (if at-begin? (dstate-end1 s) (dstate-end0 s))])
      (if (eq? cached 'unknown)
        (let ([r (or (dstate-match? s)
                     (and (pair? (dstate-eos s))
                          (with-mutex (dfa-lock d) (eos-reaches-match? d (dstate-eos s) at-begin?))))])
          (if at-begin? (dstate-end1-set! s r) (dstate-end0-set! s r))
          r)
        cached)))

  (define (eos-reaches-match? d pcs at-begin?)
    (let* ([p (dfa-prog d)] [ops (prog-ops p)] [a1 (prog-a1 p)] [a2 (prog-a2 p)]
           [marks (dfa-marks d)] [gen (next-gen! d)])
      (exists
        (lambda (start)
          (let visit ([pc (fx+ start 1)])
            (and (not (fx=? (vector-ref marks pc) gen))
                 (let ([op (vector-ref ops pc)])
                   (vector-set! marks pc gen)
                   (cond
                     [(fx=? op OP-MATCH) #t]
                     [(fx=? op OP-SPLIT) (or (visit (vector-ref a1 pc)) (visit (vector-ref a2 pc)))]
                     [(fx=? op OP-JMP) (visit (vector-ref a1 pc))]
                     [(fx=? op OP-SAVE) (visit (fx+ pc 1))]
                     [(fx=? op OP-ASSERT)
                      (and (or (fx=? (vector-ref a1 pc) A-EOS) at-begin?) (visit (fx+ pc 1)))]
                     [else #f])))))
        pcs)))

  ;; Where the leftmost-first match from I0 ends: an index, #f for no match, or 'fail when
  ;; the DFA gave up.
  (define (dfa-match-end d str i0 start end first)
    (let ([s0 (dfa-initial d (fx=? i0 start))]
          [skip (dfa-initial d #f)])
      (if (not (and s0 skip))
        'fail
        (let loop ([s s0] [i i0] [last (and (dstate-match? s0) i0)])
          (cond
            [(fx=? i end) (if (end-accept? d s (fx=? i start)) end last)]
            [(and first (eq? s skip) (not last))
             ;; Only the restart thread is alive: skip to a character that can start a match.
             (let scan ([j i])
               (cond
                 [(fx=? j end) (loop s j last)]
                 [(cset-contains? first (code-at str j))
                  (let ([t (dfa-next d s (code-at str j))])
                    (cond
                      [(not t) 'fail]
                      [else (loop t (fx+ j 1) (if (dstate-match? t) (fx+ j 1) last))]))]
                 [else (scan (fx+ j 1))]))]
            [else
             (let ([t (dfa-next d s (code-at str i))])
               (cond
                 [(not t) 'fail]
                 [(dead? t) last]
                 [else (loop t (fx+ i 1) (if (dstate-match? t) (fx+ i 1) last))]))])))))

  ;; Where the match that ends at E starts, scanning the reversed program back to LO:
  ;; the smallest such index, #f, or 'fail.
  (define (dfa-match-start d str e lo start end)
    (let ([s0 (dfa-initial d (fx=? e end))])
      (if (not s0)
        'fail
        (let loop ([s s0] [i e] [best (and (dstate-match? s0) e)])
          (if (fx=? i lo)
            (if (and (fx=? lo start) (end-accept? d s (fx=? i end))) lo best)
            (let ([t (dfa-next d s (code-at str (fx- i 1)))])
              (cond
                [(not t) 'fail]
                [(dead? t) best]
                [else (loop t (fx- i 1) (if (dstate-match? t) (fx- i 1) best))])))))))

  ;; Whether all of STR[start, end) matches: #t, #f or 'fail.
  (define (dfa-whole-match? d str start end)
    (let ([s0 (dfa-initial d #t)])
      (if (not s0)
        'fail
        (let loop ([s s0] [i start])
          (if (fx=? i end)
            (end-accept? d s (fx=? i start))
            (let ([t (dfa-next d s (code-at str i))])
              (cond
                [(not t) 'fail]
                [(dead? t) #f]
                [else (loop t (fx+ i 1))])))))))

  ;;; ------------------------------------------------------------------------------------
  ;;; Compiled regexps

  (define-record-type (regexp-rec make-regexp regexp?)
    (fields sre prog groups names first dfa-ok?
            (mutable forward) (mutable backward) (mutable whole) lock)
    (sealed #t))

  (define (regexp-slots rx) (fx* 2 (fx+ (regexp-groups rx) 1)))
  (define (regexp-prog rx) (regexp-rec-prog rx))
  (define (regexp-sre rx) (regexp-rec-sre rx))
  (define (regexp-groups rx) (regexp-rec-groups rx))
  (define (regexp-first rx) (regexp-rec-first rx))

  (define (compile-sre sre)
    (let-values ([(tree groups names) (parse-sre sre)])
      (let ([p (compile-tree tree #f)])
        (make-regexp sre p groups names (first-chars p) (dfa-able? p) #f #f #f (make-mutex)))))

  ;; The DFAs of RX, made on first use; #f when RX cannot use them or one gave up.
  (define (get-dfa rx which)
    (and (regexp-rec-dfa-ok? rx)
         (let ([d (case which
                    [(forward) (regexp-rec-forward rx)]
                    [(backward) (regexp-rec-backward rx)]
                    [else (regexp-rec-whole rx)])])
           (if d
             (and (not (dfa-broken? d)) d)
             (with-mutex (regexp-rec-lock rx)
               (let ([d (case which
                          [(forward)
                           (or (regexp-rec-forward rx)
                               (let ([d (new-dfa (regexp-prog rx) #t #t)]) (regexp-rec-forward-set! rx d) d))]
                          [(backward)
                           (or (regexp-rec-backward rx)
                               (let* ([tree (let-values ([(tree g n) (parse-sre (regexp-sre rx))]) tree)]
                                      [d (new-dfa (compile-tree tree #t) #f #f)])
                                 (regexp-rec-backward-set! rx d)
                                 d))]
                          [else
                           (or (regexp-rec-whole rx)
                               (let ([d (new-dfa (regexp-prog rx) #f #f)]) (regexp-rec-whole-set! rx d) d))])])
                 (and (not (dfa-broken? d)) d)))))))

  ;; A cache of the SREs given directly to the matching procedures.
  (define sre-cache (make-hashtable equal-hash equal?))
  (define sre-cache-size 0)
  (define sre-cache-lock (make-mutex))

  (define (->regexp re)
    (if (regexp? re)
      re
      (or (with-mutex sre-cache-lock (hashtable-ref sre-cache re #f))
          (let ([rx (compile-sre re)])
            (with-mutex sre-cache-lock
              (when (fx>=? sre-cache-size 256)
                (hashtable-clear! sre-cache)
                (set! sre-cache-size 0))
              (hashtable-set! sre-cache re rx)
              (set! sre-cache-size (fx+ sre-cache-size 1)))
            rx))))

  (define (regexp re) (if (regexp? re) re (compile-sre re)))

  (define-syntax rx
    (syntax-rules ()
      [(_ sre ...) (regexp (quasiquote (: sre ...)))]))

  (define (regexp->sre re) (regexp-sre (regexp re)))

  (define (char-set->sre cs) (list (char-set->string cs)))

  (define (valid-sre? x)
    (guard (e [#t #f])
      (parse-sre x)
      #t))

  ;;; Searching

  ;; The capture vector of the leftmost match in STR[i, end), or #f.
  (define (search rx str i start end)
    (let ([fwd (get-dfa rx 'forward)])
      (or (and fwd
               (let ([e (dfa-match-end fwd str i start end (regexp-first rx))])
                 (cond
                   [(eq? e 'fail) #f]
                   [(not e) (vector #f)]
                   [else
                    (let* ([bwd (get-dfa rx 'backward)]
                           [s (and bwd (dfa-match-start bwd str e i start end))])
                      (cond
                        [(or (not s) (eq? s 'fail)) #f]
                        [(fx=? (regexp-groups rx) 0) (vector s e)]
                        [else (pike rx str s start end #t e #f)]))])))
          (pike rx str i start end #f end #f))))

  ;; (vector #f) is the DFA's "no match"; the Pike VM's is #f.
  (define (search* rx str i start end)
    (let ([caps (search rx str i start end)])
      (and caps (vector-ref caps 0) caps)))

  (define (whole rx str start end)
    (let ([d (get-dfa rx 'whole)])
      (let ([r (if d (dfa-whole-match? d str start end) 'fail)])
        (cond
          [(eq? r 'fail) (pike rx str start start end #t end #t)]
          [(not r) #f]
          [(fx=? (regexp-groups rx) 0) (vector start end)]
          [else (pike rx str start start end #t end #t)]))))

  (define (check-bounds who str start end)
    (unless (string? str) (assertion-violation who "not a string" str))
    (unless (and (fixnum? start) (fixnum? end) (fx<=? 0 start) (fx<=? start end)
                 (fx<=? end (string-length str)))
      (assertion-violation who "invalid range" str start end)))

  (define-syntax with-bounds
    (syntax-rules ()
      [(_ who str opts (start end) body ...)
       (let* ([o opts]
              [start (if (and (pair? o) (car o)) (car o) 0)]
              [end (if (and (pair? o) (pair? (cdr o)) (cadr o)) (cadr o) (string-length str))])
         (check-bounds who str start end)
         body ...)]))

  (define (regexp-matches re str . opts)
    (with-bounds 'regexp-matches str opts (start end)
      (let* ([rx (->regexp re)] [caps (whole rx str start end)])
        (and caps (make-match rx str caps)))))

  (define (regexp-matches? re str . opts)
    (with-bounds 'regexp-matches? str opts (start end)
      (let* ([rx (->regexp re)] [d (get-dfa rx 'whole)])
        (let ([r (if d (dfa-whole-match? d str start end) 'fail)])
          (if (eq? r 'fail)
            (and (pike rx str start start end #t end #t) #t)
            r)))))

  (define (regexp-search re str . opts)
    (with-bounds 'regexp-search str opts (start end)
      (let* ([rx (->regexp re)] [caps (search* rx str start start end)])
        (and caps (make-match rx str caps)))))

  ;;; Match objects

  (define-record-type (rmatch make-match regexp-match?)
    (fields regexp string caps)
    (sealed #t))

  (define (regexp-match-count m) (regexp-groups (rmatch-regexp m)))

  (define (field-index who m field)
    (let ([rx (rmatch-regexp m)])
      (cond
        [(and (fixnum? field) (fx<=? 0 field) (fx<=? field (regexp-groups rx))) field]
        [(symbol? field)
         ;; With several groups of one name, the first that matched.
         (let ([is (map cdr (filter (lambda (e) (eq? (car e) field)) (regexp-rec-names rx)))])
           (when (null? is) (assertion-violation who "no such submatch" field))
           (or (find (lambda (i) (vector-ref (rmatch-caps m) (fx* 2 i))) is) (car is)))]
        [else (assertion-violation who "no such submatch" field)])))

  (define (regexp-match-submatch-start m field)
    (vector-ref (rmatch-caps m) (fx* 2 (field-index 'regexp-match-submatch-start m field))))

  (define (regexp-match-submatch-end m field)
    (vector-ref (rmatch-caps m) (fx+ 1 (fx* 2 (field-index 'regexp-match-submatch-end m field)))))

  (define (regexp-match-submatch m field)
    (let* ([i (field-index 'regexp-match-submatch m field)]
           [caps (rmatch-caps m)]
           [s (vector-ref caps (fx* 2 i))]
           [e (vector-ref caps (fx+ 1 (fx* 2 i)))])
      (and s e (substring (rmatch-string m) s e))))

  (define (regexp-match->list m)
    (let loop ([i (regexp-match-count m)] [acc '()])
      (if (fx<? i 0) acc (loop (fx- i 1) (cons (regexp-match-submatch m i) acc)))))

  ;;; Folding and friends

  ;; Calls (kons i match str acc) on each match, I being where the previous match ended, and
  ;; (finish i #f str acc) at the end. A match that is empty and starts where the previous
  ;; one ended is skipped.
  (define (regexp-fold re kons knil str . opts)
    (let* ([rx (->regexp re)]
           [finish (if (pair? opts) (car opts) (lambda (i m str acc) acc))]
           [opts (if (pair? opts) (cdr opts) '())])
      (with-bounds 'regexp-fold str opts (start end)
        (let loop ([i start] [from start] [acc knil])
          (let ([caps (and (fx<=? i end) (search* rx str i start end))])
            (if (not caps)
              (finish from #f str acc)
              (let ([j (vector-ref caps 1)])
                (if (fx=? i j)
                  (if (fx>=? i end)
                    (finish from #f str acc)
                    (loop (fx+ j 1) from acc))
                  (let ([acc (kons from (make-match rx str caps) str acc)])
                    (if (fx>=? j end)
                      (finish j #f str acc)
                      (loop j j acc)))))))))))

  (define (regexp-extract re str . opts)
    (apply regexp-fold re
      (lambda (from m str acc)
        (let ([s (regexp-match-submatch m 0)])
          (if (string=? s "") acc (cons s acc))))
      '()
      str
      (lambda (from m str acc) (reverse acc))
      opts))

  (define (regexp-split re str . opts)
    (with-bounds 'regexp-split str opts (start end)
      (regexp-fold re
        (lambda (from m str acc)
          (let ([i (regexp-match-submatch-start m 0)] [j (regexp-match-submatch-end m 0)])
            (if (fx=? i j)
              acc
              (cons j (cons (substring str (car acc) i) (cdr acc))))))
        (cons start '())
        str
        (lambda (from m str acc)
          (reverse (cons (substring str (car acc) end) (cdr acc))))
        start
        end)))

  (define (regexp-partition re str . opts)
    (with-bounds 'regexp-partition str opts (start end)
      (reverse
        (regexp-fold re
          (lambda (from m str acc)
            (let ([i (regexp-match-submatch-start m 0)] [j (regexp-match-submatch-end m 0)])
              (if (fx=? i j)
                acc
                (cons j (cons (regexp-match-submatch m 0)
                              (cons (substring str (car acc) i) (cdr acc)))))))
          (cons start '())
          str
          (lambda (from m str acc)
            (if (or (fx<? from end) (null? (cdr acc)))
              (cons (substring str (car acc) end) (cdr acc))
              (cdr acc)))
          start
          end))))

  ;; The pieces SUBST makes of match M, in reverse order: a string, a submatch index or name,
  ;; `pre` or `post`, a procedure of the match, or a list of these.
  (define (substitute m str subst start end acc)
    (cond
      [(string? subst) (cons subst acc)]
      [(or (fixnum? subst) (and (symbol? subst) (not (memq subst '(pre post)))))
       (cons (or (regexp-match-submatch m subst) "") acc)]
      [(eq? subst 'pre) (cons (substring str start (regexp-match-submatch-start m 0)) acc)]
      [(eq? subst 'post) (cons (substring str (regexp-match-submatch-end m 0) end) acc)]
      [(procedure? subst) (cons (subst m) acc)]
      [(list? subst)
       (fold-left (lambda (acc x) (substitute m str x start end acc)) acc subst)]
      [else (assertion-violation 'regexp-replace "invalid substitution" subst)]))

  (define (concatenate-reverse pieces)
    (apply string-append (reverse pieces)))

  ;; STR with the COUNTth match (0 for the first) in [start, end) replaced by SUBST.
  (define (regexp-replace re str subst . opts)
    (let* ([rx (->regexp re)]
           [start (if (and (pair? opts) (car opts)) (car opts) 0)]
           [opts (if (pair? opts) (cdr opts) '())]
           [end (if (and (pair? opts) (car opts)) (car opts) (string-length str))]
           [opts (if (pair? opts) (cdr opts) '())]
           [count (if (pair? opts) (car opts) 0)])
      (check-bounds 'regexp-replace str start end)
      (let loop ([i start] [count count])
        (let ([caps (and (fx<=? i end) (search* rx str i start end))])
          (cond
            [(not caps) str]
            [(> count 0)
             (let ([j (vector-ref caps 1)])
               (loop (if (fx=? j (vector-ref caps 0)) (fx+ j 1) j) (- count 1)))]
            [else
             (let ([m (make-match rx str caps)])
               (concatenate-reverse
                 (cons (substring str (vector-ref caps 1) (string-length str))
                       (substitute m str subst start end
                                   (list (substring str 0 (vector-ref caps 0)))))))])))))

  (define (regexp-replace-all re str subst . opts)
    (with-bounds 'regexp-replace-all str opts (start end)
      (regexp-fold re
        (lambda (i m str acc)
          (let ([ms (regexp-match-submatch-start m 0)])
            (substitute m str subst start end
                        (if (fx>=? i ms) acc (cons (substring str i ms) acc)))))
        (list (substring str 0 start))
        str
        (lambda (i m str acc)
          (concatenate-reverse (cons (substring str i (string-length str)) acc)))
        start
        end))))
