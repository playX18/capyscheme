;; Turns the `(c-unit ...)` S-expression produced by the C parser into the source of a
;; Scheme library.
;;
;; Everything the generated library uses is imported with a prefix (`s:` for (rnrs), `ffi:`
;; for (core foreign), `rt:` for (capy c runtime)).

(library (capy c gen)
  (export generate-library)
  (import (rnrs) (capy c classify) (only (capy) box set-box! unbox))

  (define (assq-ref key alist default)
    (let ([p (assq key alist)]) (if p (cdr p) default)))

  (define (section unit key)
    (assq-ref key (cdr unit) '()))

  (define (string-prefix? prefix s)
    (and (<= (string-length prefix) (string-length s))
         (string=? prefix (substring s 0 (string-length prefix)))))

  (define (string-suffix? suffix s)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (<= m n) (string=? suffix (substring s (- n m) n)))))

  (define (filter-map f l)
    (cond [(null? l) '()]
          [(f (car l)) => (lambda (v) (cons v (filter-map f (cdr l))))]
          [else (filter-map f (cdr l))]))

  (define (config-ref config key default)
    (assq-ref key config default))

  (define-record-type unsupported (fields reason))

  (define (unsupported! reason) (raise (make-unsupported reason)))

  (define-syntax try-lower
    (syntax-rules ()
      [(_ expr on-fail)
       (guard (e [(unsupported? e) (on-fail (unsupported-reason e))])
         expr)]))

  (define-record-type model
    (fields
      unit
      os
      records       ; id -> record datum
      enums         ; id -> enum datum
      typedefs      ; name -> type
      in-scope?     ; loc -> boolean
      arch
      ptr-size
      descriptors   ; record id -> (symbol . expression), filled while lowering
      config
      shims))       ; name -> #t (bound to its shim), 'failed, or 'skipped

  (define (build-model unit header config shims)
    (let* ([target (section unit 'target)]
           [os (car (assq-ref 'os target '("unknown")))]
           [arch (car (assq-ref 'arch target '("unknown")))]
           [ptr-size (div (car (assq-ref 'ptr-bits target '(64))) 8)]
           [files (section unit 'files)]
           [only-from (config-ref config 'only-from #f)]
           [scope-files
             (let loop ([fs files] [i 0] [acc '()])
               (cond
                 [(null? fs) acc]
                 [(and (> i 1) (in-default-scope? only-from header (list-ref files 2) (car fs)))
                   (loop (cdr fs) (+ i 1) (cons i acc))]
                 [else (loop (cdr fs) (+ i 1) acc)]))]
           [records (make-eqv-hashtable)]
           [enums (make-eqv-hashtable)]
           [typedefs (make-hashtable string-hash string=?)])
      (for-each (lambda (r) (hashtable-set! records (cadr r) r)) (cdr (assq 'records (cdr unit))))
      (for-each (lambda (e) (hashtable-set! enums (cadr e) e)) (cdr (assq 'enums (cdr unit))))
      (for-each
        (lambda (d)
          (when (eq? (car d) 'typedef)
            (hashtable-set! typedefs (cadr d) (caddr d))))
        (cdr (assq 'decls (cdr unit))))
      (make-model unit os records enums typedefs
        (lambda (loc) (and loc (memv (cadr loc) scope-files) #t))
        arch ptr-size (make-eqv-hashtable)
        config shims)))

  (define (dirname path)
    (let loop ([i (- (string-length path) 1)])
      (cond [(< i 0) ""]
            [(char=? (string-ref path i) #\/) (substring path 0 i)]
            [else (loop (- i 1))])))

  (define (basename path)
    (let ([d (dirname path)])
      (if (string=? d "") path (substring path (+ (string-length d) 1) (string-length path)))))

  ;; Which files' declarations are exported. By default the requested header itself, plus
  ;; the private pieces vendors split it into: `_stdio.h` next to `stdio.h` (macOS), files
  ;; under `bits/` (glibc). `#:only-from` replaces the rule with an explicit list.
  (define (in-default-scope? only-from header requested file)
    (if only-from
      (exists (lambda (suffix) (path-matches? file suffix)) only-from)
      (or (path-matches? file header)
          (and (string=? (dirname file) (dirname requested))
               (string-prefix? "_" (basename file)))
          (string-contains? (string-append (dirname file) "/") "/bits/"))))

  (define (string-contains? s sub)
    (let ([n (string-length s)] [m (string-length sub)])
      (let loop ([i 0])
        (cond [(> (+ i m) n) #f]
              [(string=? (substring s i (+ i m)) sub) #t]
              [else (loop (+ i 1))]))))

  (define (path-matches? path suffix)
    (or (string=? path suffix)
        (string-suffix? (string-append "/" suffix) path)))

  (define (int-type size unsigned?)
    (case size
      [(1) (if unsigned? 'ffi:uint8 'ffi:int8)]
      [(2) (if unsigned? 'ffi:uint16 'ffi:int16)]
      [(4) (if unsigned? 'ffi:uint32 'ffi:int32)]
      [(8) (if unsigned? 'ffi:uint64 'ffi:int64)]
      [else (unsupported! (string-append "integer of " (number->string size) " bytes"))]))

  (define pointer-type '(s:quote *))

  (define (lower m ty)
    (case (car ty)
      [(void) 'ffi:void]
      [(bool) 'ffi:uint8]
      [(int) (int-type (cadr ty) (caddr ty))]
      [(float)
        (case (cadr ty)
          [(4) 'ffi:float]
          [(8) 'ffi:double]
          [else (unsupported! "long double")])]
      [(ptr func array nullptr) pointer-type]
      [(typedef)
        (let ([target (hashtable-ref (model-typedefs m) (cadr ty) #f)])
          (if target
            (lower m target)
            (unsupported! (string-append "unknown type " (cadr ty)))))]
      [(enum)
        (let ([e (hashtable-ref (model-enums m) (cadr ty) #f)])
          (if e (lower m (cadddr e)) (unsupported! "unknown enum")))]
      [(atomic) (lower m (cadr ty))]
      [(record) (record-descriptor m (cadr ty))]
      [(unsupported) (unsupported! (cadr ty))]
      [else (unsupported! (string-append "type " (symbol->string (car ty))))]))

  ;; The base type behind typedefs, enums and `_Atomic`.
  (define (strip m ty)
    (case (car ty)
      [(typedef)
        (let ([target (hashtable-ref (model-typedefs m) (cadr ty) #f)])
          (if target (strip m target) (unsupported! (string-append "unknown type " (cadr ty)))))]
      [(enum)
        (let ([e (hashtable-ref (model-enums m) (cadr ty) #f)])
          (if e (strip m (cadddr e)) (unsupported! "unknown enum")))]
      [(atomic) (strip m (cadr ty))]
      [else ty]))

  (define (record-ref m id) (hashtable-ref (model-records m) id #f))

  ;; (layout SIZE ALIGN (OFFSET ...)) of a complete record, or unsupported.
  (define (record-layout m id)
    (let ([r (record-ref m id)])
      (unless (and r (list-ref r 4) (list-ref r 6))
        (unsupported! "incomplete struct or union"))
      (list-ref r 6)))

  (define (type-size m ty)
    (let ([t (strip m ty)])
      (case (car t)
        [(int float) (if (and (eq? (car t) 'float) (> (cadr t) 8))
                       (unsupported! "long double")
                       (cadr t))]
        [(bool) 1]
        [(ptr func nullptr) (model-ptr-size m)]
        [(array) (* (or (caddr t) 0) (type-size m (cadr t)))]
        [(record) (cadr (record-layout m (cadr t)))]
        [else (unsupported! "type without a size")])))

  (define (leaf-offset l) (car l))
  (define (leaf-size l) (cadr l))
  (define (leaf-align l) (caddr l))
  (define (leaf-expr l) (cadddr l))
  (define (leaf-float l) (car (cddddr l)))

  (define max-expanded-array 32)

  (define (leaves m ty base)
    (let ([t (strip m ty)])
      (case (car t)
        [(record)
          (let* ([id (cadr t)]
                 [lay (record-layout m id)])
            (append-map
              (lambda (f o)
                (cond
                  [(and (caddr f) (zero? (caddr f))) '()]
                  [(caddr f)
                    (let ([unit (cadddr o)])
                      (list (list (+ base (car o)) unit unit (int-type unit #t) #f)))]
                  [else (leaves m (cadr f) (+ base o))]))
              (list-ref (record-ref m id) 5)
              (cadddr lay)))]
        [(array)
          (let ([n (caddr t)] [elem (strip m (cadr t))])
            (cond
              [(or (not n) (zero? n)) '()]
              [(memq (car elem) '(record array))
                (when (> n max-expanded-array) (raise 'chunky))
                (let ([esize (type-size m elem)])
                  (append-map (lambda (i) (leaves m elem (+ base (* i esize))))
                    (iota n)))]
              [else
                (let ([es (type-size m elem)])
                  (list (list base (* n es) es `(s:vector ,(lower m elem) ,n)
                          (and (eq? (car elem) 'float) es))))]))]
        [else
          (let ([sz (type-size m t)])
            (list (list base sz sz (lower m t) (and (eq? (car t) 'float) sz))))])))

  (define (iota n)
    (let loop ([i (- n 1)] [acc '()])
      (if (< i 0) acc (loop (- i 1) (cons i acc)))))

  (define (round-up n a) (* a (div (+ n a -1) a)))

  (define (natural-layout? ls size align)
    (let loop ([ls ls] [off 0] [maxalign 1])
      (if (null? ls)
        (and (= (round-up off maxalign) size) (= maxalign align))
        (let ([l (car ls)])
          (and (= (leaf-offset l) (round-up off (leaf-align l)))
               (loop (cdr ls) (+ (leaf-offset l) (leaf-size l)) (max maxalign (leaf-align l))))))))

  (define (overlaps? l lo hi)
    (and (< (leaf-offset l) hi) (> (+ (leaf-offset l) (leaf-size l)) lo)))

  (define (chunk-descriptor m ls size align)
    (when (> align 8) (unsupported! "over-aligned struct or union"))
    (let* ([n (div size align)]
           [all-float? (and (pair? ls) (for-all leaf-float ls))]
           [uniform? (and all-float?
                          (let ([f (leaf-float (car ls))])
                            (and (= f align) (for-all (lambda (l) (= (leaf-float l) f)) ls))))]
           [float-chunk?
             (lambda (i)
               (let* ([lo (* i align)] [hi (+ lo align)]
                      [over (filter (lambda (l) (overlaps? l lo hi)) ls)])
                 (and (memv align '(4 8))
                      (if (string=? (model-arch m) "aarch64")
                        uniform?
                        (and (pair? over) (for-all leaf-float over))))))]
           [kind (lambda (i)
                   (if (float-chunk? i)
                     (if (= align 4) 'ffi:float 'ffi:double)
                     (int-type align #t)))])
      (let loop ([i 0] [acc '()])
        (if (= i n)
          `(s:list ,@(reverse acc))
          (let run ([j (+ i 1)])
            (if (and (< j n) (eq? (kind j) (kind i)))
              (run (+ j 1))
              (loop j (cons (if (= (- j i) 1) (kind i) `(s:vector ,(kind i) ,(- j i))) acc))))))))

  (define (record-descriptor m id)
    (let ([known (hashtable-ref (model-descriptors m) id #f)])
      (if known
        (car known)
        (let* ([lay (record-layout m id)]
               [size (cadr lay)]
               [align (caddr lay)]
               [ls (guard (e [(eq? e 'chunky) '()]) (leaves m `(record ,id) 0))]
               [ordered (list-sort (lambda (a b) (< (leaf-offset a) (leaf-offset b))) ls)])
          (when (zero? size) (unsupported! "empty struct or union"))
          (let ([expr
                  (if (and (pair? ordered) (natural-layout? ordered size align))
                    `(s:list ,@(map leaf-expr ordered))
                    (chunk-descriptor m ordered size align))]
                [sym (string->symbol (string-append "%struct-" (number->string id)))])
            (hashtable-set! (model-descriptors m) id (cons sym expr))
            sym)))))

  (define (memory-kind m ty)
    (let ([t (strip m ty)])
      (case (car t)
        [(int) (let ([u (caddr t)])
                 (case (cadr t)
                   [(1) (if u 'u8 's8)] [(2) (if u 'u16 's16)]
                   [(4) (if u 'u32 's32)] [(8) (if u 'u64 's64)]
                   [else (unsupported! "integer size")]))]
        [(bool) 'u8]
        [(float) (case (cadr t) [(4) 'f32] [(8) 'f64] [else (unsupported! "long double")])]
        [(ptr func nullptr) 'ptr]
        [else #f])))

  ;; Every named member as (NAME TYPE OFFSET BIT), looking through anonymous structs and
  ;; unions. BIT is #f or (BIT-OFFSET WIDTH UNIT-SIZE).
  (define (members m id base)
    (let ([lay (record-layout m id)])
      (append-map
        (lambda (f o)
          (let* ([name (car f)] [ty (cadr f)] [bits (caddr f)]
                 [off (+ base (if bits (car o) o))])
            (cond
              [(and name bits (> bits 0)) (list (list name ty off (cdr o)))]
              ;; a nested struct is reachable as a pointer and field by field (outer-inner)
              [(and name (not bits) (eq? (car (strip m ty)) 'record))
                (cons (list name ty off #f)
                  (map (lambda (inner) (cons (string-append name "-" (car inner)) (cdr inner)))
                    (members m (cadr (strip m ty)) off)))]
              [name (list (list name ty off #f))]
              [(and (not bits) (eq? (car (strip m ty)) 'record))
                (members m (cadr (strip m ty)) off)]
              [else '()])))
        (list-ref (record-ref m id) 5)
        (cadddr lay))))

  (define (symbol-append* . parts)
    (string->symbol (apply string-append (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))

  (define (innermost-element m ty)
    (let ([t (strip m ty)])
      (if (eq? (car t) 'array) (innermost-element m (cadr t)) t)))

  ;; A `char NAME[N]` member also gets NAME-string and NAME-string-set!, which read and
  ;; write it as a string (up to the first NUL).
  (define (char-array-entries m prefix fname ty off)
    (let ([elem (strip m (cadr ty))] [n (caddr ty)])
      (if (and (eq? (car elem) 'int) (eqv? (cadr elem) 1) n)
        (let ([get (symbol-append* prefix "-" fname "-string")]
              [set (symbol-append* prefix "-" fname "-string-set!")])
          (list
            (list (symbol->string get) `(s:define (,get p) (rt:c-memory-string p ,off ,n)))
            (list (symbol->string set) `(s:define (,set p v) (rt:c-memory-string-set! p ,off ,n v)))))
        '())))

  ;; Scalar loads and stores go straight to pointer-ref/pointer-set! 
  (define (kind->ffi-type kind)
    (case kind
      [(u8) 'ffi:uint8] [(s8) 'ffi:int8]
      [(u16) 'ffi:uint16] [(s16) 'ffi:int16]
      [(u32) 'ffi:uint32] [(s32) 'ffi:int32]
      [(u64) 'ffi:uint64] [(s64) 'ffi:int64]
      [(f32) 'ffi:float] [(f64) 'ffi:double]
      [else #f]))

  ;; The `ffi:` primitive for KIND, e.g. `ffi:pointer-ref/s32`.
  (define (typed-pointer-op prefix kind)
    (string->symbol (string-append "ffi:" prefix (symbol->string kind))))

  (define (memory-ref-form kind p offset)
    (let ([type (kind->ffi-type kind)])
      (if type
        `(,(typed-pointer-op "pointer-ref/" kind) ,p ,offset)
        `(rt:c-memory-ref ,p ,offset (s:quote ,kind)))))

  (define (memory-set-form kind p offset v)
    (let ([type (kind->ffi-type kind)])
      (if type
        `(,(typed-pointer-op "pointer-set!/" kind) ,p ,offset ,v)
        `(rt:c-memory-set! ,p ,offset (s:quote ,kind) ,v))))

  ;; (name form) entries for one member: a getter and a setter.
  (define (member-entries m prefix mem)
    (let* ([fname (car mem)] [ty (strip m (cadr mem))] [off (caddr mem)] [bit (cadddr mem)]
           [get (string-append prefix "-" fname)]
           [set (string-append prefix "-" fname "-set!")]
           [getter (string->symbol get)]
           [setter (string->symbol set)])
      (define (entries get-form set-form)
        (list (list get get-form) (list set set-form)))
      (try-lower
        (cond
          [bit
            (let ([width (cadr bit)] [unit (caddr bit)])
              (entries
                `(s:define (,getter p)
                   (rt:c-bit-field-ref p ,off ,unit ,(car bit) ,width ,(not (caddr ty))))
                `(s:define (,setter p v)
                   (rt:c-bit-field-set! p ,off ,unit ,(car bit) ,width v))))]
          [(eq? (car ty) 'record)
            (let ([size (type-size m ty)])
              (entries
                `(s:define (,getter p) (rt:c-pointer+ p ,off))
                `(s:define (,setter p v) (rt:c-memory-copy! p ,off v ,size))))]
          [(eq? (car ty) 'array)
            (let* ([elem (innermost-element m ty)] [es (type-size m elem)])
              (if (eq? (car elem) 'record)
                (entries
                  `(s:define (,getter p i) (rt:c-pointer+ p (s:+ ,off (s:* i ,es))))
                  `(s:define (,setter p i v) (rt:c-memory-copy! p (s:+ ,off (s:* i ,es)) v ,es)))
                (let ([kind (memory-kind m elem)])
                  (append
                    (entries
                      `(s:define (,getter p i) ,(memory-ref-form kind 'p `(s:+ ,off (s:* i ,es))))
                      `(s:define (,setter p i v) ,(memory-set-form kind 'p `(s:+ ,off (s:* i ,es)) 'v)))
                    (char-array-entries m prefix fname ty off)))))]
          [else
            (let ([kind (memory-kind m ty)])
              (unless kind (unsupported! "member type"))
              (entries
                `(s:define (,getter p) ,(memory-ref-form kind 'p off))
                `(s:define (,setter p v) ,(memory-set-form kind 'p off 'v))))])
        (lambda (why) '()))))

  ;; The name a record is exported under: its typedef when it has one, else its tag.
  (define (record-name m r)
    (declared-name m `(record ,(cadr r)) (cadddr r)))

  ;; The name of the typedef for TYPE (a `(record ID)` or `(enum ID)` form), else TAG.
  (define (declared-name m type tag)
    (or (let loop ([ds (cdr (assq 'decls (cdr (model-unit m))))])
          (cond [(null? ds) #f]
                [(and (eq? (caar ds) 'typedef) (equal? (caddr (car ds)) type))
                  (cadr (car ds))]
                [else (loop (cdr ds))]))
        tag))

  ;; (make-NAME value ...) fills the first fields in declaration order. Up to eight values
  ;; get a clause of their own that calls the setters directly; more go through the generic
  ;; rt:c-make-record
  (define (constructor-form ctor size setters)
    (let* ([fast (min (length setters) 8)]
           [names (list-head '(a0 a1 a2 a3 a4 a5 a6 a7) fast)])
      `(s:define ,ctor
         (s:case-lambda
           ,@(map
               (lambda (k)
                 (let ([params (list-head names k)])
                   `(,params
                     (s:let ((p (rt:c-allocate ,size)))
                       ,@(map (lambda (setter param) `(,setter p ,param))
                              (list-head setters k) params)
                       p))))
               (let up ([k 0]) (if (> k fast) '() (cons k (up (+ k 1))))))
           (args (rt:c-make-record ,size (s:list ,@setters) args))))))

  (define (list-head l k)
    (if (zero? k) '() (cons (car l) (list-head (cdr l) (- k 1)))))

  (define (record-entries m r report)
    ;; (record ID KIND TAG COMPLETE FIELDS LAYOUT PACKED LOC)
    (let ([name (record-name m r)])
      (if (or (not name)
              (not (list-ref r 4))
              (not (list-ref r 6))
              (string-prefix? "_" name)
              (not ((model-in-scope? m) (list-ref r 8)))
              (not (wanted? m name)))
        '()
        (try-lower
          (let* ([id (cadr r)]
                 [prefix (exported-name m name)]
                 [lay (list-ref r 6)]
                 [mems (members m id 0)]
                 [accessors (append-map (lambda (mem) (member-entries m prefix mem)) mems)]
                 [setters
                   (if (eq? (caddr r) 'union)
                     '()
                     (filter-map
                       (lambda (f)
                         (and (car f) (not (eq? (car (strip m (cadr f))) 'array))
                              (not (and (caddr f) (zero? (caddr f))))
                              (symbol-append* prefix "-" (car f) "-set!")))
                       (list-ref r 5)))]
                 [ctor (symbol-append* "make-" prefix)])
            (append
              (list
                (list (string-append "sizeof-" prefix)
                  `(s:define ,(symbol-append* "sizeof-" prefix) ,(cadr lay)))
                (list (string-append "alignof-" prefix)
                  `(s:define ,(symbol-append* "alignof-" prefix) ,(caddr lay)))
                (list (symbol->string ctor)
                  (constructor-form ctor (cadr lay) setters)))
              accessors))
          (lambda (why)
            (report 'skipped name why)
            '())))))

  (define (name-listed? name patterns)
    (exists
      (lambda (p)
        (if (string-suffix? "*" p)
          (string-prefix? (substring p 0 (- (string-length p) 1)) name)
          (string=? p name)))
      patterns))

  (define (exported-name m name)
    (let* ([config (model-config m)]
           [renames (config-ref config 'rename '())]
           [renamed (assoc name renames)])
      (if renamed
        (cdr renamed)
        (let* ([stripped
                 (let loop ([ps (config-ref config 'strip-prefix '())])
                   (cond [(null? ps) name]
                         [(and (string-prefix? (car ps) name) (> (string-length name) (string-length (car ps))))
                           (substring name (string-length (car ps)) (string-length name))]
                         [else (loop (cdr ps))]))])
          (string-append (config-ref config 'add-prefix "") stripped)))))

  (define (wanted? m name)
    (let* ([config (model-config m)]
           [include (config-ref config 'include #f)]
           [exclude (config-ref config 'exclude '())])
      (and (not (string-prefix? "__" name))
           (or (not include) (name-listed? name include))
           (not (name-listed? name exclude)))))

  ;; The (func RET PARAMS VARIADIC PROTOTYPED) a pointer-to-function type points at, or #f.
  (define (function-pointee m ty)
    (let ([t (strip m ty)])
      (and (eq? (car t) 'ptr)
           (let ([target (strip m (cadr t))])
             (and (eq? (car target) 'func) target)))))

  (define (char-pointer? m t)
    (and (eq? (car t) 'ptr)
         (let ([target (strip m (cadr t))])
           (and (eq? (car target) 'int) (eqv? (cadr target) 1)))))

  ;; FFI expression for a parameter
  (define (parameter-expr m ty)
    (let ([t (strip m ty)])
      (cond
        [(eq? (car t) 'bool) '(s:quote bool)]
        [(function-pointee m ty) => (lambda (f) (callback-converter-expr m f))]
        [else (lower m ty)])))

  ;; FFI expression for a return type. With #:string-returns, `char *` is `cstring`.
  (define (return-expr m ty)
    (let ([t (strip m ty)])
      (cond
        [(eq? (car t) 'bool) '(s:quote bool)]
        [(and (config-ref (model-config m) 'string-returns #f) (char-pointer? m t))
          '(s:quote cstring)]
        [else (lower m ty)])))

  ;; (rt:c-callback-converter RET (PARAMS ...)) for a (func ...) type.
  (define (callback-converter-expr m f)
    `(rt:c-callback-converter ,(return-expr m (cadr f))
       (s:list ,@(map (lambda (p) (parameter-expr m p)) (caddr f)))))

  ;; What one declaration contributes: a list of `(name definition-form)`, or a skip report.
  (define (function-entries m d report)
    ;; (function NAME RET (PTYPES) (PNAMES) VARIADIC STORAGE INLINE ASM ATTRS LOC [BODY])
    (let* ([name (list-ref d 1)]
           [ret (list-ref d 2)]
           [ptypes (list-ref d 3)]
           [variadic? (list-ref d 5)]
           [storage (list-ref d 6)]
           [inline? (list-ref d 7)]
           [asm (list-ref d 8)]
           [loc (list-ref d 10)]
           [shim (assoc name (model-shims m))])
      (cond
        [(not ((model-in-scope? m) loc)) '()]
        [(not (wanted? m name)) '()]
        [(and shim (eq? (cdr shim) 'skipped)) '()]
        [(and shim (eq? (cdr shim) 'failed))
          (report 'skipped name "its C shim could not be compiled (see the compiler output)")
          '()]
        [(and (eq? storage 'static) (not inline?)) '()]
        [(and (eq? storage 'static) (not shim))
          (report 'skipped name "static inline function bodies are not translated yet")
          '()]
        [else
          (try-lower
            (let* ([rt (return-expr m ret)]
                   [pts (map (lambda (t) (parameter-expr m t)) ptypes)]
                   [exported (exported-name m name)]
                   [cname (if shim
                            (string-append "capy_shim_" name)
                            (symbol-name m (or asm name)))]
                   [blocking? (blocking? m name)])
              (list
                (list exported
                  `(s:define ,(string->symbol exported)
                     (rt:c-function %c-libraries ,cname ,rt (s:list ,@pts) ,variadic? ,blocking? ,(length pts))))))
            (lambda (why)
              (unless (string-prefix? "_" name)
                (report 'skipped name why))
              '()))])))

  (define (symbol-name m label)
    (if (and (string=? (model-os m) "macos") (string-prefix? "_" label))
      (substring label 1 (string-length label))
      label))

  ;; Calls that may block the thread they run on, they will run on blocking pool thread(s). 
  ;; Note that this list is best effort, and not complete. 
  (define known-blocking
    '("read" "write" "pread" "pwrite" "readv" "writev" "recv" "recvfrom" "recvmsg" "send"
      "sendto" "sendmsg" "accept" "accept4" "connect" "select" "pselect" "poll" "ppoll"
      "epoll_wait" "kevent" "sleep" "usleep" "nanosleep" "clock_nanosleep" "pause"
      "wait" "waitpid" "wait3" "wait4" "waitid" "sigwait" "sigsuspend" "flock" "fsync"
      "fdatasync" "getaddrinfo" "gethostbyname" "pthread_join" "pthread_cond_wait"
      "pthread_cond_timedwait" "sem_wait" "sem_timedwait" "fgets" "fread" "fwrite" "getc"
      "fgetc" "getchar" "getline" "getdelim" "system" "popen"))

  (define (blocking? m name)
    (let ([setting (config-ref (model-config m) 'blocking 'auto)])
      (cond [(boolean? setting) setting]
            [(list? setting) (and (name-listed? name setting) #t)]
            [else (and (member name known-blocking) #t)])))

  (define (enum-entries m e)
    ;; (enum ID TAG UNDER COMPLETE? ((NAME VALUE)...) LOC)
    (if (not ((model-in-scope? m) (list-ref e 6)))
      '()
      (filter-map
        (lambda (c)
          (and (wanted? m (car c))
               (let ([n (exported-name m (car c))])
                 (list n `(s:define ,(string->symbol n) ,(cadr c))))))
        (list-ref e 5))))

  (define (typedef-entries m d report)
    ;; (typedef NAME TYPE LOC)
    (let ([name (list-ref d 1)])
      (cond
        [(not ((model-in-scope? m) (list-ref d 3))) '()]
        [(not (wanted? m name)) '()]
        [else
          (try-lower
            (let* ([n (exported-name m name)]
                   [target (list-ref d 2)]
                   [f (function-pointee m target)]
                   [entries (list (list n `(s:define ,(string->symbol n) ,(lower m target))))])
              (if f
                ;; make-NAME turns a procedure into a callback and NAME->procedure turns a
                ;; function pointer into a procedure.
                (append entries
                  (list
                    (value-def (string-append "make-" n) (callback-converter-expr m f))
                    (value-def (string-append n "->procedure")
                      `(rt:c-function-pointer-maker ,(return-expr m (cadr f))
                         (s:list ,@(map (lambda (p) (parameter-expr m p)) (caddr f)))))))
                entries))
            (lambda (why) '()))])))

  ;; (name (s:define name value)) and (name (s:define (name args ...) body)).
  (define (value-def name expr)
    (list name `(s:define ,(string->symbol name) ,expr)))

  (define (proc-def name params body)
    (list name `(s:define (,(string->symbol name) ,@params) ,body)))

  ;; `extern` variables: NAME is a procedure returning the value (for scalars), NAME-set!
  ;; stores it, and NAME-address returns the pointer to it.
  (define (var-entries m d report)
    ;; (var NAME TYPE STORAGE ASM CONST INIT ATTRS LOC)
    (let ([name (list-ref d 1)] [storage (list-ref d 3)])
      (cond
        [(not ((model-in-scope? m) (list-ref d 8))) '()]
        [(not (wanted? m name)) '()]
        [(eq? storage 'static) '()]
        [else
          (try-lower
            (let* ([n (exported-name m name)]
                   [address (string-append n "-address")]
                   [kind (memory-kind m (list-ref d 2))]
                   [cname (symbol-name m (or (list-ref d 4) name))]
                   [entries
                     (list (value-def address
                             `(rt:c-global %c-libraries ,cname)))])
              (if (and kind (not (list-ref d 5)))
                (append entries
                  (list
                    (proc-def n '() `(rt:c-memory-ref (,(string->symbol address)) 0 (s:quote ,kind)))
                    (proc-def (string-append n "-set!") '(v)
                      `(rt:c-memory-set! (,(string->symbol address)) 0 (s:quote ,kind) v))))
                (if kind
                  (append entries
                    (list (proc-def n '() `(rt:c-memory-ref (,(string->symbol address)) 0 (s:quote ,kind)))))
                  entries)))
            (lambda (why)
              (report 'skipped name why)
              '()))])))

  ;; The names of an enum's enumerators and helpers over them:
  ;;   NAME-names               the alist ((enumerator . value) ...)
  ;;   NAME->symbol / symbol->NAME
  ;;   NAME-flags->list         only when every value is zero or a power of two
  (define (enum-helper-entries m e)
    ;; (enum ID TAG UNDER COMPLETE? ((NAME VALUE)...) LOC)
    (let ([name (declared-name m `(enum ,(cadr e)) (caddr e))])
      (if (or (not name)
              (not (list-ref e 4))
              (not ((model-in-scope? m) (list-ref e 6)))
              (not (wanted? m name)))
        '()
        (let* ([prefix (exported-name m name)]
               [names (symbol-append* prefix "-names")]
               [pairs (map (lambda (c) (cons (string->symbol (exported-name m (car c))) (cadr c)))
                        (list-ref e 5))]
               [flags? (and (exists (lambda (p) (not (zero? (cdr p)))) pairs)
                         (for-all
                           (lambda (p)
                             (let ([v (cdr p)])
                               (or (zero? v) (and (> v 0) (zero? (bitwise-and v (- v 1)))))))
                           pairs))])
          (append
            (list (value-def (symbol->string names) `(s:quote ,pairs)))
            (list
              (proc-def (string-append prefix "->symbol") '(n)
                `(rt:c-enum-name ,names n))
              (proc-def (string-append "symbol->" prefix) '(s)
                `(rt:c-enum-value ,names s)))
            (if flags?
              (list (proc-def (string-append prefix "-flags->list") '(n)
                      `(rt:c-enum-flags->list ,names n)))
              '()))))))

  ;; Object-like macros, as classified by (capy c classify).
  (define (macro-entries m mac known? report)
    ;; (macro NAME PARAMS VARIADIC BODY LOC FROM-SYSTEM HINT)
    (case (car mac)
      [(fmacro-skipped) (report 'macro-skipped (cadr mac) (caddr mac)) '()]
      [(macro) (object-macro-entries m mac known? report)]
      [else '()]))

  (define (object-macro-entries m mac known? report)
    (let ([name (list-ref mac 1)]
          [params (list-ref mac 2)]
          [loc (list-ref mac 5)])
      (if (or params
              (string-prefix? "_" name)
              (not ((model-in-scope? m) loc))
              (not (wanted? m name)))
        '()
        (let ([verdict (classify-macro mac (lambda (other) (known? (exported-name m other))))]
              [n (exported-name m name)])
          (case (car verdict)
            [(constant) (list (list n `(s:define ,(string->symbol n) ,(cadr verdict))))]
            [(construct)
              (let ([ctor (string-append "make-" (exported-name m (cadr verdict)))])
                (if (known? ctor)
                  (list (list n `(s:define ,(string->symbol n)
                                   (,(string->symbol ctor) ,@(caddr verdict)))))
                  (begin (report 'macro-skipped name "struct constructor not generated") '())))]
            [(alias) (list (list n `(s:define ,(string->symbol n)
                                      ,(string->symbol (exported-name m (cadr verdict))))))]
            [else
              ;; Most macros in a system header are internal; only a report lists them.
              (report 'macro-skipped name (cadr verdict))
              '()])))))

  ;; The Scheme name of a macro parameter
  (define (param-symbol p) (string->symbol (string-append "%p-" p)))

  ;; The Scheme expression for one `(fmacro ...)` body node
  (define (fm-expr m e params refs)
    (define (sub x) (fm-expr m x params refs))
    (define (name-ref n)
      (let ([exported (exported-name m n)])
        (set-box! refs (cons exported (unbox refs)))
        (string->symbol exported)))
    (case (car e)
      [(lit str) (cadr e)]
      [(param) (param-symbol (cadr e))]
      [(id) (name-ref (cadr e))]
      [(op) (fm-op (cadr e) (map sub (cddr e)))]
      [(if) `(s:if (rt:c-true? ,(sub (cadr e))) ,(sub (caddr e)) ,(sub (cadddr e)))]
      [(cast) (fm-cast m (cadr e) (sub (caddr e)))]
      [(sizeof) (type-size m (cadr e))]
      [(call) `(,(name-ref (cadr e)) ,@(map sub (cddr e)))]
      [else (unsupported! (string-append "node " (symbol->string (car e))))]))

  ;; Operators over the translated operands ARGS
  (define (fm-op op args)
    (let ([a (car args)] [b (and (pair? (cdr args)) (cadr args))])
      (case op
        [(+) (if b `(s:+ ,a ,b) a)]
        [(-) (if b `(s:- ,a ,b) `(s:- ,a))]
        [(neg) `(s:- ,a)]
        [(*) `(s:* ,a ,b)]
        [(/) `(rt:c-div ,a ,b)]
        [(%) `(s:remainder ,a ,b)]
        [(<<) `(s:bitwise-arithmetic-shift-left ,a ,b)]
        [(>>) `(s:bitwise-arithmetic-shift-right ,a ,b)]
        [(&) `(s:bitwise-and ,a ,b)]
        [(bor) `(s:bitwise-ior ,a ,b)]
        [(^) `(s:bitwise-xor ,a ,b)]
        [(~) `(s:bitwise-not ,a)]
        [(&&) `(s:if (rt:c-true? ,a) (s:if (rt:c-true? ,b) 1 0) 0)]
        [(lor) `(s:if (rt:c-true? ,a) 1 (s:if (rt:c-true? ,b) 1 0))]
        [(!) `(s:if (rt:c-true? ,a) 0 1)]
        [(==) `(s:if (s:= ,a ,b) 1 0)]
        [(!=) `(s:if (s:= ,a ,b) 0 1)]
        [(<) `(s:if (s:< ,a ,b) 1 0)]
        [(<=) `(s:if (s:<= ,a ,b) 1 0)]
        [(>) `(s:if (s:> ,a ,b) 1 0)]
        [(>=) `(s:if (s:>= ,a ,b) 1 0)]
        [else (unsupported! (string-append "operator " (symbol->string op)))])))

  (define (fm-cast m ty code)
    (let ([t (strip m ty)])
      (case (car t)
        [(int) `(rt:c-int-cast ,(cadr t) ,(and (caddr t) #t) ,code)]
        [(bool) `(rt:c-bool-value ,code)]
        [(float) `(rt:c-float-cast ,(cadr t) ,code)]
        [else code])))

  (define (fmacro-candidate m mac report)
    ;; (fmacro NAME PARAMS BODY LOC)
    (let ([cname (list-ref mac 1)]
          [params (list-ref mac 2)]
          [loc (list-ref mac 4)])
      (cond
        [(or (string-prefix? "_" cname)
             (not ((model-in-scope? m) loc))
             (not (wanted? m cname)))
          #f]
        [else
          (let ([refs (box '())])
            (try-lower
              (let* ([code (fm-expr m (list-ref mac 3) params refs)]
                     [ename (exported-name m cname)])
                (list cname ename (unbox refs)
                  (list ename `(s:define (,(string->symbol ename) ,@(map param-symbol params)) ,code))))
              (lambda (why)
                (report 'macro-skipped cname why)
                #f)))])))

  (define (fmacro-entries m mac-list known report)
    (let loop ([pending (filter-map (lambda (mac) (fmacro-candidate m mac report)) mac-list)]
               [have known]
               [acc '()])
      (let ([ready (filter (lambda (c) (for-all (lambda (r) (member r have)) (caddr c))) pending)])
        (if (null? ready)
          (begin
            (for-each
              (lambda (c)
                (report 'macro-skipped (car c)
                  (string-append "refers to "
                    (let ([missing (find (lambda (r) (not (member r have))) (caddr c))])
                      (or missing "an unknown name"))
                    ", which is not available")))
              pending)
            (reverse acc))
          (loop (filter (lambda (c) (not (memq c ready))) pending)
            (append (map cadr ready) have)
            (append (reverse (map cadddr ready)) acc))))))

  (define (generate-library unit header config library-name shim-path)
    (let* ([shim-items (filter (lambda (x) (memq (car x) '(shim shim-skipped))) (cdr unit))]
           [shim-names (let ([s (find (lambda (x) (eq? (car x) 'shim)) shim-items)])
                         (if s (caddr s) '()))]
           [shims
             (append
               (map (lambda (n) (cons n (if shim-path #t 'failed))) shim-names)
               (map (lambda (x) (cons (cadr x) 'skipped))
                 (filter (lambda (x) (eq? (car x) 'shim-skipped)) shim-items)))]
           [reports '()]
           [report (lambda (kind name why) (set! reports (cons (list kind name why) reports)))]
           [m (build-model unit header config shims)]
           [decls (section unit 'decls)]
           [enums (section unit 'enums)]
           [macros (section unit 'macros)]
           [declared
             (append
               (append-map
                 (lambda (e) (append (enum-entries m e) (enum-helper-entries m e)))
                 enums)
               (append-map (lambda (r) (record-entries m r report)) (section unit 'records))
               (append-map
                 (lambda (d)
                   (case (car d)
                     [(function) (function-entries m d report)]
                     [(typedef) (typedef-entries m d report)]
                     [(var) (var-entries m d report)]
                     [else '()]))
                 decls))]
           [known? (lambda (n) (and (assoc n declared) #t))]
           [shim-reports (for-each (lambda (x) (when (eq? (car x) 'shim-skipped) (report 'shim-skipped (cadr x) (caddr x))))
                shim-items)]
           [object-entries (append-map (lambda (mac) (macro-entries m mac known? report)) macros)]
           [function-macro-entries
             (fmacro-entries m (filter (lambda (mac) (eq? (car mac) 'fmacro)) macros)
               (map car (append declared object-entries))
               report)]
           [entries (append declared object-entries function-macro-entries)]
           [unique (dedupe entries)]
           [links (config-ref config 'link '())])
      (values
        `(library ,library-name
           (export ,@(map (lambda (e) (string->symbol (car e))) unique))
           (import
             (prefix (rnrs) s:)
             (prefix (core foreign) ffi:)
             (prefix (capy c runtime) rt:))
           (s:define %c-libraries (rt:make-c-libraries (s:quote ,links)))
           ,@(map (lambda (d) `(s:define ,(car d) ,(cdr d)))
               (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                 (map (lambda (k) (hashtable-ref (model-descriptors m) k #f))
                   (vector->list (hashtable-keys (model-descriptors m))))))
           ,@(map cadr unique))
        (reverse reports))))

  ;; First definition of a name wins: enumerators and functions are generated before macros.
  (define (dedupe entries)
    (let ([seen (make-hashtable string-hash string=?)])
      (filter-map
        (lambda (e)
          (and (not (hashtable-ref seen (car e) #f))
               (begin (hashtable-set! seen (car e) #t) e)))
        entries)))

  (define (append-map f . lists)
    (apply append (apply map f lists))))
