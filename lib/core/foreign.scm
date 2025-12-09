(library (core foreign)
    (export 
        string->utf8/nul
        void 
        float
        float double
        complex-float complex-double
        short
        unsigned-short
        int unsigned-int long unsigned-long size_t ssize_t ptrdiff_t
        int8 uint8
        uint16 int16
        uint32 int32
        uint64 int64
        intptr_t uintptr_t

        sizeof alignof

        %null-pointer
        null-pointer?
        pointer?
        make-pointer
        pointer->scm
        scm->pointer
        pointer-address
        pointer->bytevector
        bytevector->pointer
        set-pointer-finalizer!

        dereference-pointer
        string->pointer
        pointer->string

        pointer->procedure

        read-c-struct write-c-struct
        make-c-struct parse-c-struct

        define-c-struct
        struct-size
        struct-alignment
        errno
        ioctl/pointer
        dll-suffix)
    (import (core control) (core primitives) (core match) (capy))
  (eval-when (expand load eval)
    (define void %void)
    (define float %float)
    (define double %double)
    (define complex-float %complex-float)
    (define complex-double %complex-double)
    (define short %short)
    (define unsigned-short %unsigned-short)
    (define int %int)
    (define unsigned-int %unsigned-int)
    (define long %long)
    (define unsigned-long %unsigned-long)
    (define size_t %size_t)
    (define ssize_t %ssize_t)
    (define ptrdiff_t %ptrdiff_t)
    (define int8 %int8)
    (define uint8 %uint8)
    (define int16 %int16)
    (define uint16 %uint16)
    (define int32 %int32)
    (define uint32 %uint32)
    (define int64 %int64)
    (define uint64 %uint64)
    (define intptr_t %intptr_t)
    (define uintptr_t %uintptr_t))
    

    (define (string->pointer x)
      "Converts a Scheme string to a C-style null-terminated string"
      (unless (string? x)
        (assertion-violation 'string->pointer "not a string" x))
      (define bv (make-bytevector/nonmoving (+ (string-length x) 1) 0))
      (bytevector-copy! (string->utf8 x) 0 bv 0 (string-length x))
      ;; bytevector should be alive as long as pointer is alive
      (bytevector->pointer bv))
    
    ;; pointer->string is implemented in Rust to make our life simpler.

    (define (null-pointer? x)
        (= (pointer-address x) 0))

    (define-syntax-rule (align off alignment)
        (+ 1 (logior (- off 1) (- alignment 1))))

    (define bytevector-pointer-ref
        (case (sizeof '*)
            [(4) (lambda (bf off)
                (make-pointer (bytevector-u32-ref bf off) '*))]
            [(8) (lambda (bf off)
                (make-pointer (bytevector-u64-ref bf off) '*))]))

    (define bytevector-pointer-set!
        (case (sizeof '*)
            [(4) (lambda (bf off p)
                (bytevector-u32-set! bf off (pointer-address p)))]
            [(8) (lambda (bf off p)
                (bytevector-u64-set! bf off (pointer-address p)))]))

    (define-syntax-rule (define-complex-accessors (read write) (%read %write size))
        (begin
            (define (read bv offset)
                (make-rectangular
                    (%read bv offset)
                    (%read bv (+ offset size))))
            (define (write bv offset val)
                (%write bv offset (real-part val))
                (%write bv (+ offset size) (imag-part val)))))


    (define-complex-accessors
        (bytevector-complex-single-native-ref bytevector-complex-single-native-set!)
        (bytevector-ieee-single-native-ref bytevector-ieee-single-native-set! 4))

    (define-complex-accessors
        (bytevector-complex-double-native-ref bytevector-complex-double-native-set!)
        (bytevector-ieee-double-native-ref bytevector-ieee-double-native-set! 8))
    (define-syntax cte 
        (lambda (x)
            (syntax-case x ()
                [(_ exp)
                    #`(quote #,(datum->syntax #'here (eval (syntax->datum #'exp))))])))

    (define-syntax switch/cte 
        (syntax-rules (else)
            [(_ x (k expr) ... (else alt))
                (let ([t x])
                    (cond 
                        [(eq? t (cte k)) expr]
                        ...
                        [else alt]))]))
    
    (define-syntax-rule (read-field %bv %offset %type)
        (let ((bv %bv)
              (offset %offset)
              (type %type))
            (define-syntax-rule (%read type reader)
                (let* ((offset (align offset (cte (alignof type))))
                       (val (reader bv offset)))
                    (values val
                        (+ offset (cte (sizeof type))))))
            (define-syntax-rule (dispatch-read type (%%type reader) (... ...))
                (switch/cte
                    type
                    (%%type (%read %%type reader))
                    (... ...)
                    (else
                        (let ((offset (align offset (alignof type))))
                            (values (%read-c-struct bv offset type)
                                    (+ offset (sizeof type)))))))
            (dispatch-read
                type
                (int8 bytevector-s8-ref)
                (uint8 bytevector-u8-ref)
                (int16 bytevector-s16-native-ref)
                (uint16 bytevector-u16-native-ref)
                (int32 bytevector-s32-native-ref)
                (uint32 bytevector-u32-native-ref)
                (int64 bytevector-s64-native-ref)
                (uint64 bytevector-u64-native-ref)
                (float bytevector-ieee-single-native-ref)
                (double bytevector-ieee-double-native-ref)
                (complex-float bytevector-complex-single-native-ref)
                (complex-double bytevector-complex-double-native-ref)
                ('* bytevector-pointer-ref))))  
    (define-syntax-rule (read-c-struct %bv %offset ((field type) ...) k)
      (let ((bv %bv)
            (offset %offset)
            (size (cte (sizeof (list type ...)))))
        (unless (>= (bytevector-length bv) (+ offset size))
          (error 'read-c-struct "destination bytevector too small"))
        (let*-values (((field offset)
            (read-field bv offset (cte type)))
                  ...)
            (k field ...))))

    (define-syntax-rule (write-field %bv %offset %type %value)
      (let ((bv %bv)
            (offset %offset)
            (type %type)
            (value %value))
        (define-syntax-rule (%write type writer)
          (let ((offset (align offset (cte (alignof type)))))
            (writer bv offset value)
            (+ offset (cte (sizeof type)))))
        (define-syntax-rule (dispatch-write type (%%type writer) (... ...))
          (switch/cte
          type
          (%%type (%write %%type writer))
          (... ...)
          (else
            (let ((offset (align offset (alignof type))))
              (%write-c-struct bv offset type value)
              (+ offset (sizeof type))))))
        (dispatch-write
          type
          (int8 bytevector-s8-set!)
          (uint8 bytevector-u8-set!)
          (int16 bytevector-s16-native-set!)
          (uint16 bytevector-u16-native-set!)
          (int32 bytevector-s32-native-set!)
          (uint32 bytevector-u32-native-set!)
          (int64 bytevector-s64-native-set!)
          (uint64 bytevector-u64-native-set!)
          (float bytevector-ieee-single-native-set!)
          (double bytevector-ieee-double-native-set!)
          (complex-float bytevector-complex-single-native-set!)
          (complex-double bytevector-complex-double-native-set!)
          ('* bytevector-pointer-set!))))

    (define-syntax-rule (write-c-struct %bv %offset ((field type) ...))
      (let ((bv %bv)
            (offset %offset)
            (size (cte (sizeof (list type ...)))))
        (unless (>= (bytevector-length bv) (+ offset size))
          (error 'write-c-struct "destination bytevector too small"))
        (let* ((offset (write-field bv offset (cte type) field))
              ...)
          (values (unspecified)))))


    (define (%write-c-struct bv offset types vals)
      (let lp ((offset offset) (types types) (vals vals))
        (match types
          (() (match vals
                (() #t)
                (_ (error "too many values" vals))))
          (`(array ,type ,count)
            (let loop ([i 0]
                       [value vals]
                       [o offset])
                (if (= i count)
                    #t
                    (match value
                        [(head . tail)
                            (write-field bv o type head)
                            (loop (+ i 1) tail (+ o (sizeof type)))]))))
          ((`(array ,type ,count) . types)
            (let loop ([i 0]
                       [value vals]
                       [o offset])
                (if (= i count)
                    (lp o types value)
                    (match value
                        [(head . tail)
                            (write-field bv o type head)
                            (loop (+ i 1) tail (+ o (sizeof type)))]))))
          ((type . types)
            (match vals
              ((val . vals)
                (lp (write-field bv offset type val) types vals))
              (() (error 'write-c-struct "too few values" vals)))))))

    (define (%read-c-struct bv offset types)
      (let lp ((offset offset) (types types))
        (match types
          (() '())
          (`(array ,type ,count)
            (let loop ([i 0]
                       [vals '()]
                       [o offset])
                (if (= i count)
                    (reverse vals)
                    (let-values (((val no) (read-field bv o type)))
                        (loop (+ i 1) (cons val vals) no)))))
          
          ((type . types)
            (call-with-values (lambda () (read-field bv offset type))
              (lambda (val offset)
                (cons val (lp offset types))))))))

    (define (make-c-struct types vals)
      (let ((bv (make-bytevector (sizeof types) 0)))
        (%write-c-struct bv 0 types vals)
        (bytevector->pointer bv)))

    (define (parse-c-struct foreign types)
      (%read-c-struct (pointer->bytevector foreign (sizeof types)) 0 types))

    (define-syntax struct-alignment 
        (syntax-rules () 
            [(_ offset types ...)
                (max (align offset (alignof types)) ...)]))
    (define-syntax struct-size 
        (syntax-rules () 
            [(_ offset (processed ...))
                (+ 1 (logior (- offset 1) (- (struct-alignment offset processed ...) 1)))]
            [(_ offset (processed ...) type0 types ...)
                (struct-size (+ (sizeof type0) (align offset (alignof type0)))
                             (type0 processed ...)
                             types ...)]))

        

    (define-syntax define-c-struct-macro 
        (syntax-rules ()
            ((_ name ((fields types) ...))
     (define-c-struct-macro name
       (fields ...) 0 ()
       ((fields types) ...)))
    ((_ name (fields ...) offset (clauses ...) ((field type) rest ...))
     (define-c-struct-macro name
       (fields ...)
       (+ (align offset (alignof type)) (cte (sizeof type)))
       (clauses ... ((_ field-offset field) (align offset (alignof type))))
       (rest ...)))
    ((_ name (fields ...) offset (clauses ...) ())
     (define-syntax name
       (syntax-rules (field-offset fields ...)
         clauses ...)))))

    (define-syntax define-c-struct 
        (lambda (x)
            (syntax-case x () 
                [(_ name size wrap-fields read write! (fields types) ...)
                    (identifier? #'name)
                    #'(begin 
                        (define-c-struct-macro name ((fields types) ...))
                        (define size (struct-size 0 () types ...))
                        (define (write! bv offset fields ...)
                            (write-c-struct bv offset ((fields types) ...)))
                        (define (read bv . offset)
                            (define off (if (null? offset) 0 (car offset)))
                            (read-c-struct bv off ((fields types) ...) wrap-fields)))]))))
