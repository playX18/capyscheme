(library (core foreign)
    (export 
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

        define-wrapped-pointer-type)
    (import (capy))
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
        (unless (<= (bytevector-length bv) (+ offset size))
          (error "destination bytevector too small"))
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
        (unless (<= (bytevector-length bv) (+ offset size))
          (error "destination bytevector too small"))
        (let* ((offset (write-field bv offset (cte type) field))
              ...)
          (values))))


    (define (%write-c-struct bv offset types vals)
      (let lp ((offset offset) (types types) (vals vals))
        (match types
          (() (match vals
                (() #t)
                (x (error "too many values" vals))))
          ((type . types)
          (match vals
            ((val . vals)
              
              (lp (write-field bv offset type val) types vals))
            (() (error "too few values" vals)))))))

    (define (%read-c-struct bv offset types)
      (let lp ((offset offset) (types types))
        (match types
          (() '())
          ((type . types)
          (call-with-values (lambda () (read-field bv offset type))
            (lambda (val offset)
              (cons val (lp offset types))))))))

    (define (make-c-struct types vals)
      (let ((bv (make-bytevector (sizeof types) 0)))
        (%write-c-struct bv 0 types vals)
        (bytevector->pointer bv)))

    (define (parse-c-struct foreign types)
      (%read-c-struct (pointer->bytevector foreign (sizeof types)) 0 types)))
