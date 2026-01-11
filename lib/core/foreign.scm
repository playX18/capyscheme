(library (core foreign)
  (export
    string->utf8/nul
    void
    float
    float
    double
    complex-float
    complex-double
    short
    unsigned-short
    int
    unsigned-int
    long
    unsigned-long
    size_t
    ssize_t
    ptrdiff_t
    int8
    uint8
    uint16
    int16
    uint32
    int32
    uint64
    int64
    intptr_t
    uintptr_t

    sizeof
    alignof

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

    read-c-struct
    write-c-struct
    make-c-struct
    parse-c-struct

    define-c-struct
    struct-size
    struct-alignment
    errno
    ioctl/pointer
    dll-suffix)
  (import
    (core control)
    (core bytevectors)
    (core primitives)
    (core match)
    (capy)
    (srfi 1))
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

  ;;;
  ;;; Packed structures.
  ;;;
  (define-syntax sizeof*
    (syntax-rules (int128 array)
      ((_ int128)
        16)
      ((_ (array type n))
        (* (sizeof* type) n))
      ((_ type)
        (let-syntax ((v (lambda (s)
                         (syntax-case s ()
                           (_
                             (sizeof type))))))
          v))))

  (define-syntax alignof*
    (syntax-rules (int128 array)
      ((_ int128)
        16)
      ((_ (array type n))
        (alignof* type))
      ((_ type)
        (let-syntax ((v (lambda (s)
                         (syntax-case s ()
                           (_
                             (alignof type))))))
          v))))

  (define-syntax align ;as found in (system foreign)
    (syntax-rules (~)
      "Add to OFFSET whatever it takes to get proper alignment for TYPE."
      ((_ offset (type ~ endianness))
        (align offset type))
      ((_ offset type)
        (+ 1 (logior (- offset 1) (- (alignof* type) 1))))))

  (define-syntax type-size
    (syntax-rules (~)
      ((_ (type ~ order))
        (sizeof* type))
      ((_ type)
        (sizeof* type))))

  (define-syntax struct-alignment
    (syntax-rules ()
      "Compute the alignment for the aggregate made of TYPES at OFFSET.  The
result is the alignment of the \"most strictly aligned component\"."
      ((_ offset types ...)
        (max (align offset types) ...))))

  (define-syntax struct-size
    (syntax-rules ()
      "Return the size in bytes of the structure made of TYPES."
      ((_ offset (types-processed ...))
        ;; The SysV ABI P.S. says: "Aggregates (structures and arrays) and unions
        ;; assume the alignment of their most strictly aligned component."  As an
        ;; example, a struct such as "int32, int16" has size 8, not 6.
        (+ 1 (logior (- offset 1)
              (- (struct-alignment offset types-processed ...) 1))))
      ((_ offset (types-processed ...) type0 types ...)
        (struct-size (+ (type-size type0) (align offset type0))
          (type0 types-processed ...)
          types
          ...))))

  (define-syntax write-type
    (syntax-rules (~ array *)
      ((_ bv offset (type ~ order) value)
        (bytevector-uint-set! bv offset value
          (endianness order)
          (sizeof* type)))
      ((_ bv offset (array type n) value)
        (let loop ((i 0)
                   (value value)
                   (o offset))
          (unless (= i n)
            (match value
              ((head . tail)
                (write-type bv o type head)
                (loop (+ 1 i) tail (+ o (sizeof* type))))))))
      ((_ bv offset '* value)
        (bytevector-uint-set! bv offset (pointer-address value)
          (native-endianness)
          (sizeof* '*)))
      ((_ bv offset type value)
        (bytevector-uint-set! bv offset value
          (native-endianness)
          (sizeof* type)))))

  (define-syntax write-types
    (syntax-rules ()
      ((_ bv offset () ())
        #t)
      ((_ bv offset (type0 types ...) (field0 fields ...))
        (begin
          (write-type bv (align offset type0) type0 field0)
          (write-types bv
            (+ (align offset type0) (type-size type0))
            (types ...)
            (fields ...))))))

  (define-syntax read-type
    (syntax-rules (~ array quote *)
      ((_ bv offset '*)
        (make-pointer (bytevector-uint-ref bv offset
                       (native-endianness)
                       (sizeof* '*))))
      ((_ bv offset (type ~ order))
        (bytevector-uint-ref bv offset
          (endianness order)
          (sizeof* type)))
      ((_ bv offset (array type n))
        (unfold
          (lambda (i) (= i n))
          (lambda (i)
            (read-type bv (+ offset (* i (sizeof* type))) type))
          (lambda (x) (+ 1 x))
          0))
      ((_ bv offset type)
        (bytevector-uint-ref bv offset
          (native-endianness)
          (sizeof* type)))))

  (define-syntax read-types
    (syntax-rules ()
      ((_ return bv offset () (values ...))
        (return values ...))
      ((_ return bv offset (type0 types ...) (values ...))
        (read-types return
          bv
          (+ (align offset type0) (type-size type0))
          (types ...)
          (values ... (read-type bv
                       (align offset type0)
                       type0))))))

  (define-syntax define-c-struct-macro
    (syntax-rules ()
      "Define NAME as a macro that can be queried to get information about the C
struct it represents.  In particular:

  (NAME field-offset FIELD)

returns the offset in bytes of FIELD within the C struct represented by NAME."
      ((_ name ((fields types) ...))
        (define-c-struct-macro name
          (fields ...)
          0
          ()
          ((fields types) ...)))
      ((_ name (fields ...) offset (clauses ...) ((field type) rest ...))
        (define-c-struct-macro name
          (fields ...)
          (+ (align offset type) (type-size type))
          (clauses ... ((_ field-offset field) (align offset type)))
          (rest ...)))
      ((_ name (fields ...) offset (clauses ...) ())
        (define-syntax name
          (syntax-rules (field-offset fields ...)
            clauses
            ...)))))

  (define-syntax define-c-struct
    (syntax-rules ()
      "Define SIZE as the size in bytes of the C structure made of FIELDS.  READ
as a deserializer and WRITE! as a serializer for the C structure with the
given TYPES.  READ uses WRAP-FIELDS to return its value."
      ((_ name size wrap-fields read write! (fields types) ...)
        (begin
          (define-c-struct-macro name
            ((fields types) ...))
          (define size
            (struct-size 0 () types ...))
          (define (write! bv offset fields ...)
            (write-types bv offset (types ...) (fields ...)))
          (define* (read bv #:optional (offset 0))
            (read-types wrap-fields bv offset (types ...) ()))))))

  (define-syntax-rule (c-struct-field-offset type field)
    "Return the offset in BYTES of FIELD within TYPE, where TYPE is a C struct
defined with 'define-c-struct' and FIELD is a field identifier.  An
expansion-time error is raised if FIELD does not exist in TYPE."
    (type field-offset field)))
