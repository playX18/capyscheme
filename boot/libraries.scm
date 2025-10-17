

(library (core intrinsics)
   (export               match
                         library define-library define define-syntax
                         quote lambda if set!
                         cond case and or
                         let let* letrec letrec* let-values let*-values
                         begin
                         quasiquote unquote unquote-splicing
                         let-syntax letrec-syntax syntax-rules
                         identifier-syntax
                         assert
                         else => ... _
                         eq?
                         eqv?
                         equal?
                         procedure?
                         number? complex? real? rational? integer?
                         real-valued? rational-valued? integer-valued?
                         exact? inexact?
                         inexact exact
                         = < > <= >=
                         zero? positive? negative? odd? even?
                         finite? infinite? nan?
                         max min + * - / abs
                         div-and-mod div mod div0-and-mod0 div0 mod0
                         gcd lcm numerator denominator
                         floor ceiling truncate round
                         rationalize
                         exp log sin cos tan asin acos atan
                         sqrt
                         exact-integer-sqrt
                         expt
                         make-rectangular make-polar real-part imag-part
                         magnitude angle
                         number->string string->number
                         not boolean? boolean=?
                         pair? cons car cdr
                         caar cadr cdar cddr caaar caadr cadar
                         caddr cdaar cdadr cddar cdddr caaaar caaadr
                         caadar caaddr cadaar cadadr caddar cadddr cdaaar
                         cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                         null? list? list length append reverse list-tail
                         list-ref map for-each
                         symbol? symbol=? symbol->string string->symbol
                         char? char->integer integer->char
                         char=? char<? char>? char<=? char>=?
                         string? make-string string string-length string-ref
                         string=? string<? string>? string<=? string>=?
                         substring string-append string->list list->string string-copy string-for-each
                         vector? make-vector vector vector-length vector-ref vector-set!
                         vector->list list->vector vector-fill!
                         vector-map vector-for-each
                         error assertion-violation
                         apply call-with-current-continuation call/cc
                         values call-with-values dynamic-wind)
    (import (capy)))

(library (core primitives)
    (export 
        when unless
        logior logand logxor lognot
        current-jiffy jiffies-per-second current-second
        library define-library define define-syntax
                         quote lambda if set!
                         cond case and or
                         let let* letrec letrec* let-values let*-values
                         begin
                         quasiquote unquote unquote-splicing
                         let-syntax letrec-syntax syntax-rules
                         identifier-syntax
                         assert
                         else => ... _
                         eq?
                         eqv?
                         equal?
                         procedure?
                         number? complex? real? rational? integer?
                         real-valued? rational-valued? integer-valued?
                         exact? inexact?
                         inexact exact
                         = < > <= >=
                         zero? positive? negative? odd? even?
                         finite? infinite? nan?
                         max min + * - / abs
                         div-and-mod div mod div0-and-mod0 div0 mod0
                         gcd lcm numerator denominator
                         floor ceiling truncate round
                         rationalize
                         exp log sin cos tan asin acos atan
                         sqrt
                         exact-integer-sqrt
                         expt
                         make-rectangular make-polar real-part imag-part
                         magnitude angle
                         number->string string->number
                         not boolean? boolean=?
                         pair? cons car cdr
                         caar cadr cdar cddr caaar caadr cadar
                         caddr cdaar cdadr cddar cdddr caaaar caaadr
                         caadar caaddr cadaar cadadr caddar cadddr cdaaar
                         cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                         null? list? list length append reverse list-tail
                         list-ref map for-each
                         symbol? symbol=? symbol->string string->symbol
                         char? char->integer integer->char
                         char=? char<? char>? char<=? char>=?
                         string? make-string string string-length string-ref
                         string=? string<? string>? string<=? string>=?
                         substring string-append string->list list->string string-copy string-for-each
                         vector? make-vector vector vector-length vector-ref vector-set!
                         vector->list list->vector vector-fill!
                         vector-map vector-for-each
                         error assertion-violation
                         apply call-with-current-continuation call/cc
                         values call-with-values dynamic-wind
    do
                         syntax
                         syntax-case
                         include
                         include-ci
                         ; eval
                        environment
                        eval
                        ; arith flonum
                        flonum?
                        real->flonum
                        fl=?
                        fl<?
                        fl>?
                        fl<=?
                        fl>=?
                        flinteger?
                        flzero?
                        flpositive?
                        flnegative?
                        flodd?
                        fleven?
                        flfinite?
                        flinfinite?
                        flnan?
                        flmax
                        flmin
                        fl+
                        fl*
                        fl-
                        fl/
                        fldiv
                        fldiv0
                        flnumerator
                        fldenominator
                        flfloor
                        flceiling
                        fltruncate
                        flround
                        flexp
                        flexpt
                        fllog
                        flsin
                        flcos
                        fltan
                        flasin
                        flacos
                        flatan
                        flabs
                        flsqrt
                        fixnum->flonum
                        ; arith fixnum
                        fixnum?
                        fixnum-width
                        least-fixnum
                        greatest-fixnum
                        fx=?
                        fx<?
                        fx>?
                        fx<=?
                        fx>=?
                        fxzero?
                        fxpositive?
                        fxnegative?
                        fxodd?
                        fxeven?
                        fxmax
                        fxmin
                        fx+
                        fx*
                        fx-
                        fxdiv
                        fxdiv0
                        fxnot
                        fxand
                        fxior
                        fxxor
                        fxif
                        fxbit-count
                        fxlength
                        fxfirst-bit-set
                        fxbit-set?
                        fxcopy-bit
                        fxarithmetic-shift
                        fxarithmetic-shift-left
                        fxarithmetic-shift-right
                        fxbit-field
                        fxcopy-bit-field
                        &no-infinities make-no-infinities-violation no-infinities-violation?
                        &no-nans make-no-nans-violation no-nans-violation?
                        ; arith bitwise
                        bitwise-not
                        bitwise-and
                        bitwise-ior
                        bitwise-xor
                        bitwise-arithmetic-shift
                        bitwise-first-bit-set
                        bitwise-length
                        bitwise-bit-count
                        ; r6rs syntax-case
                        make-variable-transformer
                        identifier? bound-identifier=? free-identifier=?
                        datum->syntax syntax->datum
                        generate-temporaries
                        syntax-violation
                        ; r6rs lists
                        memq memv member
                        assq assv assoc
                        cons*
                        list-head
                        ; r6rs exceptions
                        raise raise-continuable with-exception-handler
                        ; r6rs records
                        record?
                        record-rtd
                        record-type-name
                        record-type-parent
                        record-type-uid
                        record-type-generative?
                        record-type-sealed?
                        record-type-opaque?
                        record-type-field-names
                        record-field-mutable?
                        make-record-type-descriptor
                        record-type-descriptor?
                        make-record-constructor-descriptor
                        record-constructor
                        record-predicate
                        record-accessor
                        record-mutator
                        make-record-type
                        record-type?
                        record-type-rtd
                        record-type-rcd
                        ; r6rs conditions
                        condition
                        simple-conditions
                        condition?
                        condition-predicate
                        condition-accessor
                        &condition
                        &message make-message-condition message-condition? condition-message
                        &warning make-warning warning?
                        &serious make-serious-condition serious-condition?
                        &error make-error error?
                        &violation make-violation violation?
                        &assertion make-assertion-violation assertion-violation?
                        &irritants make-irritants-condition irritants-condition? condition-irritants
                        &who make-who-condition who-condition? condition-who
                        &non-continuable make-non-continuable-violation non-continuable-violation?
                        &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
                        &lexical make-lexical-violation lexical-violation?
                        &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
                        &undefined make-undefined-violation undefined-violation?
                        ; r6rs mutable-pairs
                        set-car!
                        set-cdr!
                        ; r6rs mutable-strings
                        string-set!
                        string-fill!
                        ; r6rs r5rs
                        quotient
                        remainder
                        modulo
                        ; r6rs unicode
                        char-whitespace?
                        ; r6rs i/o simple
                        display
                        write
                        newline
                        read-char
                        write-char
                        ; r6rs i/o ports
                        call-with-port
                        eof-object eof-object?
                        standard-input-port standard-output-port standard-error-port
                        current-input-port current-output-port current-error-port
                        input-port? output-port? port?
                        flush-output-port
                        output-port-buffer-mode
                        close-port
                        native-transcoder-descriptor
                        port-transcoder-descriptor
                        extract-accumulated-bytevector
                        get-accumulated-bytevector
                        extract-accumulated-string
                        get-accumulated-string
                        open-port
                        nonblock-byte-ready?
                        lookahead-char
                        get-char
                        port-has-port-position?
                        port-position
                        port-has-set-port-position!?
                        set-port-position!
                        port-eof?
                        get-u8
                        lookahead-u8
                        get-bytevector-n
                        get-bytevector-n!
                        get-bytevector-all
                        get-bytevector-some
                        get-string-n
                        get-string-n!
                        get-string-all
                        get-line
                        get-datum
                        put-u8
                        put-bytevector
                        put-char
                        put-string
                        put-datum
                        &i/o make-i/o-error i/o-error?
                        &i/o-read make-i/o-read-error i/o-read-error?
                        &i/o-write make-i/o-write-error i/o-write-error?
                        &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
                        &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
                        &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
                        &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
                        &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
                        &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
                        &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
                        &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
                        &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
                        ; r6rs files
                        file-exists?
                        delete-file
                        ; r6rs hashtables
                        string-hash
                        symbol-hash
                        equal-hash
                        ; r6rs programs
                        command-line
                        exit
                        ; r6rs bytevector
                        native-endianness
                        bytevector?
                        make-bytevector
                        bytevector-length
                        bytevector=?
                        bytevector-fill!
                        bytevector-copy!
                        bytevector-copy
                        bytevector-u8-ref
                        bytevector-s8-ref
                        bytevector-u8-set!
                        bytevector-s8-set!
                        bytevector->u8-list
                        u8-list->bytevector
                        bytevector-u16-ref bytevector-s16-ref bytevector-u16-native-ref bytevector-s16-native-ref
                        bytevector-u16-set! bytevector-s16-set! bytevector-u16-native-set! bytevector-s16-native-set!
                        bytevector-u32-ref bytevector-s32-ref bytevector-u32-native-ref bytevector-s32-native-ref
                        bytevector-u32-set! bytevector-s32-set! bytevector-u32-native-set! bytevector-s32-native-set!
                        bytevector-u64-ref bytevector-s64-ref bytevector-u64-native-ref bytevector-s64-native-ref
                        bytevector-u64-set! bytevector-s64-set! bytevector-u64-native-set! bytevector-s64-native-set!
                        bytevector-ieee-single-ref bytevector-ieee-single-native-ref
                        bytevector-ieee-single-set! bytevector-ieee-single-native-set!
                        bytevector-ieee-double-ref bytevector-ieee-double-native-ref
                        bytevector-ieee-double-set! bytevector-ieee-double-native-set!
                        string->utf8
                        utf8->string
                        ; extensions
                        put-byte
                        put-fasl
                        make-string-output-port
                        make-string-input-port
                        make-transcoded-port
                        make-temporary-file-port
                        port-device-subtype
                        core-eval
                        command-line-shift
                        unspecified
                        unspecified?
                        generate-temporary-symbol
                        circular-list?
                        cyclic-object?
                        list-transpose
                        list-transpose+
                        list-transpose*
                        list-copy
                        vector-copy
                        make-parameter
                        gensym
                        format
                        pretty-print
                        pretty-print-line-length
                        pretty-print-initial-indent
                        pretty-print-maximum-lines
                        pretty-print-unwrap-syntax
                        peek-char
                        read
                        read-with-shared-structure
                        write-with-shared-structure
                        tuple tuple? make-tuple tuple-ref tuple-set! tuple-size tuple-index tuple->list
                        make-weak-mapping weak-mapping? weak-mapping-key weak-mapping-value
                        make-core-hashtable core-hashtable?
                        make-weak-core-hashtable weak-core-hashtable?
                        core-hashtable-contains?
                        core-hashtable-ref core-hashtable-set! core-hashtable-delete! core-hashtable-clear!
                        core-hashtable->alist core-hashtable-size
                        core-hashtable-copy
                        core-hashtable-mutable?
                        core-hashtable-equivalence-function
                        core-hashtable-hash-function
                        usleep
                        macroexpand compile-tree-il
                        current-environment current-macro-environment current-variable-environment current-dynamic-environment
                        system-environment interaction-environment

                        variable-ref
                        variable-set!
                        make-variable 
                        variable-bound?

                        current-module
                        make-module
                        module?
                        module-obarray
                        module-uses
                        module-binder
                        module-declarative?
                        module-transformer
                        raw-module-name
                        module-version
                        module-kind
                        module-import-obarray
                        module-submodules
                        module-filename
                        module-public-interface
                        module-next-unique-id
                        module-replacements
                        module-inlinable-exports
                        set-module-obarray!
                        set-module-uses!
                        set-module-declarative!
                        set-module-name!
                        set-module-version!
                        set-module-kind!
                        set-module-filename!
                        set-module-public-interface!
                        set-module-next-unique-id!
                        set-module-replacements!
                        set-module-inlinable-exports!
                        module-local-variable
                        module-locally-bound?
                        module-bound?
                        module-variable
                        module-symbol-binding
                        module-symbol-local-binding
                        module-make-local-var!
                        module-ensure-local-variable!
                        module-add!
                        module-remove!
                        module-clear!
                        module-gensym
                        module-generate-unique-id
                        module-environment
                        set-module-environment!
                        module-map
                        module-ref-submodule
                        module-define-submodule!
                        save-module-excursion
                        module-ref
                        module-set!
                        module-defined?
                        module-use!
                        module-use-interfaces!
                        module-define!
                        nested-ref
                        nested-set!
                        nested-remove!
                        nested-ref-module
                        nested-define-module!
                        local-ref
                        local-set!
                        local-define
                        local-remove
                        local-ref-module
                        local-define-module
                        module-name
                        make-modules-in
                        beautify-user-module!
                        make-fresh-user-module
                        resolve-module
                        resolve-interface
                        define-module*
                        module-export!
                        module-replace!
                        module-export-all!
                        process-use-modules
                        lookup-bound
                        module-re-export!
                        current-exception-printer

                        procedure-properties
                        set-procedure-properties!
                        procedure-property
                        procedure-name
                        procedure-documentation
                        procedure-sourcev

                        core-read
                        current-source-comments
                        current-after-expansion-hook
                        string-contains symbol-contains subr?
                        make-bytevector-mapping bytevector-mapping?
                        bytevector->pinned-c-void*
                        scheme-library-exports
                        scheme-library-paths
                        scheme-load-paths
                        scheme-load-verbose
                        add-load-path
                        add-library-path
                        library-extensions
                        auto-compile-verbose
                        auto-compile-cache
                        directory-list
                        current-directory
                        create-directory
                        home-directory
                        decode-flonum
                        load
                        system-share-path
                        system-extension-path
                        lookup-process-environment
                        process-environment->alist
                        set-current-input-port! set-current-output-port! set-current-error-port!
                        open-builtin-data-input-port
                        current-library-infix
                        current-library-suffix
                        current-primitive-prefix
                        current-rename-delimiter
                        string->uninterned-symbol
                        uninterned-symbol?
                        uninterned-symbol-prefix
                        uninterned-symbol-suffix

                        ; socket
                        socket?
                        make-socket socket-shutdown socket-close socket->port socket-port
                        socket-send socket-recv socket-accept
                        shutdown-output-port
                        port-closed?

                        ; ffi
                        c-main-argc
                        c-main-argv
                        load-shared-object
                        lookup-shared-object
                        bytevector-c-short-ref bytevector-c-unsigned-short-ref bytevector-c-short-set!
                        bytevector-c-int-ref bytevector-c-unsigned-int-ref bytevector-c-int-set!
                        bytevector-c-long-ref bytevector-c-unsigned-long-ref bytevector-c-long-set!
                        bytevector-c-long-long-ref bytevector-c-unsigned-long-long-ref bytevector-c-long-long-set!
                        bytevector-c-void*-ref bytevector-c-void*-set!
                        bytevector-c-int8-ref bytevector-c-int16-ref bytevector-c-int32-ref bytevector-c-int64-ref
                        bytevector-c-uint8-ref bytevector-c-uint16-ref bytevector-c-uint32-ref bytevector-c-uint64-ref
                        bytevector-c-float-ref bytevector-c-double-ref
                        bytevector-c-int8-set! bytevector-c-int16-set! bytevector-c-int32-set! bytevector-c-int64-set!
                        bytevector-c-float-set! bytevector-c-double-set!
                        string->utf8/nul
                        bytevector-c-strlen

                        codegen-cdecl-callout
                        codegen-cdecl-callback
                        codegen-queue-count
                        codegen-queue-push!
                        display-codegen-statistics
                        closure-codegen

                        mat4x4-identity
                        mat4x4-dup
                        mat4x4-transpose
                        mat4x4-invert
                        mat4x4-orthonormalize
                        mat4x4-add
                        mat4x4-sub
                        mat4x4-mul
                        mat4x4-scale
                        mat4x4-translate
                        mat4x4-frustum
                        mat4x4-ortho
                        mat4x4-perspective
                        mat4x4-look-at
                        mat4x4-rotate

                        getenv
                        gethostname
                        system
                        process-spawn
                        process-wait
                        errno/string

                        make-uuid
                        time-usage
                        microsecond
                        microsecond->utc
                        microsecond->string
                        decode-microsecond
                        encode-microsecond

                        acquire-lockfile
                        release-lockfile

                        file-size-in-bytes
                        file-regular?
                        file-directory?
                        file-symbolic-link?
                        file-readable?
                        file-writable?
                        file-executable?
                        file-stat-ctime
                        file-stat-mtime
                        file-stat-atime
                        create-symbolic-link
                        create-hard-link
                        rename-file
                        change-file-mode
                        ;; win32-error->string
                        make-cmwc-random-state
                        cmwc-random-u32
                        cmwc-random-real

                        drop-last-cdr
                        drop-last-pair
                        last-pair
                        last-cdr
                        count-pair
                        last-n-pair
                        drop-last-n-pair

                        feature-identifiers
                        fulfill-feature-requirements?
                        continuation-to-exit
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
                        pointer?
                        make-pointer
                        pointer->scm
                        scm->pointer
                        pointer-address
                        pointer->bytevector
                        bytevector->pointer
                        set-pointer-finalizer!
                        dereference-pointer
                        pointer->procedure
                        dlopen
                        dlsym
                        dlclose
                        match
                        match-next
                        match-two
                        match-quasiquote
                        match-drop-ids
                        match-tuck-ids
                        match-drop-first-arg
                        match-gen-or
                        match-gen-or-step
                        match-gen-ellipsis
                        match-verify-no-ellipsis
                        match-gen-search
                        match-vector
                        match-vector-two
                        match-vector-step
                        match-gen-vector-ellipsis
                        match-vector-tail
                        match-vector-tail-two
                        match-record-refs
                        match-extract-vars
                        match-extract-vars-step
                        match-extract-quasiquote-vars
                        match-extract-quasiquote-vars-step
                        match-lambda
                        match-lambda*
                        match-let
                        match-letrec
                        match-let/helper
                        match-named-let
                        match-let*
                        match-check-ellipsis
                        match-check-identifier
                        match-bound-identifier-memv
                        )
    (import (capy)))


(library (core syntax-case)
    (export 
        syntax-case
          syntax
          with-syntax
          _
          ...
          ;make-variable-transformer
          identifier?
          bound-identifier=?
          free-identifier=?
          datum->syntax
          syntax->datum
          generate-temporaries
          quasisyntax
          syntax-violation
          datum
          ;define-macro
          unsyntax
          unsyntax-splicing)
    (import (capy))
    
    (define-syntax datum
        (syntax-rules ()
            ((_ x) (syntax->datum #'x))))
)