

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
                         substring string-append list->string string-copy string-for-each
                         vector? make-vector vector vector-length vector-ref vector-set!
                         vector->list list->vector vector-fill!
                         vector-map vector-for-each
                         error assertion-violation
                         apply call-with-current-continuation call/cc
                         values call-with-values dynamic-wind)
    (import (capy)))

(library (core primitives)
    (export 
        cond-expand
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
                        &stacktrace make-stacktrace-condition stacktrace-condition? condition-stacktrace
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
                        write-simple
                        write-shared
                        newline
                        read-char
                        write-char
                        ; r6rs i/o ports
                        make-custom-binary-input-port
                        make-custom-binary-input/output-port
                        make-custom-binary-output-port
                        make-custom-textual-input-port
                        make-custom-textual-input/output-port
                        make-custom-textual-output-port
                        call-with-port
                        eof-object eof-object?
                        standard-input-port standard-output-port standard-error-port
                        current-input-port current-output-port current-error-port
                        input-port? output-port? port?
                        flush-output-port
                        output-port-buffer-mode
                        close-port
                        lookahead-char
                        get-char
                        get-output-string
                        file-options
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
                        make-transcoder
                        trasncoder-codec
                        transcoder-eol-style
                        transcoder-error-handling-mode
                        eol-style
                        latin-1-codec
                        utf-16-codec
                        utf-8-codec
                        buffer-mode?
                        buffer-mode
                        native-transcoder
                        native-eol-style
                        bytevector->string
                        string->bytevector
                        binary-port?
                        textual-port?
                        port-transcoder
                        ; r6rs files
                        file-exists?
                        delete-file
                        ; r6rs hashtables
                        string-hash
                        symbol-hash
                        equal-hash
                        ; r6rs programs
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
                        string->utf16
                        utf16->string
                        string->utf32
                        utf32->string
                        transcoded-port
                        error-handling-mode
                        open-file-input/output-port
                        call-with-string-output-port
                        open-string-output-port
                        call-with-bytevector-output-port
                        open-bytevector-output-port
                        open-file-output-port
                        open-bytevector-input-port
                        open-file-input-port
                        call-with-output-file
                        call-with-input-file
                        with-input-from-file
                        with-output-to-file
                        open-output-file
                        open-input-file
                        close-output-port
                        close-input-port
                        transcoder-codec
                    
                        ; extensions
                       
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
                        interpreted-procedure?
                        interpreted-expression?
                        interpreted-expression-source
                        interpreted-procedure-meta

                        string-contains procedure?
                        bytevector-mapping?
                        directory-list
                        current-directory
                        create-directory
                        home-directory
                        decode-flonum
                        load
                        system-share-path
                        system-extension-path
                        process-environment->alist
                       
                        open-string-input-port
                       
                        string->uninterned-symbol
                        uninterned-symbol?

                        ; socket

                        port-closed?

                        ; ffi
                        string->utf8/nul

                        getenv
                        gethostname
                        system

                        errno/string

                        make-uuid
                        microsecond

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

                        drop-last-cdr
                        drop-last-pair
                        last-pair
                        last-cdr
                        count-pair
                        last-n-pair
                        drop-last-n-pair
                        char-upcase char-downcase char-titlecase char-foldcase
                        char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
                        char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char-title-case?
                        char-general-category
                        string-upcase string-downcase string-titlecase string-foldcase
                        string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                        string-normalize-nfd  string-normalize-nfkd string-normalize-nfc string-normalize-nfkc
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
                        errno 
                        ioctl/pointer
                        dlopen
                        dlsym
                        dlclose
                        load-native-extension
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
                        *raw-log*
                        *raw-log/src*
                        log:none
                        log:error
                        log:warn
                        log:info
                        log:debug
                        log:trace

                        log:max-level
                        log:set-max-level!
                        log:max
                        log:set-logger
                        *simple-logger*
                        
                        shadow-stack
                        resolve-address-name

                        %load-path
                        %load-extensions
                        %load-compiled-path
                        %load-compiled-extensions
                        %compile-fallback-path
                        %capy-root
                        %fresh-auto-compile
                        )
    (import (capy)))


(library (core syntax-case)
    (export 
        syntax-case
          syntax
          with-syntax
          _
          ...
          make-variable-transformer
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