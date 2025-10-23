(library (core io)

  (export file-options
          buffer-mode
          buffer-mode?
          utf-8-codec
          utf-16-codec
          latin-1-codec
          eol-style
          error-handling-mode
          make-transcoder
          transcoder-codec
          transcoder-eol-style
          transcoder-error-handling-mode
          native-transcoder
          native-eol-style
          bytevector->string
          string->bytevector
          eof-object
          eof-object?
          port?
          port-transcoder
          textual-port?
          binary-port?
          transcoded-port
          port-has-port-position?
          port-position
          port-has-set-port-position!?
          set-port-position!

          close-port
          call-with-port
          input-port?
          port-eof?

          open-file-input-port
          open-bytevector-input-port
          open-string-input-port
          standard-input-port
          current-input-port

          get-u8
          lookahead-u8
          get-bytevector-n
          get-bytevector-n!
          get-bytevector-some
          get-bytevector-all

          get-char
          lookahead-char
          get-string-n
          get-string-n!
          get-string-all
          get-line
          get-datum

          output-port?
          flush-output-port
          output-port-buffer-mode
          open-file-output-port
          open-bytevector-output-port
          call-with-bytevector-output-port
          open-string-output-port
          call-with-string-output-port
          standard-output-port
          standard-error-port
          current-output-port
          current-error-port

          put-u8
          put-bytevector
          put-char
          put-string
          put-datum

          open-file-input/output-port

          ; io simple
          call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          open-input-file
          open-output-file
          close-input-port
          close-output-port
          read-char
          peek-char
          read
          write-char
          newline
          display
          write
          get-output-string

          make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port

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
          format)

  (import 
          (core primitives)
          (core syntax-case)
          (core lists)
          (core conditions)
          (core bytevectors)
          (core optargs)
          (core io assistants)
          (core enums))
          
  ;; 8.2.2  File options

  (define-syntax file-options->bits
    (syntax-rules ()
      ((_ x who args)
       (begin
         (or (and (enum-set? x) (enum-set-subset? x (enum-set-universe (file-options))))
             (assertion-violation 'who (format "expected file-options object, but got ~r, as argument 2" x) args))
         (apply + (map (lambda (e) (port-lookup-file-option-code e)) (enum-set->list x)))))))
) ;[end]
