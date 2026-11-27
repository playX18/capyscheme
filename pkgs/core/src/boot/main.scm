;; Boot driver
(|#%require|)

(let ()
  (define registry (root-library-registry))
  (define linklet
    (let ([thunk (load-thunk-in-vicinity "boot/linker.scm" #t)])
      (receive vals (thunk) (car vals))))
  (define instance (make-linklet-instance (linklet-name linklet) #f))

  (define (import-instances imports)
    (if (null? imports)
        '()
        (cons (or (library-instance registry (car (car imports)))
                  (%raise (list 'boot "the linker imports a library that is not instantiated"
                                (car (car imports)))))
              (import-instances (cdr imports)))))

  (apply (linklet-body linklet)
         (linklet-slots registry linklet (import-instances (linklet-imports linklet)) instance))
  (register-library-instance! registry (linklet-name linklet) instance)

  ;; The libmap says where the compiled libraries are, boot files included, so it is installed
  ;; before anything else is required.
  (if %libmap-path
      ((instance-ref instance 'linker-install-libmap!) %libmap-path))

  ((instance-ref instance 'linker-require!) '(boot init)))
