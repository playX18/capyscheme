(import (srfi 64)
  (capy)
  (capy compiler tree-il)
  (capy compiler tree-il letrectify)
  (capy compiler tree-il resolve-free-vars)
  (capy compiler tree-il terms))

(test-begin "capy compiler tree-il")

(define src #f)

(test-group "tree-il->scheme lexical denoising"
  (let* ((id-a 'lexical-a-identity)
         (id-b 'lexical-b-identity)
         (term
           (make-let
             src
             'let
             (list 'foo-1890077875460208832-1
                   'foo-1890077875460208832-2)
             (list id-a id-b)
             (list (make-constant src 1)
                   (make-constant src 2))
             (make-primcall
               src
               '+
               (list (make-lref src 'wrong-a-1890077875460208832-10 id-a)
                     (make-lref src 'wrong-b-1890077875460208832-11 id-b))))))
    (test-equal
      "same cleaned base keeps distinct lexical aliases"
      (tree-il->scheme term '(denoise-lexicals?))
      '(let ((foo (quote 1))
             (foo.1 (quote 2)))
         (+ foo foo.1))))

  (let* ((id 'lexical-quote-identity)
         (term
           (make-let
             src
             'let
             (list 'foo-1890077875460208832-3)
             (list id)
             (list (make-constant src 'bar-1890077875460208832-99))
             (make-lref src 'foo-1890077875460208832-3 id))))
    (test-equal
      "quoted symbols are left unchanged"
      (tree-il->scheme term '(denoise-lexicals?))
      '(let ((foo (quote bar-1890077875460208832-99)))
         foo)))

  (let* ((proc-args (list 'lexical-x-identity))
         (proc-ids (list 'x-1890077875460208832-4))
         (term
           (make-proc
             src
             proc-args
             (make-lref src 'wrong-x-1890077875460208832-12 'lexical-x-identity)
             '()
             proc-ids)))
    (test-equal
      "lambda parameters are denoised by proc identity"
      (tree-il->scheme term '(denoise-lexicals?))
      '(lambda (x) x)))

  (let* ((proc-args (list 'lexical-syntax-x-identity))
         (proc-ids (list (datum->syntax #f 'x-1890077875460208832-5)))
         (term
           (make-proc
             src
             proc-args
             (make-lref src 'wrong-x-1890077875460208832-13 'lexical-syntax-x-identity)
             '()
             proc-ids)))
    (test-equal
      "lambda parameters with syntax-object readable ids are denoised"
      (tree-il->scheme term '(denoise-lexicals?))
      '(lambda (x) x))))

(test-group "resolve-free-vars"
  (let* ((provider-name '(test resolve-free-vars provider))
         (consumer-name '(test resolve-free-vars consumer))
         (provider (define-module* provider-name #t))
         (consumer (define-module* consumer-name #t)))
    (module-define! provider 'exported-value 42)
    (module-export! provider '(exported-value))
    (module-use-interfaces! consumer (list (module-public-interface provider)))
    (test-equal
      "imported free toplevel reference becomes public module ref"
      `(let ((module-var
               ((@@ (capy) define-module*)
                ',consumer-name)))
         (@ ,provider-name exported-value))
      (tree-il->scheme
        (resolve-free-vars
          (make-let
            src
            'let
            (list 'module-id)
            (list 'module-var)
            (list
              (make-application
                src
                (make-module-ref src '(capy) 'define-module* #f)
                (list (make-constant src consumer-name))))
            (make-toplevel-ref src consumer-name 'exported-value)))
        '()))))

(test-group "letrectify"
  (let* ((module-name '(test letrectify private))
         (module (define-module* module-name #t))
         (term
           (make-sequence
             src
             (make-toplevel-define
               src
               module-name
               'private-value
               (make-constant src 42))
             (make-toplevel-ref src module-name 'private-value))))
    (test-equal
      "private declarative toplevel define becomes lexical letrec"
      '(letrec ((private-value (quote 42)))
         private-value)
      (tree-il->scheme
        (letrectify term #:seal-private-bindings? #t)
        '(denoise-lexicals?)))))

(test-end "capy compiler tree-il")
