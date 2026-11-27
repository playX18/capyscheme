;;; Using a native extension from a library.
;;;
;;;   (library (my ext)
;;;     (export fast-add)
;;;     (import (rnrs) (capy native))
;;;     (require-native-extension fast)
;;;     (define fast-add (native-extension-ref '(my fast) 'fast-add)))
;;;
;;; `(require-native-extension NAME)` loads the native extension NAME of the `native` section of a
;;; package manifest when the library runs. NAME is the name the manifest gives it, or its key
;;; `(|#%native| <package-id> <name>)` when two packages have an extension of that name. The macro
;;; looks the extension up in the libmap while the library is compiled:
;;;
;;;   - an extension that does not exist is an error of the compilation, not of the run;
;;;   - the identity of the extension is recorded in the library as a compile-time dependency, like a
;;;     header `c-include` parsed, so the library is compiled again when the extension changes (its
;;;     sources, Cargo.lock or flags: see `(capy pkg native)`);
;;;   - the key is a constant of the code, which is how `capyc --static --exe` knows to put the shared
;;;     library beside the executable (lib/<package-id>/<name>.<dll>), where it is loaded from.
;;;
;;; The extension itself registers what it defines in a module of its choosing (`SNI_OnLoad`,
;;; `#[onload]`); the library takes the procedures from that module's instance.

(library (capy native)
  (export require-native-extension native-extension-ref)
  (import (rnrs) (capy))

  ;; The value NAME of the module MODULE (a library name) that a native extension registered when it
  ;; was loaded.
  (define (native-extension-ref module name)
    (instance-ref (library-instance (root-library-registry) module) name))

  (define-syntax require-native-extension
    (lambda (stx)
      (syntax-case stx ()
        [(_ spec)
          (let ([key (linker-require-native! (syntax->datum #'spec))])
            (with-syntax ([key (datum->syntax #'spec key)])
              #'(load-native-extension 'key)))]))))
