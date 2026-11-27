;;; Toolchain handling, used by bootstrap and for development of Capy. 
;;; Not for public use!

(library (capy pkg toolchain)
  (export locate-toolchain
    make-bootstrap-toolchain
    toolchain-bootstrap?
    locate-runtime
    toolchain-runtime
    toolchain-libmap-path
    toolchain-libmap
    toolchain-has-library?
    toolchain-entry
    toolchain-entries
    toolchain-native-entries
    toolchain-package-ids
    native-libraries)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) getenv executable-path %libmap-path file-exists?)
    (capy libmap)
    (capy path)
    (capy pkg ui))

  (define-record-type toolchain
    (fields runtime libmap-path libmap table))

;; The toolchain of a build that makes the toolchain itself: it is the runtime that compiles, and
  ;; there is no libmap, since the libraries are the ones being built. The runtime is `capy-rt` next
  ;; to the driver, or the one `CAPY_RT` names.
  ;; The libraries the runtime defines natively (`(capy)`, `(|#%capy|)`) have no source: they are
  ;; there before anything is compiled.
  (define native-libraries '((capy) (|#%capy|)))

  (define (make-bootstrap-toolchain)
    (let ([table (make-hashtable equal-hash equal?)])
      (for-each (lambda (key) (hashtable-set! table key (list key #f #f #f #f #f))) native-libraries)
      (make-toolchain (locate-runtime) #f #f table)))

  (define (toolchain-bootstrap? tc) (not (toolchain-libmap tc)))

  (define (locate-runtime)
    (let ([runtime (or (let ([env (getenv "CAPY_RT")]) (and env (not (string=? env "")) env))
                     (path-join (path-directory (executable-path)) "capy-rt"))])
      (unless (file-exists? runtime)
        (fail "cannot find the runtime executable" runtime "(set CAPY_RT)"))
      runtime))

  (define (locate-toolchain)
    (let ([libmap-path %libmap-path])
      (unless libmap-path
        (fail "cannot find the libmap of the toolchain (is capy installed? set CAPY_LIBMAP)"))
      (let* ([libmap (read-libmap libmap-path)]
             [table (make-hashtable equal-hash equal?)])
        (for-each (lambda (entry) (hashtable-set! table (car entry) entry)) (libmap-entries libmap))
        (make-toolchain (locate-runtime) libmap-path libmap table))))

  ;; Whether the toolchain has the library KEY: one of its libmap, or one the runtime defines
  ;; natively, which no libmap lists.
  (define (toolchain-has-library? tc key)
    (or (and (hashtable-ref (toolchain-table tc) key #f) #t)
      (and (member key native-libraries) #t)))

  ;; The entry `(key source fasl index interface-hash wpo)` of the library KEY, or #f.
  (define (toolchain-entry tc key) (hashtable-ref (toolchain-table tc) key #f))

  ;; The ids of the packages of the toolchain.
  (define (toolchain-package-ids tc)
    (if (toolchain-libmap tc) (map car (libmap-views (toolchain-libmap tc))) '()))

  ;; The entries of the toolchain with absolute paths, so that they can go in the libmap of a
  ;; package build, which is in another directory.
  (define (toolchain-entries tc)
    (if (not (toolchain-libmap tc))
      '()
      (toolchain-entries* tc)))

;; The native extensions of the toolchain, `(key file identity)` with absolute files.
  (define (toolchain-native-entries tc)
    (if (not (toolchain-libmap tc))
      '()
      (let ([directory (path-directory (toolchain-libmap-path tc))])
        (map (lambda (entry)
              (list (car entry) (path-join directory (cadr entry)) (caddr entry)))
          (libmap-native (toolchain-libmap tc))))))

  (define (toolchain-entries* tc)
    (let ([directory (path-directory (toolchain-libmap-path tc))])
      (define (absolute path) (and path (path-join directory path)))
      (map (lambda (entry)
            (list (list-ref entry 0)
              (absolute (list-ref entry 1))
              (absolute (list-ref entry 2))
              (list-ref entry 3)
              (list-ref entry 4)
              (absolute (list-ref entry 5))))
        (libmap-entries (toolchain-libmap tc))))))
