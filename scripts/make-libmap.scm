;;; Writes the libmap of a toolchain directory.
;;;
;;;   capy-rt -s scripts/make-libmap.scm TOOLCHAIN-DIR PKGS-DIR
;;;
;;; TOOLCHAIN-DIR/capy.libmap, for the packages of PKGS-DIR: see `write-toolchain-libmap!` in
;;; (capy pkg dist), which `capy install --toolchain` and `capy dist --toolchain` use for the
;;; installed tree.

(import (rnrs) (capy pkg dist))

(let ([args (cdr (command-line))])
  (unless (= (length args) 2)
    (display "usage: make-libmap.scm TOOLCHAIN-DIR PKGS-DIR\n" (current-error-port))
    (exit 2))
  (write-toolchain-libmap! (car args) (cadr args)))
