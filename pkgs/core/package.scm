;; The runtime's own libraries: the boot libraries, the core libraries and the primitives.
;; core imports nothing outside of itself (`no-std`), see scripts/check-layering.scm.
(package
  (name core)
  (version "3.1.0")
  (edition r6rs)
  (provides (core) (core *) (boot *) (capy) (|#%capy|))
  (source "src")
  (dependencies)
  (no-std #t)
  ;; Compiled by the Rust expander when the toolchain is built: the boot driver, the linker and
  ;; the boot files `init` requires (psyntax is not there yet). The other boot files are psyntax's.
  (boot-expander "boot/linker.scm" "boot/init.scm" "boot/main.scm"))
