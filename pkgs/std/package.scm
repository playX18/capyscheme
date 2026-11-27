;; The standard libraries: R6RS, R7RS, SRFIs and the (capy ...) libraries.
(package
  (name std)
  (version "4.0.0")
  (edition r6rs)
  (provides (rnrs) (rnrs *) (scheme *) (srfi *) (capy *) (cc))
  (source "src")
  (dependencies
    (core (path "../core")))
  (native
    (cparse (cargo "../../crates/capy-cparse-sni")))
  (no-std #f))
