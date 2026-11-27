;; The package manager and the `capy` driver.
(package
  (name capy-pkg)
  (version "0.1.0")
  (edition r6rs)
  (provides (capy pkg *))
  (source "src")
  (dependencies
    (std (path "../std")))
  (programs
    (capy "src/capy/pkg/main.scm" (static #t)))
  (no-std #f))
