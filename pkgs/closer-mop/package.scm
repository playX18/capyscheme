;; A Closer-MOP compatibility layer over the object system of (capy mop).
(package
  (name closer-mop)
  (version "4.0.0")
  (edition r6rs)
  (provides (closer-mop *))
  (source "src")
  (dependencies
    (std (path "../std")))
  (no-std #f))
