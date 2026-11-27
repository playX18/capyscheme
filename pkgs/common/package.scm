;; Utilities that sit on top of the standard libraries.
(package
  (name common)
  (version "3.1.0")
  (edition r6rs)
  (provides (common *))
  (source "src")
  (dependencies
    (std (path "../std")))
  (no-std #f))
