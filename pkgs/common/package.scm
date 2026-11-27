;; Utilities that sit on top of the standard libraries.
(package
  (name common)
  (version "4.0.0")
  (edition r6rs)
  (provides (common *))
  (source "src")
  (dependencies
    (std (path "../std")))
  (no-std #f))
