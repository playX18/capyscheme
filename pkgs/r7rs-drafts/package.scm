;; Libraries of R7RS-large drafts that are not part of the standard libraries. They use
;; nothing but the primitives of (capy), so they need no std.
(package
  (name r7rs-drafts)
  (version "4.0.0")
  (edition r7rs)
  (provides (r7rs-drafts *))
  (source "src")
  (dependencies)
  (no-std #t))
