;; The implementation is (core pretty-print): the compiler of core uses it.
(library (capy pretty-print)
  (export pretty-print pretty-line-length)
  (import (core pretty-print)))
