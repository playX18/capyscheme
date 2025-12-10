(define-library (srfi 132 sorting)

  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!)

  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy! vector-fill!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-fill! r7rs-vector-fill!))
          (scheme cxr)
          (only (srfi srfi-27) random-integer))

  (import (rename (rnrs sorting)
                  (list-sort    r6rs-list-sort)
                  (vector-sort  r6rs-vector-sort)
                  (vector-sort! r6rs-vector-sort!)))
  (import (only (rnrs base) assert))

  (include "merge.scm")
  (include "delndups.scm")
  (include "sortp.scm")
  (include "vector-util.scm")
  (include "sortfaster.scm")
  (include "select.scm"))