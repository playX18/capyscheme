;; Scheduler control: worker indices, pinning and thread priorities.
;;
;; Kept out of (capy concurrency) because most programs never need a worker
;; index. Import it for the cases that do, e.g. running a GUI library on the
;; main thread:
;;
;;   (import (capy concurrency) (capy scheduler))
;;   (pin (main-worker))
(library (capy scheduler)
  (export
    main-worker
    current-worker
    pinned-worker
    worker-count
    pin
    unpin
    go-pinned
    go/priority
    priority-levels
    thread-priority
    set-thread-priority!)
  (import (core thread)))
