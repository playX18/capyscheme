;; Runtime introspection: GC, timing and runtime statistics.
(library (capy runtime)
  (export
    ;; GC. `gc-statistics` returns #(collections bytes reclaimed gc-cpu-ms gc-real-ms).
    collect-garbage
    gc-statistics
    gc-logging-enable!
    ;; Weak references (SRFI 124 primitives).
    make-ephemeron
    ephemeron?
    ephemeron-key
    ephemeron-datum
    reference-barrier
    ;; Timing. `statistics` returns
    ;; (cpu real bytes gc-count gc-cpu gc-real gc-bytes).
    cpu-time
    real-time
    statistics
    time
    ;; Runtime counters (also behind the CLI's stats flag).
    runtime-stats-enable!
    runtime-stats-enabled?
    runtime-stats-report)
  (import (only (capy)
            collect-garbage
            gc-statistics
            gc-logging-enable!
            make-ephemeron
            ephemeron?
            ephemeron-key
            ephemeron-datum
            reference-barrier
            cpu-time
            real-time
            statistics
            time
            runtime-stats-enable!
            runtime-stats-enabled?
            runtime-stats-report)))
