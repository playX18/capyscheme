;; Clocks for (capy time): clock_gettime(2) through (core foreign).
;;
;; `clock-now` reads one of the four POSIX clocks as two exact integers, seconds and nanoseconds.
;; The clock ids differ between systems and are listed here for macOS and Linux; elsewhere, and
;; when a call fails, the real-time and monotonic clocks fall back on gettimeofday(2)
;; (microseconds) and the CPU clocks on the process CPU time (milliseconds).
;;
;; The binding is written by hand rather than with c-include: it is one function and four
;; constants, and it keeps the header parser out of the build of std.
(library (capy time sys)
  (export clock-now)
  (import
    (rnrs)
    (only (scheme base) cond-expand)
    (only (core primitives) current-jiffy)
    (only (capy) cpu-time make-bytevector/nonmoving)
    (only (core foreign) int long sizeof)
    (only (core foreign-library) load-foreign-library foreign-library-function))

  ;; CLOCK_REALTIME, CLOCK_MONOTONIC, CLOCK_PROCESS_CPUTIME_ID, CLOCK_THREAD_CPUTIME_ID.
  (define clock-ids
    (cond-expand
      [macos '((realtime . 0) (monotonic . 6) (process . 12) (thread . 16))]
      [linux '((realtime . 0) (monotonic . 1) (process . 2) (thread . 3))]
      [else #f]))

  (define clock-gettime
    (and clock-ids
         (guard (e [#t #f])
           (foreign-library-function (load-foreign-library #f) "clock_gettime"
             #:return int #:arguments (list int '*) #:blocking? #f))))

  ;; struct timespec { time_t tv_sec; long tv_nsec; }, with time_t as wide as long on the
  ;; systems above.
  (define word (sizeof long))

  (define (word-ref bv offset)
    (if (= word 8)
      (bytevector-s64-native-ref bv offset)
      (bytevector-s32-native-ref bv offset)))

  (define (fallback which)
    (case which
      [(realtime monotonic)
       (let ([us (current-jiffy)])
         (values (div us 1000000) (* 1000 (mod us 1000000))))]
      [else
       (let ([ms (cpu-time)])
         (values (div ms 1000) (* 1000000 (mod ms 1000))))]))

  ;; Seconds and nanoseconds of the clock WHICH: realtime, monotonic, process or thread.
  (define (clock-now which)
    (unless (memq which '(realtime monotonic process thread))
      (assertion-violation 'clock-now "unknown clock" which))
    (if clock-gettime
      (let ([ts (make-bytevector/nonmoving (* 2 word) 0)])
        (if (zero? (clock-gettime (cdr (assq which clock-ids)) ts))
          (values (word-ref ts 0) (word-ref ts word))
          (fallback which)))
      (fallback which))))
