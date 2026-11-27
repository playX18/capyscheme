;;; Time: instants, durations, clocks, civil dates and times, time zones, parsing and formatting.
;;;
;;;   (import (capy time))
;;;
;;;   (now)                                   ; => #<instant 2026-11-25T10:31:07.118374Z>
;;;   (format-rfc3339 (now))                  ; => "2026-11-25T10:31:07.118374Z"
;;;   (define t (parse-rfc3339 "2024-02-28T22:30:00+01:00"))
;;;   (datetime-weekday t)                    ; => 3, a Wednesday (Sunday is 0)
;;;   (format-datetime (datetime-add t #:days 2) "%A %e %B %Y, %H:%M")
;;;                                           ; => "Friday  1 March 2024, 22:30"
;;;   (format-http-date (datetime->instant t)); => "Wed, 28 Feb 2024 21:30:00 GMT"
;;;   (parse-http-date "Sun, 06 Nov 1994 08:49:37 GMT")   ; => #<instant 1994-11-06T08:49:37Z>
;;;
;;;   (define berlin (load-zone "Europe/Berlin"))
;;;   (instant->datetime (now) berlin)        ; => #<datetime 2026-11-25T11:31:07.118374+01:00 Europe/Berlin>
;;;   (make-datetime 2024 3 31 2 30 #:zone berlin)   ; in the spring gap: 03:30+02:00
;;;
;;;   (define sw (stopwatch))  ...  (duration->string (sw))   ; => "12.5ms"
;;;   (duration #:hours 1 #:minutes 30)       ; => #<duration 1h30m>
;;;   (parse-duration "1h30m")                ; the same
;;;
;;; The pieces:
;;;
;;; - An *instant* is a point on the UTC time line: seconds and nanoseconds since 1970-01-01T00:00Z,
;;;   leap seconds not counted (POSIX time). `now` reads the system clock.
;;; - A *duration* is an exact span of time, seconds and nanoseconds, negative or positive.
;;;   `monotonic-now` reads a clock that never jumps back; `stopwatch` and `elapsed` use it.
;;; - A *datetime* is a civil date and time of day with the UTC offset it was read or computed
;;;   at, in seconds east of UTC, and optionally the zone it belongs to. Years are proleptic
;;;   Gregorian and unbounded; months and days count from 1, weekdays from Sunday = 0.
;;; - A *zone* maps instants to offsets: `utc`, `(fixed-zone secs)`, `(load-zone "Area/City")`
;;;   from the system's tz database, `(posix-zone "EST5EDT,M3.2.0,M11.1.0")` and `(local-zone)`.
;;;
;;; Formats: RFC 3339 and ISO 8601 (`format-rfc3339`, `parse-rfc3339`, `format-iso8601`,
;;; `parse-iso8601`), HTTP dates (RFC 9110/7231: `format-http-date`, `parse-http-date`, which
;;; accepts the obsolete RFC 850 and asctime forms too), and strftime/strptime-style patterns
;;; (`format-datetime`, `parse-datetime`).
(library (capy time)
  (export
    ;; instants
    instant? make-instant instant-seconds instant-nanoseconds now unix-time
    instant->seconds seconds->instant instant+ instant- instant=? instant<? instant<=?
    instant>? instant>=? instant-compare unix-epoch
    ;; durations
    duration? make-duration duration duration-seconds duration-nanoseconds
    duration->seconds seconds->duration duration->nanoseconds nanoseconds->duration
    duration->milliseconds
    duration+ duration- duration* duration-negate duration-abs duration-zero?
    duration-negative? duration=? duration<? duration<=? duration>? duration>=?
    duration->string parse-duration zero-duration
    ;; clocks
    monotonic-now clock-time stopwatch elapsed
    ;; calendar
    leap-year? days-in-month days-in-year day-of-week day-of-year weekday-name month-name
    ;; zones
    zone? zone-name utc fixed-zone load-zone posix-zone local-zone zone-offset-at
    zone-abbreviation-at zone-dst-at? zone-fixed-offset tzif->zone zone-search-path
    ;; datetimes
    datetime? make-datetime datetime-year datetime-month datetime-day datetime-hour
    datetime-minute datetime-second datetime-nanosecond datetime-offset datetime-zone
    datetime-weekday datetime-day-of-year datetime-iso-week datetime-days
    instant->datetime datetime->instant datetime-with-zone datetime-with-offset
    current-datetime today datetime+ datetime- datetime-add datetime-truncate
    datetime=? datetime<? datetime<=? datetime>? datetime>=? datetime-compare
    days-between datetime-abbreviation
    ;; formats
    format-rfc3339 parse-rfc3339 format-iso8601 parse-iso8601
    format-http-date parse-http-date format-datetime parse-datetime
    &time-parse-error time-parse-error? time-parse-error-input)
  (import
    (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (only (capy) getenv file-exists? canonicalize-path-string set-rtd-printer!)
    (only (capy sync) make-concurrent-hash-table concurrent-hash-table-get
          concurrent-hash-table-set!)
    (capy time civil)
    (capy time tzif)
    (capy time sys))

  (define billion 1000000000)

  ;; ======================================================================================
  ;; Durations
  ;; ======================================================================================

  ;; Normalized: 0 <= nanoseconds < 10^9, seconds any exact integer. -1.5s is (-2, 5*10^8).
  (define-record-type (<duration> %make-duration duration?)
    (fields (immutable seconds duration-seconds)
            (immutable nanoseconds duration-nanoseconds)))

  (define (check-integer who x)
    (unless (and (integer? x) (exact? x))
      (assertion-violation who "expected an exact integer" x)))

  (define (make-duration seconds [nanoseconds 0])
    "A duration of SECONDS plus NANOSECONDS, both exact integers of any sign."
    (check-integer 'make-duration seconds)
    (check-integer 'make-duration nanoseconds)
    (nanoseconds->duration (+ (* seconds billion) nanoseconds)))

  (define (nanoseconds->duration ns)
    (check-integer 'nanoseconds->duration ns)
    (%make-duration (div ns billion) (mod ns billion)))

  (define (duration->nanoseconds d)
    (+ (* (duration-seconds d) billion) (duration-nanoseconds d)))

  ;; A real number of seconds, rounded to the nanosecond.
  (define (seconds->duration secs)
    (unless (real? secs) (assertion-violation 'seconds->duration "expected a real number" secs))
    (nanoseconds->duration (exact (round (* (exact secs) billion)))))

  (define (duration->seconds d)
    "The duration in seconds, as a flonum."
    (inexact (/ (duration->nanoseconds d) billion)))

  (define (duration->milliseconds d)
    "The duration in whole milliseconds, rounded toward negative infinity."
    (div (duration->nanoseconds d) 1000000))

  (define zero-duration (%make-duration 0 0))

  ;; Keyword constructor; every part is a real number and they add up.
  ;;   (duration #:hours 1 #:minutes 30)  (duration #:milliseconds 250)
  (define (duration #:weeks [w 0] #:days [d 0] #:hours [h 0] #:minutes [m 0] #:seconds [s 0]
                    #:milliseconds [ms 0] #:microseconds [us 0] #:nanoseconds [ns 0])
    (nanoseconds->duration
      (exact (round (+ (* (exact w) 604800 billion) (* (exact d) 86400 billion)
                       (* (exact h) 3600 billion) (* (exact m) 60 billion) (* (exact s) billion)
                       (* (exact ms) 1000000) (* (exact us) 1000) (exact ns))))))

  (define (check-duration who d)
    (unless (duration? d) (assertion-violation who "expected a duration" d)))

  (define (duration+ . ds)
    (for-each (lambda (d) (check-duration 'duration+ d)) ds)
    (nanoseconds->duration (fold-left (lambda (acc d) (+ acc (duration->nanoseconds d))) 0 ds)))

  (define (duration- d . ds)
    (check-duration 'duration- d)
    (if (null? ds)
      (duration-negate d)
      (nanoseconds->duration
        (fold-left (lambda (acc x) (check-duration 'duration- x) (- acc (duration->nanoseconds x)))
                   (duration->nanoseconds d) ds))))

  (define (duration* d k)
    "D scaled by the real number K, rounded to the nanosecond."
    (check-duration 'duration* d)
    (nanoseconds->duration (exact (round (* (duration->nanoseconds d) (exact k))))))

  (define (duration-negate d) (nanoseconds->duration (- (duration->nanoseconds d))))
  (define (duration-abs d) (if (duration-negative? d) (duration-negate d) d))
  (define (duration-zero? d) (and (zero? (duration-seconds d)) (zero? (duration-nanoseconds d))))
  (define (duration-negative? d) (negative? (duration-seconds d)))

  (define (duration-cmp a b)
    (check-duration 'duration-compare a)
    (check-duration 'duration-compare b)
    (let ([x (duration->nanoseconds a)] [y (duration->nanoseconds b)])
      (cond [(< x y) -1] [(> x y) 1] [else 0])))

  (define (duration=? a b) (= 0 (duration-cmp a b)))
  (define (duration<? a b) (= -1 (duration-cmp a b)))
  (define (duration<=? a b) (<= (duration-cmp a b) 0))
  (define (duration>? a b) (= 1 (duration-cmp a b)))
  (define (duration>=? a b) (>= (duration-cmp a b) 0))

  ;; Go's format: "1h2m3.5s", "1.5ms", "250us", "0s", "-3s". Days are written as hours.
  (define (duration->string d)
    (let* ([total (duration->nanoseconds d)] [neg? (negative? total)] [ns (abs total)])
      (define (frac n unit)
        ;; n/unit with up to 9 fraction digits, trailing zeros dropped.
        (let-values ([(q r) (div-and-mod n unit)])
          (if (zero? r)
            (number->string q)
            (let* ([digits (let loop ([u unit] [k 0]) (if (= u 1) k (loop (div u 10) (+ k 1))))]
                   [s (number->string r)]
                   [s (string-append (make-string (- digits (string-length s)) #\0) s)]
                   [s (let trim ([e (string-length s)])
                        (if (char=? (string-ref s (- e 1)) #\0) (trim (- e 1)) (substring s 0 e)))])
              (string-append (number->string q) "." s)))))
      (string-append
        (if neg? "-" "")
        (cond
          [(zero? ns) "0s"]
          [(< ns 1000) (string-append (number->string ns) "ns")]
          [(< ns 1000000) (string-append (frac ns 1000) "us")]
          [(< ns billion) (string-append (frac ns 1000000) "ms")]
          [else
           (let* ([secs (div ns billion)] [h (div secs 3600)] [m (div (mod secs 3600) 60)]
                  [s-part (frac (- ns (* (+ (* h 3600) (* m 60)) billion)) billion)])
             (string-append
               (if (> h 0) (string-append (number->string h) "h") "")
               (if (or (> h 0) (> m 0)) (string-append (number->string m) "m") "")
               s-part "s"))]))))

  ;; "1h30m", "2.5s", "-1m", "300ms", "1d12h". Units: ns us µs ms s m h d w.
  (define (parse-duration str)
    (define n (string-length str))
    (define (fail) (raise-parse-error 'parse-duration "invalid duration" str))
    (define units
      '(("ns" . 1) ("us" . 1000) ("\xB5;s" . 1000) ("\x3BC;s" . 1000) ("ms" . 1000000)
        ("s" . 1000000000) ("m" . 60000000000) ("h" . 3600000000000)
        ("d" . 86400000000000) ("w" . 604800000000000)))
    (when (= n 0) (fail))
    (let* ([sign (case (string-ref str 0) [(#\-) -1] [else 1])]
           [start (if (memv (string-ref str 0) '(#\- #\+)) 1 0)])
      (when (= start n) (fail))
      (if (and (= (- n start) 1) (char=? (string-ref str start) #\0))
        zero-duration
        (let loop ([i start] [total 0])
          (if (= i n)
            (nanoseconds->duration (* sign total))
            (let num ([j i])
              (if (and (< j n) (or (char-numeric? (string-ref str j)) (char=? (string-ref str j) #\.)))
                (num (+ j 1))
                (let* ([value (and (> j i) (string->number (substring str i j)))]
                       [_ (unless (and value (real? value)) (fail))]
                       [k (let unit ([k j])
                            (if (and (< k n) (not (char-numeric? (string-ref str k)))
                                     (not (char=? (string-ref str k) #\.)))
                              (unit (+ k 1))
                              k))]
                       [u (assoc (substring str j k) units)])
                  (unless u (fail))
                  (loop k (+ total (exact (round (* (exact value) (cdr u))))))))))))))

  ;; ======================================================================================
  ;; Instants
  ;; ======================================================================================

  (define-record-type (<instant> %make-instant instant?)
    (fields (immutable seconds instant-seconds)
            (immutable nanoseconds instant-nanoseconds)))

  (define (make-instant seconds [nanoseconds 0])
    "The instant SECONDS plus NANOSECONDS after 1970-01-01T00:00:00Z."
    (check-integer 'make-instant seconds)
    (check-integer 'make-instant nanoseconds)
    (let ([ns (+ (* seconds billion) nanoseconds)])
      (%make-instant (div ns billion) (mod ns billion))))

  (define unix-epoch (%make-instant 0 0))

  (define (check-instant who x)
    (unless (instant? x) (assertion-violation who "expected an instant" x)))

  (define (instant->nanoseconds i) (+ (* (instant-seconds i) billion) (instant-nanoseconds i)))
  (define (nanoseconds->instant ns) (%make-instant (div ns billion) (mod ns billion)))

  (define (now)
    "The current instant, from the system's real-time clock."
    (let-values ([(s ns) (clock-now 'realtime)]) (%make-instant s ns)))

  (define (unix-time)
    "The current time in whole seconds since the epoch."
    (let-values ([(s ns) (clock-now 'realtime)]) s))

  (define (instant->seconds i)
    "Seconds since the epoch, as a flonum."
    (check-instant 'instant->seconds i)
    (inexact (+ (instant-seconds i) (/ (instant-nanoseconds i) billion))))

  (define (seconds->instant secs)
    "The instant SECS (a real number) seconds after the epoch, as `current-second` returns."
    (unless (real? secs) (assertion-violation 'seconds->instant "expected a real number" secs))
    (nanoseconds->instant (exact (round (* (exact secs) billion)))))

  (define (instant+ i d)
    (check-instant 'instant+ i)
    (check-duration 'instant+ d)
    (nanoseconds->instant (+ (instant->nanoseconds i) (duration->nanoseconds d))))

  ;; (instant- a b): the duration from B to A. (instant- a d): A moved back by the duration D.
  (define (instant- a b)
    (check-instant 'instant- a)
    (cond
      [(instant? b) (nanoseconds->duration (- (instant->nanoseconds a) (instant->nanoseconds b)))]
      [(duration? b) (nanoseconds->instant (- (instant->nanoseconds a) (duration->nanoseconds b)))]
      [else (assertion-violation 'instant- "expected an instant or a duration" b)]))

  (define (instant-compare a b)
    (check-instant 'instant-compare a)
    (check-instant 'instant-compare b)
    (cond
      [(< (instant-seconds a) (instant-seconds b)) -1]
      [(> (instant-seconds a) (instant-seconds b)) 1]
      [(< (instant-nanoseconds a) (instant-nanoseconds b)) -1]
      [(> (instant-nanoseconds a) (instant-nanoseconds b)) 1]
      [else 0]))

  (define (instant=? a b) (= 0 (instant-compare a b)))
  (define (instant<? a b) (= -1 (instant-compare a b)))
  (define (instant<=? a b) (<= (instant-compare a b) 0))
  (define (instant>? a b) (= 1 (instant-compare a b)))
  (define (instant>=? a b) (>= (instant-compare a b) 0))

  ;; ======================================================================================
  ;; Clocks
  ;; ======================================================================================

  (define (clock-time clock)
    "The reading of CLOCK as a duration: realtime (since the epoch), monotonic (since an
unspecified start), process or thread (CPU time used)."
    (let-values ([(s ns) (clock-now clock)]) (%make-duration s ns)))

  (define (monotonic-now)
    "A reading of the monotonic clock, as a duration since an unspecified start."
    (clock-time 'monotonic))

  (define (elapsed start)
    "The duration since START, an earlier `monotonic-now`."
    (duration- (monotonic-now) start))

  (define (stopwatch)
    "A procedure that returns the duration since the stopwatch was made."
    (let ([start (monotonic-now)])
      (lambda () (elapsed start))))

  ;; ======================================================================================
  ;; Calendar
  ;; ======================================================================================

  (define (day-of-week y m d)
    "0 for Sunday through 6 for Saturday."
    (days->weekday (civil->days y m d)))

  (define (day-of-year y m d) (year-day y m d))

  (define weekday-names '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
  (define month-names
    '#("January" "February" "March" "April" "May" "June" "July" "August" "September" "October"
       "November" "December"))

  (define (abbrev s) (substring s 0 3))

  (define (weekday-name wd #:short? [short? #f])
    "The English name of weekday WD, 0 = Sunday. With #:short? #t, the three-letter form."
    (let ([s (vector-ref weekday-names (mod wd 7))]) (if short? (abbrev s) s)))

  (define (month-name m #:short? [short? #f])
    "The English name of month M, 1 = January."
    (unless (and (fixnum? m) (<= 1 m 12)) (assertion-violation 'month-name "month out of range" m))
    (let ([s (vector-ref month-names (- m 1))]) (if short? (abbrev s) s)))

  ;; ======================================================================================
  ;; Zones
  ;; ======================================================================================

  (define-record-type (<zone> %make-zone zone?)
    (fields (immutable name zone-name) (immutable data zone-data)))

  (define (zone-lookup z secs)
    (tzdata-lookup (zone-data z) secs))

  (define (fixed-tzdata offset abbrev)
    (make-tzdata (vector) (vector) (vector offset #f abbrev) #f))

  (define utc (%make-zone "UTC" (fixed-tzdata 0 "UTC")))

  (define (offset->string offset colon?)
    (let* ([sign (if (negative? offset) "-" "+")] [a (abs offset)]
           [h (div a 3600)] [m (div (mod a 3600) 60)] [s (mod a 60)])
      (string-append sign (pad h 2) (if colon? ":" "") (pad m 2)
                     (if (zero? s) "" (string-append (if colon? ":" "") (pad s 2))))))

  (define (fixed-zone offset [name #f])
    "A zone always OFFSET seconds east of UTC."
    (check-integer 'fixed-zone offset)
    (unless (< (abs offset) 86400) (assertion-violation 'fixed-zone "offset out of range" offset))
    (if (and (zero? offset) (not name))
      utc
      (let ([name (or name (string-append "UTC" (offset->string offset #t)))])
        (%make-zone name (fixed-tzdata offset name)))))

  (define (zone-fixed-offset z)
    "The offset of Z if it never changes, else #f."
    (let ([d (zone-data z)])
      (and (zero? (vector-length (tzdata-transitions d)))
           (not (tzdata-rule d))
           (let-values ([(off dst? abbr) (tzdata-lookup d 0)]) off))))

  (define (posix-zone tz-string [name #f])
    "A zone from a POSIX TZ rule string such as \"CET-1CEST,M3.5.0,M10.5.0/3\"."
    (%make-zone (or name tz-string) (parse-posix-tz tz-string)))

  (define (tzif->zone bv name)
    "A zone named NAME from the bytes of a TZif file."
    (%make-zone name (bytevector->tzdata bv)))

  (define (zone-search-path)
    "The directories `load-zone` looks in: $TZDIR, then the usual system locations."
    (let ([env (getenv "TZDIR")])
      (append (if (and env (not (string=? env ""))) (list env) '())
              '("/usr/share/zoneinfo" "/var/db/timezone/zoneinfo" "/usr/lib/zoneinfo"
                "/usr/share/lib/zoneinfo" "/etc/zoneinfo"))))

  (define zone-cache (make-concurrent-hash-table))

  (define (safe-zone-name? name)
    (and (> (string-length name) 0)
         (not (char=? (string-ref name 0) #\/))
         (let loop ([cs (string->list name)] [prev #\/])
           (cond
             [(null? cs) #t]
             [(and (char=? (car cs) #\.) (char=? prev #\/)) #f]
             [(or (char-alphabetic? (car cs)) (char-numeric? (car cs))
                  (memv (car cs) '(#\/ #\_ #\- #\+ #\.)))
              (loop (cdr cs) (car cs))]
             [else #f]))))

  (define (load-zone name)
    "The zone NAME (\"Europe/Berlin\", \"America/New_York\", \"UTC\") of the system's tz
database. Raises an error naming the directories searched if there is no such zone."
    (unless (string? name) (assertion-violation 'load-zone "expected a zone name" name))
    (or (concurrent-hash-table-get zone-cache name #f)
        (let ([z (cond
                   [(member name '("UTC" "Etc/UTC" "Z" "Zulu" "GMT" "Etc/GMT")) (%make-zone name (fixed-tzdata 0 (if (string=? name "GMT") "GMT" "UTC")))]
                   [(not (safe-zone-name? name)) (error 'load-zone "invalid zone name" name)]
                   [else
                    (let loop ([dirs (zone-search-path)])
                      (cond
                        [(null? dirs)
                         (error 'load-zone "no such time zone in the tz database" name (zone-search-path))]
                        [(file-exists? (string-append (car dirs) "/" name))
                         (let ([path (string-append (car dirs) "/" name)])
                           (%make-zone name (guard (e [#t (error 'load-zone "cannot read the zone file" path)])
                                              (read-tzif path))))]
                        [else (loop (cdr dirs))]))])])
          (concurrent-hash-table-set! zone-cache name z)
          z)))

  ;; The zone name of a path into a zoneinfo directory, ".../zoneinfo/Europe/Berlin".
  (define (zoneinfo-name path)
    (let ([key "zoneinfo/"] [n (string-length path)])
      (let loop ([i (- n 9)])
        (cond
          [(< i 0) #f]
          [(string=? (substring path i (+ i 9)) key) (substring path (+ i 9) n)]
          [else (loop (- i 1))]))))

  (define (detect-local-zone)
    (let ([tz (getenv "TZ")])
      (cond
        [(and tz (not (string=? tz "")))
         (let ([tz (if (char=? (string-ref tz 0) #\:) (substring tz 1 (string-length tz)) tz)])
           (or (guard (e [#t #f])
                 (if (and (> (string-length tz) 0) (char=? (string-ref tz 0) #\/))
                   (tzif->zone (call-with-port (open-file-input-port tz) get-bytevector-all)
                               (or (zoneinfo-name tz) tz))
                   (load-zone tz)))
               (guard (e [#t #f]) (posix-zone tz))
               utc))]
        [(file-exists? "/etc/localtime")
         (guard (e [#t utc])
           (let ([name (or (guard (e [#t #f]) (zoneinfo-name (canonicalize-path-string "/etc/localtime")))
                           "localtime")])
             (or (guard (e [#t #f]) (and (not (string=? name "localtime")) (load-zone name)))
                 (tzif->zone (call-with-port (open-file-input-port "/etc/localtime") get-bytevector-all)
                             name))))]
        [else utc])))

  (define cached-local-zone #f)

  (define (local-zone)
    "The system's zone: from $TZ, else /etc/localtime, else UTC. Read once and remembered."
    (or cached-local-zone
        (let ([z (detect-local-zone)])
          (set! cached-local-zone z)
          z)))

  (define (zone-of who x)
    (cond
      [(zone? x) x]
      [(and (integer? x) (exact? x)) (fixed-zone x)]
      [(string? x) (load-zone x)]
      [(eq? x 'local) (local-zone)]
      [(eq? x 'utc) utc]
      [else (assertion-violation who "expected a zone, an offset in seconds or a zone name" x)]))

  (define (instant-secs who x)
    (cond
      [(instant? x) (instant-seconds x)]
      [(datetime? x) (instant-seconds (datetime->instant x))]
      [else (assertion-violation who "expected an instant or a datetime" x)]))

  (define (zone-offset-at z x)
    "The UTC offset of zone Z, in seconds east, at the instant (or datetime) X."
    (let-values ([(off dst? abbr) (zone-lookup (zone-of 'zone-offset-at z) (instant-secs 'zone-offset-at x))])
      off))

  (define (zone-abbreviation-at z x)
    (let-values ([(off dst? abbr) (zone-lookup (zone-of 'zone-abbreviation-at z) (instant-secs 'zone-abbreviation-at x))])
      abbr))

  (define (zone-dst-at? z x)
    (let-values ([(off dst? abbr) (zone-lookup (zone-of 'zone-dst-at? z) (instant-secs 'zone-dst-at? x))])
      dst?))

  ;; The instant of local wall-clock seconds LOCAL (seconds since the epoch as if the zone
  ;; were UTC) in zone Z. In an overlap the earlier instant wins; in a gap the time is moved
  ;; forward by the length of the gap, like a clock that was not set forward.
  (define (local->utc-seconds z local)
    (let* ([off-at (lambda (t) (let-values ([(o d a) (zone-lookup z t)]) o))]
           [o1 (off-at (- local 86400))]
           [o2 (off-at (+ local 86400))])
      (if (= o1 o2)
        (- local o1)
        (let ([v1 (= o1 (off-at (- local o1)))] [v2 (= o2 (off-at (- local o2)))])
          (cond
            [(and v1 v2) (min (- local o1) (- local o2))]
            [v1 (- local o1)]
            [v2 (- local o2)]
            [else (- local o1)])))))

  ;; ======================================================================================
  ;; Datetimes
  ;; ======================================================================================

  (define-record-type (<datetime> %make-datetime datetime?)
    (fields (immutable year datetime-year)
            (immutable month datetime-month)
            (immutable day datetime-day)
            (immutable hour datetime-hour)
            (immutable minute datetime-minute)
            (immutable second datetime-second)
            (immutable nanosecond datetime-nanosecond)
            (immutable offset datetime-offset)
            (immutable zone datetime-zone)))

  (define (check-datetime who x)
    (unless (datetime? x) (assertion-violation who "expected a datetime" x)))

  (define (check-range who what v lo hi)
    (unless (and (integer? v) (exact? v) (<= lo v hi))
      (assertion-violation who (string-append what " out of range") v)))

  (define (validate-civil who y m d h mi s ns)
    (check-integer who y)
    (check-range who "month" m 1 12)
    (check-range who "day" d 1 (days-in-month y m))
    (check-range who "hour" h 0 23)
    (check-range who "minute" mi 0 59)
    (check-range who "second" s 0 59)
    (check-range who "nanosecond" ns 0 (- billion 1)))

  ;; Local seconds (wall clock as if UTC) of civil fields.
  (define (civil->local y m d h mi s)
    (+ (* 86400 (civil->days y m d)) (* 3600 h) (* 60 mi) s))

  ;; A datetime from an instant in seconds/ns and a zone (or #f and a fixed offset).
  (define (secs->datetime secs ns offset zone)
    (let* ([local (+ secs offset)] [days (div local 86400)] [rem (mod local 86400)])
      (let-values ([(y m d) (days->civil days)])
        (%make-datetime y m d (div rem 3600) (div (mod rem 3600) 60) (mod rem 60) ns offset zone))))

  ;; (make-datetime 2024 1 2)  (make-datetime 2024 1 2 15 30 #:offset 3600)
  ;; (make-datetime 2024 3 31 2 30 #:zone (load-zone "Europe/Berlin"))
  ;; Without #:offset or #:zone the datetime is in UTC. With #:zone, a local time that does not
  ;; exist (a DST gap) moves forward by the gap, and an ambiguous one takes the earlier instant.
  (define (make-datetime year month day [hour 0] [minute 0] [second 0] [nanosecond 0]
                         #:offset [offset #f] #:zone [zone #f])
    (validate-civil 'make-datetime year month day hour minute second nanosecond)
    (cond
      [zone
       (let* ([z (zone-of 'make-datetime zone)]
              [secs (local->utc-seconds z (civil->local year month day hour minute second))])
         (secs->datetime secs nanosecond (zone-offset-at z (%make-instant secs 0)) z))]
      [else
       (let ([offset (or offset 0)])
         (check-range 'make-datetime "offset" offset -86399 86399)
         (%make-datetime year month day hour minute second nanosecond offset #f))]))

  (define (datetime->instant dt)
    (check-datetime 'datetime->instant dt)
    (%make-instant (- (civil->local (datetime-year dt) (datetime-month dt) (datetime-day dt)
                                    (datetime-hour dt) (datetime-minute dt) (datetime-second dt))
                      (datetime-offset dt))
                   (datetime-nanosecond dt)))

  ;; ZONE is a zone, an offset in seconds, a zone name, or 'local / 'utc. The default is UTC.
  (define (instant->datetime i [zone utc])
    (check-instant 'instant->datetime i)
    (let* ([z (zone-of 'instant->datetime zone)]
           [fixed (zone-fixed-offset z)])
      (secs->datetime (instant-seconds i) (instant-nanoseconds i)
                      (or fixed (zone-offset-at z i))
                      (and (not (eq? z utc)) (not (and (integer? zone) fixed)) z))))

  (define (datetime-with-zone dt zone)
    "The same instant as DT, in ZONE."
    (instant->datetime (datetime->instant dt) zone))

  (define (datetime-with-offset dt offset)
    "The same instant as DT, at the fixed OFFSET (seconds east of UTC)."
    (check-integer 'datetime-with-offset offset)
    (let ([i (datetime->instant dt)])
      (secs->datetime (instant-seconds i) (instant-nanoseconds i) offset #f)))

  (define (current-datetime [zone 'local])
    "The current date and time in ZONE (the local zone by default)."
    (instant->datetime (now) zone))

  (define (today [zone 'local])
    "Midnight at the start of the current day in ZONE."
    (datetime-truncate (current-datetime zone) 'day))

  (define (datetime-days dt)
    "The day number of DT's civil date: days since 1970-01-01."
    (civil->days (datetime-year dt) (datetime-month dt) (datetime-day dt)))

  (define (datetime-weekday dt)
    "0 for Sunday through 6 for Saturday."
    (days->weekday (datetime-days dt)))

  (define (datetime-day-of-year dt) (year-day (datetime-year dt) (datetime-month dt) (datetime-day dt)))

  (define (datetime-iso-week dt)
    "The ISO 8601 week date: (values week-year week weekday), weekday 1 = Monday."
    (iso-week-date (datetime-days dt)))

  (define (datetime-abbreviation dt)
    "The zone abbreviation of DT (\"CET\", \"EDT\"), or the offset as +hh:mm without a zone."
    (cond
      [(datetime-zone dt) (zone-abbreviation-at (datetime-zone dt) (datetime->instant dt))]
      [(zero? (datetime-offset dt)) "UTC"]
      [else (offset->string (datetime-offset dt) #t)]))

  ;; Rebuild a datetime from local fields, in DT's zone if it has one, else at its offset.
  (define (relocal dt y m d h mi s ns)
    (if (datetime-zone dt)
      (let* ([z (datetime-zone dt)] [secs (local->utc-seconds z (civil->local y m d h mi s))])
        (secs->datetime secs ns (zone-offset-at z (%make-instant secs 0)) z))
      (%make-datetime y m d h mi s ns (datetime-offset dt) #f)))

  (define (datetime+ dt d)
    "DT moved by the exact duration D, keeping its zone (or offset)."
    (check-datetime 'datetime+ dt)
    (let ([i (instant+ (datetime->instant dt) d)])
      (if (datetime-zone dt)
        (instant->datetime i (datetime-zone dt))
        (secs->datetime (instant-seconds i) (instant-nanoseconds i) (datetime-offset dt) #f))))

  ;; (datetime- a b): the duration from B to A. (datetime- a d): A moved back by D.
  (define (datetime- a b)
    (check-datetime 'datetime- a)
    (cond
      [(datetime? b) (instant- (datetime->instant a) (datetime->instant b))]
      [(duration? b) (datetime+ a (duration-negate b))]
      [else (assertion-violation 'datetime- "expected a datetime or a duration" b)]))

  ;; Calendar arithmetic. Years and months move the date and clamp the day to the end of the
  ;; month (Jan 31 + 1 month = Feb 29 in a leap year); weeks and days move the date keeping the
  ;; wall-clock time, even across a DST change; hours and smaller units are exact durations.
  (define (datetime-add dt #:years [years 0] #:months [months 0] #:weeks [weeks 0] #:days [days 0]
                        #:hours [hours 0] #:minutes [minutes 0] #:seconds [seconds 0]
                        #:nanoseconds [nanoseconds 0])
    (check-datetime 'datetime-add dt)
    (for-each (lambda (v) (check-integer 'datetime-add v))
              (list years months weeks days hours minutes seconds nanoseconds))
    (let* ([mi (+ (* 12 (datetime-year dt)) (- (datetime-month dt) 1) (* 12 years) months)]
           [y (div mi 12)] [m (+ 1 (mod mi 12))]
           [d (min (datetime-day dt) (days-in-month y m))]
           [z (+ (civil->days y m d) (* 7 weeks) days)])
      (let-values ([(y m d) (days->civil z)])
        (let ([moved (if (and (zero? years) (zero? months) (zero? weeks) (zero? days))
                       dt
                       (relocal dt y m d (datetime-hour dt) (datetime-minute dt)
                                (datetime-second dt) (datetime-nanosecond dt)))])
          (if (and (zero? hours) (zero? minutes) (zero? seconds) (zero? nanoseconds))
            moved
            (datetime+ moved (make-duration (+ (* 3600 hours) (* 60 minutes) seconds) nanoseconds)))))))

  (define (datetime-truncate dt unit)
    "DT with the fields below UNIT (year, month, week, day, hour, minute or second) reset.
A week starts on Monday."
    (check-datetime 'datetime-truncate dt)
    (let ([y (datetime-year dt)] [m (datetime-month dt)] [d (datetime-day dt)]
          [h (datetime-hour dt)] [mi (datetime-minute dt)] [s (datetime-second dt)])
      (case unit
        [(year) (relocal dt y 1 1 0 0 0 0)]
        [(month) (relocal dt y m 1 0 0 0 0)]
        [(week)
         (let-values ([(wy wk wd) (iso-week-date (datetime-days dt))])
           (let-values ([(y m d) (days->civil (- (datetime-days dt) (- wd 1)))])
             (relocal dt y m d 0 0 0 0)))]
        [(day) (relocal dt y m d 0 0 0 0)]
        [(hour) (relocal dt y m d h 0 0 0)]
        [(minute) (relocal dt y m d h mi 0 0)]
        [(second) (relocal dt y m d h mi s 0)]
        [else (assertion-violation 'datetime-truncate "unknown unit" unit)])))

  (define (days-between a b)
    "The number of days from A's civil date to B's, ignoring the time of day."
    (- (datetime-days b) (datetime-days a)))

  (define (datetime-compare a b)
    (instant-compare (datetime->instant a) (datetime->instant b)))
  (define (datetime=? a b) (= 0 (datetime-compare a b)))
  (define (datetime<? a b) (= -1 (datetime-compare a b)))
  (define (datetime<=? a b) (<= (datetime-compare a b) 0))
  (define (datetime>? a b) (= 1 (datetime-compare a b)))
  (define (datetime>=? a b) (>= (datetime-compare a b) 0))

  ;; ======================================================================================
  ;; Formatting
  ;; ======================================================================================

  (define (pad n width [ch #\0])
    (let ([s (number->string n)])
      (if (>= (string-length s) width) s (string-append (make-string (- width (string-length s)) ch) s))))

  (define (year->string y)
    (cond
      [(<= 0 y 9999) (pad y 4)]
      [(negative? y) (string-append "-" (pad (- y) 4))]
      [else (string-append "+" (number->string y))]))

  ;; Fraction digits: #f means automatic (none, 3, 6 or 9 as needed).
  (define (fraction ns digits)
    (let ([digits (or digits
                      (cond [(zero? ns) 0] [(zero? (mod ns 1000000)) 3] [(zero? (mod ns 1000)) 6] [else 9]))])
      (if (zero? digits)
        ""
        (string-append "." (substring (pad ns 9) 0 (min digits 9))
                       (if (> digits 9) (make-string (- digits 9) #\0) "")))))

  (define (as-datetime who x)
    (cond
      [(datetime? x) x]
      [(instant? x) (instant->datetime x utc)]
      [else (assertion-violation who "expected an instant or a datetime" x)]))

  (define (format-rfc3339 x #:digits [digits #f] #:z? [z? #t])
    "X (an instant or a datetime) as an RFC 3339 timestamp, \"2024-01-02T03:04:05.25+01:00\".
#:digits fixes the number of fraction digits (default: as many as needed, in groups of 3).
An offset of zero is written Z unless #:z? is #f."
    (let ([dt (as-datetime 'format-rfc3339 x)])
      (string-append
        (year->string (datetime-year dt)) "-" (pad (datetime-month dt) 2) "-" (pad (datetime-day dt) 2)
        "T" (pad (datetime-hour dt) 2) ":" (pad (datetime-minute dt) 2) ":" (pad (datetime-second dt) 2)
        (fraction (datetime-nanosecond dt) digits)
        (if (and z? (zero? (datetime-offset dt))) "Z" (offset->string (datetime-offset dt) #t)))))

  (define (format-iso8601 x #:basic? [basic? #f] #:digits [digits #f] #:date-only? [date-only? #f])
    "X as ISO 8601: the extended format of RFC 3339 by default, \"20240102T030405Z\" with
#:basic? #t, only the date with #:date-only? #t."
    (let ([dt (as-datetime 'format-iso8601 x)])
      (cond
        [date-only?
         (string-append (year->string (datetime-year dt)) (if basic? "" "-") (pad (datetime-month dt) 2)
                        (if basic? "" "-") (pad (datetime-day dt) 2))]
        [basic?
         (string-append (year->string (datetime-year dt)) (pad (datetime-month dt) 2) (pad (datetime-day dt) 2)
                        "T" (pad (datetime-hour dt) 2) (pad (datetime-minute dt) 2) (pad (datetime-second dt) 2)
                        (fraction (datetime-nanosecond dt) digits)
                        (if (zero? (datetime-offset dt)) "Z" (offset->string (datetime-offset dt) #f)))]
        [else (format-rfc3339 dt #:digits digits)])))

  (define (format-http-date x)
    "X as an HTTP date (RFC 9110 IMF-fixdate), always in GMT: \"Sun, 06 Nov 1994 08:49:37 GMT\"."
    (let ([dt (instant->datetime (cond [(instant? x) x]
                                       [(datetime? x) (datetime->instant x)]
                                       [(real? x) (seconds->instant x)]
                                       [else (assertion-violation 'format-http-date "expected an instant" x)])
                                 utc)])
      (string-append (weekday-name (datetime-weekday dt) #:short? #t) ", " (pad (datetime-day dt) 2) " "
                     (month-name (datetime-month dt) #:short? #t) " " (pad (datetime-year dt) 4) " "
                     (pad (datetime-hour dt) 2) ":" (pad (datetime-minute dt) 2) ":"
                     (pad (datetime-second dt) 2) " GMT")))

  ;; strftime. Conversions:
  ;;   %Y year  %C century  %y 2-digit year  %G %g ISO week year  %m month  %B %b %h month name
  ;;   %d day  %e day space-padded  %j day of year  %H %k hour (00-23)  %I %l hour (01-12)
  ;;   %M minute  %S second  %f microseconds  %N nanoseconds (%3N: milliseconds, %6N, ...)
  ;;   %p AM/PM  %P am/pm  %A %a weekday name  %u weekday 1-7 (Monday 1)  %w weekday 0-6
  ;;   %U %W week of year (Sunday/Monday first)  %V ISO week  %z +hhmm  %:z +hh:mm  %Z zone
  ;;   %s seconds since the epoch  %F %Y-%m-%d  %T %H:%M:%S  %R %H:%M  %D %m/%d/%y
  ;;   %r %I:%M:%S %p  %c %a %b %e %H:%M:%S %Y  %x %m/%d/%y  %X %H:%M:%S  %n %t %%
  ;; Flags after %: - no padding, _ pad with spaces, 0 pad with zeros, ^ upper case.
  (define (format-datetime x fmt)
    (let ([dt (as-datetime 'format-datetime x)] [n (string-length fmt)])
      (call-with-string-output-port
        (lambda (out)
          (let loop ([i 0])
            (when (< i n)
              (let ([c (string-ref fmt i)])
                (if (or (not (char=? c #\%)) (= (+ i 1) n))
                  (begin (put-char out c) (loop (+ i 1)))
                  ;; flags, width, colon, conversion
                  (let flags ([j (+ i 1)] [padc #f] [upper? #f])
                    (let ([f (string-ref fmt j)])
                      (cond
                        [(and (memv f '(#\- #\_ #\0 #\^)) (< (+ j 1) n))
                         (case f
                           [(#\-) (flags (+ j 1) 'none upper?)]
                           [(#\_) (flags (+ j 1) #\space upper?)]
                           [(#\0) (flags (+ j 1) #\0 upper?)]
                           [else (flags (+ j 1) padc #t)])]
                        [else
                         (let width ([k j] [w #f])
                           (let ([g (string-ref fmt k)])
                             (if (and (char-numeric? g) (< (+ k 1) n))
                               (width (+ k 1) (+ (* 10 (or w 0)) (- (char->integer g) 48)))
                               (let*-values ([(colon? k) (if (and (char=? g #\:) (< (+ k 1) n))
                                                           (values #t (+ k 1))
                                                           (values #f k))])
                                 (let ([s (conversion dt (string-ref fmt k) padc w colon?)])
                                   (put-string out (if upper? (string-upcase s) s))
                                   (loop (+ k 1)))))))])))))))))))

  (define (conversion dt c padc width colon?)
    (define (num v w [default #\0])
      (case padc
        [(none) (number->string v)]
        [(#f) (pad v w default)]
        [else (pad v w padc)]))
    (define (hour12 h) (let ([h (mod h 12)]) (if (= h 0) 12 h)))
    (let ([y (datetime-year dt)] [m (datetime-month dt)] [d (datetime-day dt)]
          [h (datetime-hour dt)] [mi (datetime-minute dt)] [s (datetime-second dt)]
          [ns (datetime-nanosecond dt)] [wd (datetime-weekday dt)])
      (define (sub fmt) (format-datetime dt fmt))
      (case c
        [(#\Y) (if (eq? padc 'none) (number->string y) (year->string y))]
        [(#\C) (num (div y 100) 2)]
        [(#\y) (num (mod y 100) 2)]
        [(#\G) (let-values ([(wy wk wwd) (datetime-iso-week dt)]) (year->string wy))]
        [(#\g) (let-values ([(wy wk wwd) (datetime-iso-week dt)]) (num (mod wy 100) 2))]
        [(#\m) (num m 2)]
        [(#\B) (month-name m)]
        [(#\b #\h) (month-name m #:short? #t)]
        [(#\d) (num d 2)]
        [(#\e) (num d 2 #\space)]
        [(#\j) (num (datetime-day-of-year dt) 3)]
        [(#\H) (num h 2)]
        [(#\k) (num h 2 #\space)]
        [(#\I) (num (hour12 h) 2)]
        [(#\l) (num (hour12 h) 2 #\space)]
        [(#\M) (num mi 2)]
        [(#\S) (num s 2)]
        [(#\f) (pad (div ns 1000) 6)]
        [(#\N) (let ([w (or width 9)])
                 (if (<= w 9) (substring (pad ns 9) 0 w) (string-append (pad ns 9) (make-string (- w 9) #\0))))]
        [(#\p) (if (< h 12) "AM" "PM")]
        [(#\P) (if (< h 12) "am" "pm")]
        [(#\A) (weekday-name wd)]
        [(#\a) (weekday-name wd #:short? #t)]
        [(#\u) (number->string (if (= wd 0) 7 wd))]
        [(#\w) (number->string wd)]
        [(#\U) (num (div (+ (datetime-day-of-year dt) 6 (- wd)) 7) 2)]
        [(#\W) (num (div (+ (datetime-day-of-year dt) 6 (- (mod (+ wd 6) 7))) 7) 2)]
        [(#\V) (let-values ([(wy wk wwd) (datetime-iso-week dt)]) (num wk 2))]
        [(#\z) (offset->string (datetime-offset dt) colon?)]
        [(#\Z) (datetime-abbreviation dt)]
        [(#\s) (number->string (instant-seconds (datetime->instant dt)))]
        [(#\F) (sub "%Y-%m-%d")]
        [(#\T) (sub "%H:%M:%S")]
        [(#\R) (sub "%H:%M")]
        [(#\D #\x) (sub "%m/%d/%y")]
        [(#\r) (sub "%I:%M:%S %p")]
        [(#\c) (sub "%a %b %e %H:%M:%S %Y")]
        [(#\X) (sub "%H:%M:%S")]
        [(#\n) "\n"]
        [(#\t) "\t"]
        [(#\%) "%"]
        [else (assertion-violation 'format-datetime "unknown conversion" (string #\% c))])))

  ;; ======================================================================================
  ;; Parsing
  ;; ======================================================================================

  (define-condition-type &time-parse-error &error
    make-time-parse-error time-parse-error?
    (input time-parse-error-input))

  (define (raise-parse-error who message input)
    (raise (condition (make-time-parse-error input) (make-who-condition who)
                      (make-message-condition message) (make-irritants-condition (list input)))))

  ;; A scanner over STR: procedures that read pieces and escape to FAIL on a mismatch.
  (define (digit-value c) (and (char<=? #\0 c #\9) (- (char->integer c) 48)))

  ;; Parse the run of digits at i (between lo and hi of them). Returns (values n next) or #f.
  (define (read-digits str i lo hi)
    (let ([n (string-length str)])
      (let loop ([j i] [v 0])
        (if (and (< j n) (< (- j i) hi) (digit-value (string-ref str j)))
          (loop (+ j 1) (+ (* 10 v) (digit-value (string-ref str j))))
          (if (>= (- j i) lo) (values v j) (values #f i))))))

  ;; A fraction ".123" or ",123" at i: (values nanoseconds next).
  (define (read-fraction str i)
    (let ([n (string-length str)])
      (if (and (< i n) (memv (string-ref str i) '(#\. #\,))
               (< (+ i 1) n) (digit-value (string-ref str (+ i 1))))
        (let loop ([j (+ i 1)] [v 0] [k 0])
          (if (and (< j n) (digit-value (string-ref str j)))
            (if (< k 9)
              (loop (+ j 1) (+ (* 10 v) (digit-value (string-ref str j))) (+ k 1))
              (loop (+ j 1) v k))
            (values (* v (expt 10 (- 9 k))) j)))
        (values 0 i))))

  ;; Build a datetime from parsed fields, allowing second 60 (a leap second, which becomes the
  ;; first second of the next minute) and hour 24:00:00 (the end of the day).
  (define (build who input y m d h mi s ns offset)
    (unless (and (<= 1 m 12) (<= 1 d (days-in-month y m)) (<= 0 h 24) (<= 0 mi 59) (<= 0 s 60)
                 (or (< h 24) (and (= mi 0) (= s 0) (= ns 0)))
                 (< (abs offset) 86400))
      (raise-parse-error who "field out of range" input))
    (cond
      [(= h 24) (datetime-add (%make-datetime y m d 0 0 0 0 offset #f) #:days 1)]
      [(= s 60) (datetime+ (%make-datetime y m d h mi 59 ns offset #f) (make-duration 1))]
      [else (%make-datetime y m d h mi s ns offset #f)]))

  ;; A UTC offset at i: "Z", "+hh:mm", "+hhmm", "+hh". Returns (values offset next) or (values #f i).
  (define (read-offset str i colon-required?)
    (let ([n (string-length str)])
      (cond
        [(>= i n) (values #f i)]
        [(memv (string-ref str i) '(#\Z #\z)) (values 0 (+ i 1))]
        [(memv (string-ref str i) '(#\+ #\- #\x2212))
         (let ([sign (if (char=? (string-ref str i) #\+) 1 -1)])
           (let-values ([(hh j) (read-digits str (+ i 1) 2 2)])
             (cond
               [(not hh) (values #f i)]
               [(and (< j n) (char=? (string-ref str j) #\:))
                (let-values ([(mm k) (read-digits str (+ j 1) 2 2)])
                  (if (and mm (< mm 60)) (values (* sign (+ (* 3600 hh) (* 60 mm))) k) (values #f i)))]
               [colon-required? (values #f i)]
               [else
                (let-values ([(mm k) (read-digits str j 2 2)])
                  (if mm
                    (if (< mm 60) (values (* sign (+ (* 3600 hh) (* 60 mm))) k) (values #f i))
                    (values (* sign 3600 hh) j)))])))]
        [else (values #f i)])))

  (define (parse-rfc3339 str)
    "Parse an RFC 3339 timestamp such as \"2024-01-02T03:04:05.5+01:00\" into a datetime.
The offset is required; \"t\", \"z\" and a space for the T are accepted. Raises a
&time-parse-error on malformed input."
    (define (fail) (raise-parse-error 'parse-rfc3339 "invalid RFC 3339 timestamp" str))
    (unless (string? str) (assertion-violation 'parse-rfc3339 "expected a string" str))
    (let ([n (string-length str)])
      (define (expect i c) (unless (and (< i n) (char=? (string-ref str i) c)) (fail)) (+ i 1))
      (let*-values ([(y i) (read-digits str 0 4 4)] [(_) (unless y (fail))]
                    [(i) (expect i #\-)] [(m i) (read-digits str i 2 2)] [(_) (unless m (fail))]
                    [(i) (expect i #\-)] [(d i) (read-digits str i 2 2)] [(_) (unless d (fail))]
                    [(_) (unless (and (< i n) (memv (string-ref str i) '(#\T #\t #\space))) (fail))]
                    [(h i) (read-digits str (+ i 1) 2 2)] [(_) (unless h (fail))]
                    [(i) (expect i #\:)] [(mi i) (read-digits str i 2 2)] [(_) (unless mi (fail))]
                    [(i) (expect i #\:)] [(s i) (read-digits str i 2 2)] [(_) (unless s (fail))]
                    [(ns i) (if (and (< i n) (char=? (string-ref str i) #\.)) (read-fraction str i) (values 0 i))]
                    [(off i) (read-offset str i #t)])
        (unless (and off (= i n) (< h 24)) (fail))
        (build 'parse-rfc3339 str y m d h mi s ns off))))

  ;; ISO 8601: calendar dates (2024-01-02, 20240102), ordinal dates (2024-002, 2024002), week
  ;; dates (2024-W01-2, 2024W012), each optionally followed by T (or a space) and a time
  ;; (hh, hh:mm, hh:mm:ss, hhmm, hhmmss, with a fraction on the last part) and an offset
  ;; (Z, +hh, +hh:mm, +hhmm). Without an offset the time is at #:offset, 0 by default.
  (define (parse-iso8601 str #:offset [default-offset 0])
    (define (fail) (raise-parse-error 'parse-iso8601 "invalid ISO 8601 date or time" str))
    (unless (string? str) (assertion-violation 'parse-iso8601 "expected a string" str))
    (let ([n (string-length str)])
      (define (at? i c) (and (< i n) (char=? (string-ref str i) c)))
      (define (digits i k) (let-values ([(v j) (read-digits str i k k)]) (unless v (fail)) (values v j)))
      ;; The date part: (values days next).
      (define (date-part)
        (let*-values ([(sign i) (cond [(at? 0 #\+) (values 1 1)] [(at? 0 #\-) (values -1 1)] [else (values 1 0)])]
                      [(y i) (if (= i 0) (digits i 4)
                                 (let-values ([(v j) (read-digits str i 4 9)]) (unless v (fail)) (values v j)))]
                      [(y) (* sign y)])
          (cond
            [(at? i #\-)
             (cond
               [(at? (+ i 1) #\W)
                (let*-values ([(w j) (digits (+ i 2) 2)])
                  (if (at? j #\-)
                    (let-values ([(wd k) (digits (+ j 1) 1)]) (values (week y w wd) k))
                    (values (week y w 1) j)))]
               [else
                (let-values ([(v j) (read-digits str (+ i 1) 2 3)])
                  (cond
                    [(not v) (fail)]
                    [(= (- j i 1) 3) (values (ordinal y v) j)]
                    [(at? j #\-) (let-values ([(d k) (digits (+ j 1) 2)]) (values (calendar y v d) k))]
                    [else (values (calendar y v 1) j)]))])]
            [(at? i #\W)
             (let*-values ([(w j) (digits (+ i 1) 2)])
               (if (and (< j n) (digit-value (string-ref str j)))
                 (let-values ([(wd k) (digits j 1)]) (values (week y w wd) k))
                 (values (week y w 1) j)))]
            [else
             (let-values ([(v j) (read-digits str i 3 4)])
               (cond
                 [(not v) (values (calendar y 1 1) i)]
                 [(= (- j i) 3) (values (ordinal y v) j)]
                 [else (values (calendar y (div v 100) (mod v 100)) j)]))])))
      (define (calendar y m d)
        (unless (and (<= 1 m 12) (<= 1 d (days-in-month y m))) (fail))
        (civil->days y m d))
      (define (ordinal y k)
        (unless (<= 1 k (days-in-year y)) (fail))
        (ordinal->days y k))
      (define (week y w wd)
        (unless (and (<= 1 w 53) (<= 1 wd 7)) (fail))
        (let ([z (iso-week->days y w wd)])
          (let-values ([(wy wk wwd) (iso-week-date z)]) (unless (= wy y) (fail)))
          z))
      ;; The time part at i: (values h m s ns next), the fraction applying to the last field.
      (define (time-part i)
        (let*-values ([(h i) (digits i 2)])
          (cond
            [(at? i #\:)
             (let*-values ([(mi i) (digits (+ i 1) 2)])
               (if (at? i #\:)
                 (let*-values ([(s i) (digits (+ i 1) 2)] [(ns i) (read-fraction str i)])
                   (values h mi s ns i))
                 (let-values ([(f i) (read-fraction str i)])
                   (values h mi (div (* f 60) billion) (mod (* f 60) billion) i))))]
            [(and (< i n) (digit-value (string-ref str i)))
             (let*-values ([(mi i) (digits i 2)])
               (if (and (< i n) (digit-value (string-ref str i)))
                 (let*-values ([(s i) (digits i 2)] [(ns i) (read-fraction str i)])
                   (values h mi s ns i))
                 (let-values ([(f i) (read-fraction str i)])
                   (values h mi (div (* f 60) billion) (mod (* f 60) billion) i))))]
            [else
             (let-values ([(f i) (read-fraction str i)])
               (let ([secs (* f 3600)])
                 (values h (div secs (* 60 billion)) (mod (div secs billion) 60) (mod secs billion) i)))])))
      (let-values ([(days i) (date-part)])
        (let-values ([(y m d) (days->civil days)])
          (if (= i n)
            (build 'parse-iso8601 str y m d 0 0 0 0 default-offset)
            (begin
              (unless (memv (string-ref str i) '(#\T #\t #\space)) (fail))
              (let*-values ([(h mi s ns j) (time-part (+ i 1))]
                            [(off k) (read-offset str j #f)])
                (unless (= k n) (fail))
                (build 'parse-iso8601 str y m d h mi s ns (or off default-offset)))))))))

  (define (month-index str i)
    ;; A three-letter English month name at i, any case: (values month next) or (values #f i).
    (let ([n (string-length str)])
      (if (> (+ i 3) n)
        (values #f i)
        (let ([s (string-downcase (substring str i (+ i 3)))])
          (let loop ([k 0])
            (cond
              [(= k 12) (values #f i)]
              [(string=? s (string-downcase (abbrev (vector-ref month-names k)))) (values (+ k 1) (+ i 3))]
              [else (loop (+ k 1))]))))))

  (define (parse-http-date str)
    "Parse an HTTP date into an instant, or return #f if STR is not one. Accepts the IMF-fixdate
of RFC 9110 (\"Sun, 06 Nov 1994 08:49:37 GMT\") and the obsolete RFC 850
(\"Sunday, 06-Nov-94 08:49:37 GMT\") and asctime (\"Sun Nov  6 08:49:37 1994\") forms."
    (and (string? str)
         (let* ([s (string-trim str)] [n (string-length s)])
           (define (at? i c) (and (< i n) (char=? (string-ref s i) c)))
           (define (skip-name i)
             (let loop ([j i]) (if (and (< j n) (char-alphabetic? (string-ref s j))) (loop (+ j 1)) j)))
           (define (hms i k)
             ;; "hh:mm:ss" at i, then call k with h m s next.
             (let*-values ([(h i) (read-digits s i 2 2)])
               (and h (at? i #\:)
                    (let*-values ([(m i) (read-digits s (+ i 1) 2 2)])
                      (and m (at? i #\:)
                           (let-values ([(sec i) (read-digits s (+ i 1) 2 2)])
                             (and sec (k h m sec i))))))))
           (define (make y mo d h mi sec)
             (and (<= 1 d (days-in-month y mo)) (<= h 23) (<= mi 59) (<= sec 60)
                  (%make-instant (+ (civil->local y mo d h mi 0) (min sec 59)) 0)))
           (let ([w (skip-name 0)])
             (cond
               [(= w 0) #f]
               ;; IMF-fixdate and RFC 850 both have a comma after the day name.
               [(at? w #\,)
                (let-values ([(d i) (read-digits s (+ w 2) 1 2)])
                  (and d (at? (+ w 1) #\space)
                       (cond
                         [(at? i #\space)
                          (let*-values ([(mo i) (month-index s (+ i 1))])
                            (and mo (at? i #\space)
                                 (let-values ([(y i) (read-digits s (+ i 1) 4 4)])
                                   (and y (at? i #\space)
                                        (hms (+ i 1)
                                             (lambda (h mi sec i)
                                               (and (= i (- n 4)) (string=? (substring s i n) " GMT")
                                                    (make y mo d h mi sec))))))))]
                         [(at? i #\-)
                          (let*-values ([(mo i) (month-index s (+ i 1))])
                            (and mo (at? i #\-)
                                 (let-values ([(yy i) (read-digits s (+ i 1) 2 2)])
                                   (and yy (at? i #\space)
                                        (hms (+ i 1)
                                             (lambda (h mi sec i)
                                               (and (= i (- n 4)) (string=? (substring s i n) " GMT")
                                                    ;; RFC 9110 5.6.7: a two-digit year more than
                                                    ;; 50 years in the future is in the past century.
                                                    (let* ([this (datetime-year (instant->datetime (now)))]
                                                           [y (+ yy (* 100 (div this 100)))]
                                                           [y (if (> y (+ this 50)) (- y 100) y)])
                                                      (make y mo d h mi sec)))))))))]
                         [else #f])))]
               ;; asctime: "Sun Nov  6 08:49:37 1994"
               [(at? w #\space)
                (let*-values ([(mo i) (month-index s (+ w 1))])
                  (and mo (at? i #\space)
                       (let*-values ([(i) (if (at? (+ i 1) #\space) (+ i 2) (+ i 1))]
                                     [(d i) (read-digits s i 1 2)])
                         (and d (at? i #\space)
                              (hms (+ i 1)
                                   (lambda (h mi sec i)
                                     (and (at? i #\space)
                                          (let-values ([(y j) (read-digits s (+ i 1) 4 4)])
                                            (and y (= j n) (make y mo d h mi sec))))))))))]
               [else #f])))))

  (define (string-trim s)
    (let* ([n (string-length s)]
           [a (let loop ([i 0]) (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i))]
           [b (let loop ([i n]) (if (and (> i a) (char-whitespace? (string-ref s (- i 1)))) (loop (- i 1)) i))])
      (substring s a b)))

  ;; strptime. The conversions of `format-datetime` that make sense to read back:
  ;;   %Y %y %C %m %d %e %j %H %k %I %l %M %S %f %N %p %P %b %h %B %a %A %u %w %z %:z %Z %s
  ;;   %F %T %R %D %%, and whitespace, which matches any amount of whitespace.
  ;; Names match any case. %Z reads UTC, GMT, Z or an offset; %a and %A are read and ignored.
  ;; Missing fields default to 1970-01-01T00:00:00 at #:offset (0 by default).
  (define (parse-datetime str fmt #:offset [default-offset 0])
    (define (fail) (raise-parse-error 'parse-datetime (string-append "does not match the format " fmt) str))
    (define n (string-length str))
    (define m (string-length fmt))
    (define year #f) (define century #f) (define yy #f) (define month 1) (define day 1)
    (define yday #f) (define hour 0) (define minute 0) (define sec 0) (define ns 0)
    (define pm #f) (define offset #f) (define epoch #f)
    (define (num i lo hi)
      (let* ([i (if (and (< i n) (char=? (string-ref str i) #\space) (> lo 0)) (+ i 1) i)])
        (let-values ([(v j) (read-digits str i lo hi)]) (unless v (fail)) (values v j))))
    (define (name i names)
      ;; The longest full or three-letter name at i: (values index next).
      (let loop ([k 0] [best #f] [best-end i])
        (if (= k (vector-length names))
          (begin (unless best (fail)) (values best best-end))
          (let* ([full (vector-ref names k)]
                 [try (lambda (s)
                        (let ([e (+ i (string-length s))])
                          (and (<= e n) (string-ci=? s (substring str i e)) e)))]
                 [e (or (try full) (try (abbrev full)))])
            (if (and e (> e best-end)) (loop (+ k 1) k e) (loop (+ k 1) best best-end))))))
    (define (directive c i colon?)
      (case c
        [(#\Y) (let*-values ([(sign i) (if (and (< i n) (memv (string-ref str i) '(#\- #\+)))
                                         (values (if (char=? (string-ref str i) #\-) -1 1) (+ i 1))
                                         (values 1 i))]
                             [(v j) (num i 1 9)])
                 (set! year (* sign v)) j)]
        [(#\y) (let-values ([(v j) (num i 2 2)]) (set! yy v) j)]
        [(#\C) (let-values ([(v j) (num i 1 2)]) (set! century v) j)]
        [(#\m) (let-values ([(v j) (num i 1 2)]) (set! month v) j)]
        [(#\d #\e) (let-values ([(v j) (num i 1 2)]) (set! day v) j)]
        [(#\j) (let-values ([(v j) (num i 1 3)]) (set! yday v) j)]
        [(#\H #\k #\I #\l) (let-values ([(v j) (num i 1 2)]) (set! hour v) j)]
        [(#\M) (let-values ([(v j) (num i 1 2)]) (set! minute v) j)]
        [(#\S) (let-values ([(v j) (num i 1 2)]) (set! sec v) j)]
        [(#\f #\N) (unless (and (< i n) (digit-value (string-ref str i))) (fail))
                   (let loop ([k i] [v 0] [count 0])
                     (if (and (< k n) (digit-value (string-ref str k)))
                       (if (< count 9)
                         (loop (+ k 1) (+ (* 10 v) (digit-value (string-ref str k))) (+ count 1))
                         (loop (+ k 1) v count))
                       (begin (set! ns (* v (expt 10 (- 9 count)))) k)))]
        [(#\p #\P) (cond
                     [(and (<= (+ i 2) n) (string-ci=? (substring str i (+ i 2)) "AM")) (set! pm 'am) (+ i 2)]
                     [(and (<= (+ i 2) n) (string-ci=? (substring str i (+ i 2)) "PM")) (set! pm 'pm) (+ i 2)]
                     [else (fail)])]
        [(#\b #\h #\B) (let-values ([(k j) (name i month-names)]) (set! month (+ k 1)) j)]
        [(#\a #\A) (let-values ([(k j) (name i weekday-names)]) j)]
        [(#\u #\w) (let-values ([(v j) (num i 1 1)]) j)]
        [(#\z) (let-values ([(off j) (read-offset str i colon?)]) (unless off (fail)) (set! offset off) j)]
        [(#\Z) (cond
                 [(and (<= (+ i 3) n) (member (string-upcase (substring str i (+ i 3))) '("UTC" "GMT")))
                  (set! offset 0) (+ i 3)]
                 [else (let-values ([(off j) (read-offset str i #f)]) (unless off (fail)) (set! offset off) j)])]
        [(#\s) (let*-values ([(sign i) (if (and (< i n) (char=? (string-ref str i) #\-)) (values -1 (+ i 1)) (values 1 i))]
                             [(v j) (num i 1 20)])
                 (set! epoch (* sign v)) j)]
        [(#\F) (sub i "%Y-%m-%d")]
        [(#\T) (sub i "%H:%M:%S")]
        [(#\R) (sub i "%H:%M")]
        [(#\D) (sub i "%m/%d/%y")]
        [(#\%) (unless (and (< i n) (char=? (string-ref str i) #\%)) (fail)) (+ i 1)]
        [else (assertion-violation 'parse-datetime "unknown conversion" (string #\% c))]))
    (define (sub i f) (run i f 0 (string-length f)))
    (define (run i f p e)
      (cond
        [(= p e) i]
        [(char-whitespace? (string-ref f p))
         (run (let skip ([i i]) (if (and (< i n) (char-whitespace? (string-ref str i))) (skip (+ i 1)) i))
              f (+ p 1) e)]
        [(and (char=? (string-ref f p) #\%) (< (+ p 1) e))
         (let*-values ([(p) (let skip ([p (+ p 1)])
                              (if (and (< (+ p 1) e) (or (memv (string-ref f p) '(#\- #\_ #\0 #\^))
                                                         (char-numeric? (string-ref f p))))
                                (skip (+ p 1)) p))]
                       [(colon? p) (if (and (char=? (string-ref f p) #\:) (< (+ p 1) e)) (values #t (+ p 1)) (values #f p))])
           (run (directive (string-ref f p) i colon?) f (+ p 1) e))]
        [(and (< i n) (char=? (string-ref str i) (string-ref f p))) (run (+ i 1) f (+ p 1) e)]
        [else (fail)]))
    (unless (= (run 0 fmt 0 m) n) (fail))
    (cond
      [epoch (instant->datetime (%make-instant epoch ns) (or offset default-offset))]
      [else
       (let* ([year (cond [year year]
                          [yy (+ yy (if century (* 100 century) (if (< yy 69) 2000 1900)))]
                          [century (* 100 century)]
                          [else 1970])]
              [hour (case pm [(am) (if (= hour 12) 0 hour)] [(pm) (if (= hour 12) 12 (+ hour 12))] [else hour])])
         (if yday
           (begin
             (unless (<= 1 yday (days-in-year year)) (fail))
             (let-values ([(y mo d) (days->civil (ordinal->days year yday))])
               (build 'parse-datetime str y mo d hour minute sec ns (or offset default-offset))))
           (build 'parse-datetime str year month day hour minute sec ns (or offset default-offset))))]))

  ;; ======================================================================================
  ;; Printing
  ;; ======================================================================================

  (set-rtd-printer! (record-type-descriptor <instant>)
    (lambda (x port quote?)
      (put-string port "#<instant ")
      (put-string port (format-rfc3339 x))
      (put-string port ">")))

  (set-rtd-printer! (record-type-descriptor <duration>)
    (lambda (x port quote?)
      (put-string port "#<duration ")
      (put-string port (duration->string x))
      (put-string port ">")))

  (set-rtd-printer! (record-type-descriptor <datetime>)
    (lambda (x port quote?)
      (put-string port "#<datetime ")
      (put-string port (format-rfc3339 x))
      (when (datetime-zone x)
        (put-string port " ")
        (put-string port (zone-name (datetime-zone x))))
      (put-string port ">")))

  (set-rtd-printer! (record-type-descriptor <zone>)
    (lambda (x port quote?)
      (put-string port "#<zone ")
      (put-string port (zone-name x))
      (put-string port ">"))))
