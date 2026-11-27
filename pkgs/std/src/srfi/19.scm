;; SRFI 19: Time Data Types and Procedures, on top of (capy time).
;;
;;   (import (srfi 19))
;;   (date->string (current-date 0) "~Y-~m-~d ~H:~M:~S~z")   ; => "2026-11-25 10:31:07Z"
;;   (time-difference (current-time) (make-time time-utc 0 0))
;;   (date->julian-day (make-date 0 0 0 12 1 1 2000 0))       ; => 2451545
;;   (string->date "2024-02-29 13:05" "~Y-~m-~d ~H:~M")
;;
;; Times are normalized with 0 <= nanosecond < 10^9; a negative duration has a negative second.
;; TAI and monotonic time differ from UTC by the leap seconds of the table below (TAI - UTC is
;; 10 s in 1972 and 37 s since 2017); before 1972 they are taken as equal. time-monotonic
;; counts like time-tai, as in the reference implementation, so it converts to dates; for an
;; interval clock that never jumps use `monotonic-now` of (capy time). The default time zone
;; offset is the local zone's at the instant converted.
(library (srfi 19)
  (export
    time-duration time-monotonic time-process time-tai time-thread time-utc
    current-date current-julian-day current-modified-julian-day current-time time-resolution
    make-time time? time-type time-nanosecond time-second set-time-type! set-time-nanosecond!
    set-time-second! copy-time
    time<=? time<? time=? time>=? time>?
    time-difference time-difference! add-duration add-duration! subtract-duration subtract-duration!
    make-date date? date-nanosecond date-second date-minute date-hour date-day date-month
    date-year date-zone-offset date-year-day date-week-day date-week-number
    date->julian-day date->modified-julian-day date->time-monotonic date->time-tai date->time-utc
    julian-day->date julian-day->time-monotonic julian-day->time-tai julian-day->time-utc
    modified-julian-day->date modified-julian-day->time-monotonic modified-julian-day->time-tai
    modified-julian-day->time-utc
    time-monotonic->date time-monotonic->julian-day time-monotonic->modified-julian-day
    time-monotonic->time-tai time-monotonic->time-tai! time-monotonic->time-utc
    time-monotonic->time-utc!
    time-tai->date time-tai->julian-day time-tai->modified-julian-day time-tai->time-monotonic
    time-tai->time-monotonic! time-tai->time-utc time-tai->time-utc!
    time-utc->date time-utc->julian-day time-utc->modified-julian-day time-utc->time-monotonic
    time-utc->time-monotonic! time-utc->time-tai time-utc->time-tai!
    date->string string->date)
  (import
    (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (prefix (capy time) t:)
    (capy time civil))

  (define billion 1000000000)

  (define time-duration 'time-duration)
  (define time-monotonic 'time-monotonic)
  (define time-process 'time-process)
  (define time-tai 'time-tai)
  (define time-thread 'time-thread)
  (define time-utc 'time-utc)

  (define time-types '(time-duration time-monotonic time-process time-tai time-thread time-utc))

  ;; ---- Time objects ----------------------------------------------------------------------

  (define-record-type (<time> %make-time time?)
    (fields (mutable type time-type %set-time-type!)
            (mutable nanosecond time-nanosecond %set-time-nanosecond!)
            (mutable second time-second %set-time-second!)))

  (define (check-type who type)
    (unless (memq type time-types) (assertion-violation who "not a time type" type)))

  (define (check-time who t)
    (unless (time? t) (assertion-violation who "not a time object" t)))

  (define (make-time type nanosecond second)
    (check-type 'make-time type)
    (unless (and (integer? nanosecond) (exact? nanosecond) (integer? second) (exact? second))
      (assertion-violation 'make-time "nanosecond and second must be exact integers" nanosecond second))
    (let ([total (+ (* second billion) nanosecond)])
      (%make-time type (mod total billion) (div total billion))))

  (define (time->ns t) (+ (* (time-second t) billion) (time-nanosecond t)))
  (define (ns->time type ns) (%make-time type (mod ns billion) (div ns billion)))

  (define (set-time-type! t type) (check-type 'set-time-type! type) (%set-time-type! t type))
  (define (set-time-nanosecond! t ns)
    (let ([n (+ (* (time-second t) billion) ns)])
      (%set-time-nanosecond! t (mod n billion))
      (%set-time-second! t (div n billion))))
  (define (set-time-second! t s) (%set-time-second! t s))

  (define (copy-time t) (%make-time (time-type t) (time-nanosecond t) (time-second t)))

  (define (current-time [type time-utc])
    (check-type 'current-time type)
    (case type
      [(time-utc) (instant->time (t:now) time-utc)]
      [(time-tai time-monotonic) (time-utc->tai (instant->time (t:now) time-utc) type)]
      [(time-process) (duration->time (t:clock-time 'process) time-process)]
      [(time-thread) (duration->time (t:clock-time 'thread) time-thread)]
      [else (assertion-violation 'current-time "cannot read the current time of this type" type)]))

  (define (time-resolution [type time-utc])
    (check-type 'time-resolution type)
    1)

  (define (instant->time i type) (%make-time type (t:instant-nanoseconds i) (t:instant-seconds i)))
  (define (duration->time d type) (%make-time type (t:duration-nanoseconds d) (t:duration-seconds d)))

  (define (compare who a b)
    (check-time who a)
    (check-time who b)
    (unless (eq? (time-type a) (time-type b))
      (assertion-violation who "times of different types" a b))
    (let ([x (time->ns a)] [y (time->ns b)]) (cond [(< x y) -1] [(> x y) 1] [else 0])))

  (define (time=? a b) (= 0 (compare 'time=? a b)))
  (define (time<? a b) (= -1 (compare 'time<? a b)))
  (define (time<=? a b) (<= (compare 'time<=? a b) 0))
  (define (time>? a b) (= 1 (compare 'time>? a b)))
  (define (time>=? a b) (>= (compare 'time>=? a b) 0))

  (define (time-difference a b)
    (compare 'time-difference a b)
    (ns->time time-duration (- (time->ns a) (time->ns b))))

  (define (assign! t from)
    (%set-time-type! t (time-type from))
    (%set-time-nanosecond! t (time-nanosecond from))
    (%set-time-second! t (time-second from))
    t)

  (define (time-difference! a b) (assign! a (time-difference a b)))

  (define (check-duration who d)
    (check-time who d)
    (unless (eq? (time-type d) time-duration) (assertion-violation who "not a duration" d)))

  (define (add-duration t d)
    (check-time 'add-duration t)
    (check-duration 'add-duration d)
    (ns->time (time-type t) (+ (time->ns t) (time->ns d))))

  (define (subtract-duration t d)
    (check-time 'subtract-duration t)
    (check-duration 'subtract-duration d)
    (ns->time (time-type t) (- (time->ns t) (time->ns d))))

  (define (add-duration! t d) (assign! t (add-duration t d)))
  (define (subtract-duration! t d) (assign! t (subtract-duration t d)))

  ;; ---- Leap seconds ----------------------------------------------------------------------

  ;; (UTC seconds of the day it takes effect . TAI - UTC from then on), newest first.
  (define leap-seconds
    (map (lambda (e) (cons (* 86400 (civil->days (car e) (cadr e) 1)) (caddr e)))
         '((2017 1 37) (2015 7 36) (2012 7 35) (2009 1 34) (2006 1 33) (1999 1 32) (1997 7 31)
           (1996 1 30) (1994 7 29) (1993 7 28) (1992 7 27) (1991 1 26) (1990 1 25) (1988 1 24)
           (1985 7 23) (1983 7 22) (1982 7 21) (1981 7 20) (1980 1 19) (1979 1 18) (1978 1 17)
           (1977 1 16) (1976 1 15) (1975 1 14) (1974 1 13) (1973 1 12) (1972 7 11) (1972 1 10))))

  (define (utc-leap utc-secs)
    (let loop ([table leap-seconds])
      (cond [(null? table) 0]
            [(>= utc-secs (caar table)) (cdar table)]
            [else (loop (cdr table))])))

  ;; TAI - UTC at TAI seconds; during an inserted second the old value still applies.
  (define (tai-leap tai-secs)
    (let loop ([table leap-seconds])
      (cond [(null? table) 0]
            [(>= tai-secs (+ (caar table) (cdar table))) (cdar table)]
            [else (loop (cdr table))])))

  ;; Whether TAI seconds fall in an inserted leap second (shown as 23:59:60).
  (define (tai-in-leap-second? tai-secs)
    (exists (lambda (e) (= tai-secs (+ (car e) (cdr e) -1)))
            (filter (lambda (e) (> (cdr e) 10)) leap-seconds)))

  (define (time-utc->tai t type)
    (%make-time type (time-nanosecond t) (+ (time-second t) (utc-leap (time-second t)))))

  (define (time-tai->utc t)
    (%make-time time-utc (time-nanosecond t) (- (time-second t) (tai-leap (time-second t)))))

  (define (check-kind who t type)
    (check-time who t)
    (unless (eq? (time-type t) type) (assertion-violation who "wrong time type" t type)))

  (define (time-utc->time-tai t) (check-kind 'time-utc->time-tai t time-utc) (time-utc->tai t time-tai))
  (define (time-utc->time-monotonic t)
    (check-kind 'time-utc->time-monotonic t time-utc) (time-utc->tai t time-monotonic))
  (define (time-tai->time-utc t) (check-kind 'time-tai->time-utc t time-tai) (time-tai->utc t))
  (define (time-monotonic->time-utc t)
    (check-kind 'time-monotonic->time-utc t time-monotonic) (time-tai->utc t))
  (define (time-tai->time-monotonic t)
    (check-kind 'time-tai->time-monotonic t time-tai)
    (%make-time time-monotonic (time-nanosecond t) (time-second t)))
  (define (time-monotonic->time-tai t)
    (check-kind 'time-monotonic->time-tai t time-monotonic)
    (%make-time time-tai (time-nanosecond t) (time-second t)))

  (define (time-utc->time-tai! t) (assign! t (time-utc->time-tai t)))
  (define (time-utc->time-monotonic! t) (assign! t (time-utc->time-monotonic t)))
  (define (time-tai->time-utc! t) (assign! t (time-tai->time-utc t)))
  (define (time-monotonic->time-utc! t) (assign! t (time-monotonic->time-utc t)))
  (define (time-tai->time-monotonic! t) (assign! t (time-tai->time-monotonic t)))
  (define (time-monotonic->time-tai! t) (assign! t (time-monotonic->time-tai t)))

  ;; ---- Dates -----------------------------------------------------------------------------

  (define-record-type (<date> %make-date date?)
    (fields (immutable nanosecond date-nanosecond)
            (immutable second date-second)
            (immutable minute date-minute)
            (immutable hour date-hour)
            (immutable day date-day)
            (immutable month date-month)
            (immutable year date-year)
            (immutable zone-offset date-zone-offset)))

  (define (make-date nanosecond second minute hour day month year zone-offset)
    (%make-date nanosecond second minute hour day month year zone-offset))

  (define (local-offset utc-secs) (t:zone-offset-at (t:local-zone) (t:make-instant utc-secs)))

  (define (current-date [tz #f])
    (time-utc->date (current-time time-utc) tz))

  (define (utc-secs->date secs ns tz)
    (let* ([tz (if tz tz (local-offset secs))]
           [local (+ secs tz)] [days (div local 86400)] [rem (mod local 86400)])
      (let-values ([(y m d) (days->civil days)])
        (%make-date ns (mod rem 60) (div (mod rem 3600) 60) (div rem 3600) d m y tz))))

  (define (time-utc->date t [tz #f])
    (check-kind 'time-utc->date t time-utc)
    (utc-secs->date (time-second t) (time-nanosecond t) tz))

  (define (tai->date t tz)
    (let ([s (time-second t)])
      (if (tai-in-leap-second? s)
        ;; 23:59:60 of the day before the new offset.
        (let ([d (utc-secs->date (- s (tai-leap s) 1) (time-nanosecond t) tz)])
          (%make-date (date-nanosecond d) 60 (date-minute d) (date-hour d) (date-day d)
                      (date-month d) (date-year d) (date-zone-offset d)))
        (utc-secs->date (- s (tai-leap s)) (time-nanosecond t) tz))))

  (define (time-tai->date t [tz #f]) (check-kind 'time-tai->date t time-tai) (tai->date t tz))
  (define (time-monotonic->date t [tz #f])
    (check-kind 'time-monotonic->date t time-monotonic) (tai->date t tz))

  (define (check-date who d) (unless (date? d) (assertion-violation who "not a date" d)))

  ;; UTC seconds of a date. Second 60 counts as the next second.
  (define (date-utc-secs d)
    (- (+ (* 86400 (civil->days (date-year d) (date-month d) (date-day d)))
          (* 3600 (date-hour d)) (* 60 (date-minute d)) (date-second d))
       (date-zone-offset d)))

  (define (date->time-utc d)
    (check-date 'date->time-utc d)
    (%make-time time-utc (date-nanosecond d) (date-utc-secs d)))

  (define (date->tai d type)
    (if (= (date-second d) 60)
      ;; The leap second itself: one second before the TAI of the next day.
      (let ([next (date-utc-secs d)])
        (%make-time type (date-nanosecond d) (+ next (utc-leap next) -1)))
      (time-utc->tai (date->time-utc d) type)))

  (define (date->time-tai d) (check-date 'date->time-tai d) (date->tai d time-tai))
  (define (date->time-monotonic d) (check-date 'date->time-monotonic d) (date->tai d time-monotonic))

  (define (date-year-day d) (year-day (date-year d) (date-month d) (date-day d)))
  (define (date-week-day d) (days->weekday (civil->days (date-year d) (date-month d) (date-day d))))

  ;; Weeks start on DAY-OF-WEEK-STARTING-WEEK (0 = Sunday), ignoring the first partial week,
  ;; computed as the reference implementation does.
  (define (date-week-number d day-of-week-starting-week)
    (let* ([first (days->weekday (civil->days (date-year d) 1 1))]
           [before (mod (- day-of-week-starting-week first) 7)])
      (quotient (- (date-year-day d) before) 7)))

  ;; ---- Julian days -----------------------------------------------------------------------

  (define epoch-jd 4881175/2)           ; 2440587.5, the Julian day of 1970-01-01T00:00Z
  (define mjd-offset 4800001/2)         ; JD - MJD

  (define (utc-time->jd t)
    (+ epoch-jd (/ (+ (time-second t) (/ (time-nanosecond t) billion)) 86400)))

  (define (jd->utc-time jd)
    (let ([ns (exact (round (* (- (exact jd) epoch-jd) 86400 billion)))])
      (ns->time time-utc ns)))

  (define (date->julian-day d) (check-date 'date->julian-day d) (utc-time->jd (date->time-utc d)))
  (define (date->modified-julian-day d) (- (date->julian-day d) mjd-offset))

  (define (time-utc->julian-day t) (check-kind 'time-utc->julian-day t time-utc) (utc-time->jd t))
  (define (time-utc->modified-julian-day t) (- (time-utc->julian-day t) mjd-offset))
  (define (time-tai->julian-day t) (check-kind 'time-tai->julian-day t time-tai) (utc-time->jd (time-tai->utc t)))
  (define (time-tai->modified-julian-day t) (- (time-tai->julian-day t) mjd-offset))
  (define (time-monotonic->julian-day t)
    (check-kind 'time-monotonic->julian-day t time-monotonic) (utc-time->jd (time-tai->utc t)))
  (define (time-monotonic->modified-julian-day t) (- (time-monotonic->julian-day t) mjd-offset))

  (define (julian-day->time-utc jd) (jd->utc-time jd))
  (define (julian-day->time-tai jd) (time-utc->tai (jd->utc-time jd) time-tai))
  (define (julian-day->time-monotonic jd) (time-utc->tai (jd->utc-time jd) time-monotonic))
  (define (julian-day->date jd [tz #f]) (time-utc->date (jd->utc-time jd) tz))

  (define (modified-julian-day->time-utc mjd) (julian-day->time-utc (+ (exact mjd) mjd-offset)))
  (define (modified-julian-day->time-tai mjd) (julian-day->time-tai (+ (exact mjd) mjd-offset)))
  (define (modified-julian-day->time-monotonic mjd) (julian-day->time-monotonic (+ (exact mjd) mjd-offset)))
  (define (modified-julian-day->date mjd [tz #f]) (julian-day->date (+ (exact mjd) mjd-offset) tz))

  (define (current-julian-day) (time-utc->julian-day (current-time time-utc)))
  (define (current-modified-julian-day) (time-utc->modified-julian-day (current-time time-utc)))

  ;; ---- date->string ----------------------------------------------------------------------

  (define (pad n width [c #\0])
    (let ([s (number->string n)])
      (if (>= (string-length s) width) s (string-append (make-string (- width (string-length s)) c) s))))

  (define (zone-string offset)
    (if (zero? offset)
      "Z"
      (let ([a (abs offset)])
        (string-append (if (negative? offset) "-" "+") (pad (div a 3600) 2) (pad (div (mod a 3600) 60) 2)))))

  (define (seconds-with-fraction d)
    (let ([ns (date-nanosecond d)])
      (if (zero? ns)
        (pad (date-second d) 2)
        (let* ([f (pad ns 9)]
               [f (let trim ([e 9]) (if (char=? (string-ref f (- e 1)) #\0) (trim (- e 1)) (substring f 0 e)))])
          (string-append (pad (date-second d) 2) "." f)))))

  (define (date->string d [fmt "~c"])
    (check-date 'date->string d)
    (let ([n (string-length fmt)] [wd (date-week-day d)] [h (date-hour d)])
      (define (h12) (let ([x (mod h 12)]) (if (= x 0) 12 x)))
      (call-with-string-output-port
        (lambda (out)
          (define (emit s) (put-string out s))
          (let loop ([i 0])
            (when (< i n)
              (let ([c (string-ref fmt i)])
                (if (or (not (char=? c #\~)) (= (+ i 1) n))
                  (begin (put-char out c) (loop (+ i 1)))
                  (begin
                    (case (string-ref fmt (+ i 1))
                      [(#\~) (emit "~")]
                      [(#\a) (emit (t:weekday-name wd #:short? #t))]
                      [(#\A) (emit (t:weekday-name wd))]
                      [(#\b #\h) (emit (t:month-name (date-month d) #:short? #t))]
                      [(#\B) (emit (t:month-name (date-month d)))]
                      [(#\c) (emit (date->string d "~a ~b ~d ~H:~M:~S~z ~Y"))]
                      [(#\d) (emit (pad (date-day d) 2))]
                      [(#\D #\x) (emit (date->string d "~m/~d/~y"))]
                      [(#\e) (emit (pad (date-day d) 2 #\space))]
                      [(#\f) (emit (seconds-with-fraction d))]
                      [(#\H) (emit (pad h 2))]
                      [(#\I) (emit (pad (h12) 2))]
                      [(#\j) (emit (pad (date-year-day d) 3))]
                      [(#\k) (emit (pad h 2 #\space))]
                      [(#\l) (emit (pad (h12) 2 #\space))]
                      [(#\m) (emit (pad (date-month d) 2))]
                      [(#\M) (emit (pad (date-minute d) 2))]
                      [(#\n) (emit "\n")]
                      [(#\N) (emit (pad (date-nanosecond d) 9))]
                      [(#\p) (emit (if (< h 12) "AM" "PM"))]
                      [(#\r) (emit (date->string d "~I:~M:~S ~p"))]
                      [(#\s) (emit (number->string (time-second (date->time-utc d))))]
                      [(#\S) (emit (pad (date-second d) 2))]
                      [(#\t) (emit "\t")]
                      [(#\T #\X #\3) (emit (date->string d "~H:~M:~S"))]
                      [(#\U) (emit (pad (date-week-number d 0) 2))]
                      [(#\V) (emit (pad (let-values ([(wy wk wwd) (iso-week-date (civil->days (date-year d) (date-month d) (date-day d)))]) wk) 2))]
                      [(#\w) (emit (number->string wd))]
                      [(#\W) (emit (pad (date-week-number d 1) 2))]
                      [(#\y) (emit (pad (mod (date-year d) 100) 2))]
                      [(#\Y) (emit (number->string (date-year d)))]
                      [(#\z) (emit (zone-string (date-zone-offset d)))]
                      [(#\Z) (emit "")]
                      [(#\1) (emit (date->string d "~Y-~m-~d"))]
                      [(#\2) (emit (date->string d "~H:~M:~S~z"))]
                      [(#\4) (emit (date->string d "~Y-~m-~dT~H:~M:~S~z"))]
                      [(#\5) (emit (date->string d "~Y-~m-~dT~H:~M:~S"))]
                      [else (assertion-violation 'date->string "unknown directive" (substring fmt i (+ i 2)))])
                    (loop (+ i 2)))))))))))

  ;; ---- string->date ----------------------------------------------------------------------

  (define (string->date str template)
    (define n (string-length str))
    (define m (string-length template))
    (define (fail) (error 'string->date "string does not match the template" str template))
    (define year #f) (define month #f) (define day #f) (define hour 0) (define minute 0)
    (define sec 0) (define ns 0) (define offset #f)
    (define (digit i) (and (< i n) (char-numeric? (string-ref str i)) (- (char->integer (string-ref str i)) 48)))
    (define (int i max-digits)
      (let loop ([j i] [v 0])
        (if (and (< (- j i) max-digits) (digit j))
          (loop (+ j 1) (+ (* 10 v) (digit j)))
          (begin (when (= j i) (fail)) (values v j)))))
    (define (skip-spaces i) (if (and (< i n) (char=? (string-ref str i) #\space)) (skip-spaces (+ i 1)) i))
    (define (month-name-at i full?)
      (let loop ([k 1])
        (if (> k 12)
          (fail)
          (let* ([name (t:month-name k #:short? (not full?))] [e (+ i (string-length name))])
            (if (and (<= e n) (string-ci=? name (substring str i e))) (values k e) (loop (+ k 1)))))))
    (define (skip-alpha i) (if (and (< i n) (char-alphabetic? (string-ref str i))) (skip-alpha (+ i 1)) i))
    (let loop ([i 0] [p 0])
      (cond
        [(= p m) (unless (= i n) (fail))]
        [(and (char=? (string-ref template p) #\~) (< (+ p 1) m))
         (let ([next
                (case (string-ref template (+ p 1))
                  [(#\~) (unless (and (< i n) (char=? (string-ref str i) #\~)) (fail)) (+ i 1)]
                  [(#\a #\A) (let ([j (skip-alpha i)]) (when (= j i) (fail)) j)]
                  [(#\b #\h) (let-values ([(k j) (month-name-at i #f)]) (set! month k) j)]
                  [(#\B) (let-values ([(k j) (month-name-at i #t)]) (set! month k) j)]
                  [(#\d) (let-values ([(v j) (int i 2)]) (set! day v) j)]
                  [(#\e) (let-values ([(v j) (int (skip-spaces i) 2)]) (set! day v) j)]
                  [(#\H) (let-values ([(v j) (int i 2)]) (set! hour v) j)]
                  [(#\k) (let-values ([(v j) (int (skip-spaces i) 2)]) (set! hour v) j)]
                  [(#\m) (let-values ([(v j) (int i 2)]) (set! month v) j)]
                  [(#\M) (let-values ([(v j) (int i 2)]) (set! minute v) j)]
                  [(#\S) (let-values ([(v j) (int i 2)]) (set! sec v) j)]
                  [(#\f)
                   (let-values ([(v j) (int i 2)])
                     (set! sec v)
                     (if (and (< j n) (char=? (string-ref str j) #\.))
                       (let frac ([k (+ j 1)] [v 0] [count 0])
                         (if (digit k)
                           (frac (+ k 1) (if (< count 9) (+ (* 10 v) (digit k)) v) (min 9 (+ count 1)))
                           (begin (set! ns (* v (expt 10 (- 9 count)))) k)))
                       j))]
                  [(#\N) (let-values ([(v j) (int i 9)]) (set! ns (* v (expt 10 (- 9 (- j i))))) j)]
                  [(#\y)
                   (let-values ([(v j) (int i 2)])
                     ;; The year within 50 years of the current one.
                     (let* ([this (t:datetime-year (t:current-datetime 'utc))]
                            [y (+ v (* 100 (div this 100)))]
                            [y (cond [(> y (+ this 50)) (- y 100)] [(<= y (- this 50)) (+ y 100)] [else y])])
                       (set! year y))
                     j)]
                  [(#\Y)
                   (let*-values ([(sign i) (if (and (< i n) (char=? (string-ref str i) #\-)) (values -1 (+ i 1)) (values 1 i))]
                                 [(v j) (int i 9)])
                     (set! year (* sign v))
                     j)]
                  [(#\z)
                   (cond
                     [(and (< i n) (memv (string-ref str i) '(#\Z #\z))) (set! offset 0) (+ i 1)]
                     [(and (< i n) (memv (string-ref str i) '(#\+ #\-)))
                      (let*-values ([(sign) (if (char=? (string-ref str i) #\+) 1 -1)]
                                    [(hh j) (int (+ i 1) 2)]
                                    [(j) (if (and (< j n) (char=? (string-ref str j) #\:)) (+ j 1) j)]
                                    [(mm k) (int j 2)])
                        (set! offset (* sign (+ (* 3600 hh) (* 60 mm))))
                        k)]
                     [else (fail)])]
                  [else (assertion-violation 'string->date "unknown directive"
                                             (substring template p (+ p 2)))])])
           (loop next (+ p 2)))]
        [(and (< i n) (char=? (string-ref str i) (string-ref template p))) (loop (+ i 1) (+ p 1))]
        [else (fail)]))
    (let* ([year (or year (t:datetime-year (t:current-datetime 'local)))]
           [month (or month 1)]
           [day (or day 1)])
      (unless (and (<= 1 month 12) (<= 1 day (days-in-month year month)) (<= 0 hour 23)
                   (<= 0 minute 59) (<= 0 sec 60))
        (error 'string->date "date field out of range" str))
      (%make-date ns sec minute hour day month year
                  (or offset
                      (local-offset (- (+ (* 86400 (civil->days year month day)) (* 3600 hour) (* 60 minute) sec)
                                       0)))))))
