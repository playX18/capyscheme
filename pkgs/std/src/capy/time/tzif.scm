;; Time zone data: the TZif files of the tz database (RFC 8536) and POSIX TZ rule strings.
;;
;;   (define berlin (read-tzif "/usr/share/zoneinfo/Europe/Berlin"))
;;   (tzdata-lookup berlin 1700000000)          => 3600 #f "CET"  (three values)
;;   (tzdata-lookup (parse-posix-tz "EST5EDT,M3.2.0,M11.1.0") 1690000000)
;;                                              => -14400 #t "EDT"
;;
;; A `tzdata` holds the transitions of the file (the 64-bit section when there is one) and the
;; rule of its footer, which gives the offsets after the last transition. `tzdata-lookup` takes
;; seconds since the epoch and returns the UTC offset in seconds (east positive), whether it is
;; daylight saving time, and the abbreviation.
(library (capy time tzif)
  (export read-tzif bytevector->tzdata parse-posix-tz make-tzdata tzdata? tzdata-lookup tzdata-rule
          tzdata-transitions)
  (import (rnrs) (rnrs mutable-strings) (capy time civil))

  ;; transitions: vector of seconds, ascending. types: vector of #(offset dst? abbrev), one per
  ;; transition, the type in effect from it. initial: the type before the first transition.
  ;; rule: a posix-rule or #f.
  (define-record-type tzdata
    (fields transitions types initial rule))

  (define-record-type posix-rule
    ;; std and dst are #(offset dst? abbrev); dst is #f for a zone without daylight saving.
    ;; start and end are (date . seconds-after-local-midnight); date is (J n), (N n) or (M m w d).
    (fields std dst start end))

  ;; ---- TZif ------------------------------------------------------------------------------

  (define (read-tzif path)
    (bytevector->tzdata (call-with-port (open-file-input-port path) get-bytevector-all)))

  (define (bad-tzif why) (error 'read-tzif (string-append "malformed TZif data: " why)))

  (define (u32 bv i) (bytevector-u32-ref bv i (endianness big)))
  (define (s32 bv i) (bytevector-s32-ref bv i (endianness big)))
  (define (s64 bv i) (bytevector-s64-ref bv i (endianness big)))

  (define (latin1 bv start end)
    (let ([s (make-string (- end start))])
      (do ([i start (+ i 1)]) ((= i end) s)
        (string-set! s (- i start) (integer->char (bytevector-u8-ref bv i))))))

  (define (nul-string bv start limit)
    (let loop ([i start])
      (if (or (>= i limit) (zero? (bytevector-u8-ref bv i)))
        (latin1 bv start i)
        (loop (+ i 1)))))

  (define (bytevector->tzdata bv)
    (define n (bytevector-length bv))
    (define (header at)
      (unless (and (<= (+ at 44) n)
                   (= (bytevector-u8-ref bv at) 84) (= (bytevector-u8-ref bv (+ at 1)) 90)
                   (= (bytevector-u8-ref bv (+ at 2)) 105) (= (bytevector-u8-ref bv (+ at 3)) 102))
        (bad-tzif "no TZif header"))
      (vector (bytevector-u8-ref bv (+ at 4))
              (u32 bv (+ at 20)) (u32 bv (+ at 24)) (u32 bv (+ at 28))
              (u32 bv (+ at 32)) (u32 bv (+ at 36)) (u32 bv (+ at 40))))
    ;; The size of a data block of header H with TIME-SIZE-byte times.
    (define (block-size h time-size)
      (let ([isut (vector-ref h 1)] [isstd (vector-ref h 2)] [leap (vector-ref h 3)]
            [time (vector-ref h 4)] [type (vector-ref h 5)] [char (vector-ref h 6)])
        (+ (* time time-size) time (* type 6) char (* leap (+ time-size 4)) isstd isut)))
    (define (parse-block h at time-size)
      (let* ([timecnt (vector-ref h 4)] [typecnt (vector-ref h 5)] [charcnt (vector-ref h 6)]
             [times-at at]
             [idx-at (+ at (* timecnt time-size))]
             [types-at (+ idx-at timecnt)]
             [chars-at (+ types-at (* typecnt 6))])
        (when (> (+ at (block-size h time-size)) n) (bad-tzif "truncated data block"))
        (when (zero? typecnt) (bad-tzif "no local time types"))
        (let ([types (make-vector typecnt)])
          (do ([i 0 (+ i 1)]) ((= i typecnt))
            (let ([p (+ types-at (* i 6))])
              (vector-set! types i
                (vector (s32 bv p)
                        (not (zero? (bytevector-u8-ref bv (+ p 4))))
                        (nul-string bv (+ chars-at (bytevector-u8-ref bv (+ p 5)))
                                    (+ chars-at charcnt))))))
          (let ([transitions (make-vector timecnt)] [ttypes (make-vector timecnt)])
            (do ([i 0 (+ i 1)]) ((= i timecnt))
              (vector-set! transitions i
                (if (= time-size 8) (s64 bv (+ times-at (* i 8))) (s32 bv (+ times-at (* i 4)))))
              (let ([k (bytevector-u8-ref bv (+ idx-at i))])
                (unless (< k typecnt) (bad-tzif "type index out of range"))
                (vector-set! ttypes i (vector-ref types k))))
            (values transitions ttypes (vector-ref types 0)
                    (+ at (block-size h time-size)))))))
    (let ([h1 (header 0)])
      (if (zero? (vector-ref h1 0))
        (let-values ([(transitions types initial end) (parse-block h1 44 4)])
          (make-tzdata transitions types initial #f))
        (let* ([at2 (+ 44 (block-size h1 4))]
               [h2 (header at2)])
          (let-values ([(transitions types initial end) (parse-block h2 (+ at2 44) 8)])
            (make-tzdata transitions types initial (footer-rule bv end n)))))))

  ;; The footer is "\n" TZ-string "\n"; an empty string means no rule.
  (define (footer-rule bv start n)
    (and (< (+ start 1) n)
         (= (bytevector-u8-ref bv start) 10)
         (let loop ([i (+ start 1)])
           (cond
             [(>= i n) #f]
             [(= (bytevector-u8-ref bv i) 10)
              (and (> i (+ start 1))
                   (guard (e [#t #f]) (posix-rule-of (latin1 bv (+ start 1) i))))]
             [else (loop (+ i 1))]))))

  ;; ---- POSIX TZ strings ------------------------------------------------------------------

  ;; A tzdata with no transitions, only a rule, from a TZ string such as "CET-1CEST,M3.5.0,M10.5.0/3".
  (define (parse-posix-tz s)
    (let ([rule (posix-rule-of s)])
      (make-tzdata (vector) (vector) (posix-rule-std rule) rule)))

  (define (bad-tz s) (error 'parse-posix-tz "malformed TZ string" s))

  (define (posix-rule-of s)
    (define n (string-length s))
    (define pos 0)
    (define (peek) (and (< pos n) (string-ref s pos)))
    (define (next!) (let ([c (peek)]) (set! pos (+ pos 1)) c))
    (define (digits!)
      (let loop ([v #f])
        (let ([c (peek)])
          (if (and c (char<=? #\0 c #\9))
            (begin (next!) (loop (+ (* 10 (or v 0)) (- (char->integer c) 48))))
            (or v (bad-tz s))))))
    (define (name!)
      (if (eqv? (peek) #\<)
        (begin
          (next!)
          (let loop ([start pos])
            (cond
              [(not (peek)) (bad-tz s)]
              [(char=? (peek) #\>) (let ([nm (substring s start pos)]) (next!) nm)]
              [else (next!) (loop start)])))
        (let ([start pos])
          (let loop ()
            (let ([c (peek)])
              (when (and c (char-alphabetic? c)) (next!) (loop))))
          (when (< (- pos start) 3) (bad-tz s))
          (substring s start pos))))
    ;; [+-]hh[:mm[:ss]] as seconds.
    (define (hms!)
      (let* ([sign (case (peek) [(#\+) (next!) 1] [(#\-) (next!) -1] [else 1])]
             [h (digits!)]
             [m (if (eqv? (peek) #\:) (begin (next!) (digits!)) 0)]
             [sec (if (eqv? (peek) #\:) (begin (next!) (digits!)) 0)])
        (* sign (+ (* 3600 h) (* 60 m) sec))))
    (define (date!)
      (case (peek)
        [(#\J) (next!) (list 'J (digits!))]
        [(#\M)
         (next!)
         (let* ([m (digits!)] [_ (unless (eqv? (next!) #\.) (bad-tz s))]
                [w (digits!)] [_ (unless (eqv? (next!) #\.) (bad-tz s))]
                [d (digits!)])
           (unless (and (<= 1 m 12) (<= 1 w 5) (<= 0 d 6)) (bad-tz s))
           (list 'M m w d))]
        [else (list 'N (digits!))]))
    (define (date-time!)
      (let ([d (date!)])
        (cons d (if (eqv? (peek) #\/) (begin (next!) (hms!)) 7200))))
    (let* ([std-name (name!)]
           [std-off (- (hms!))])
      (if (not (peek))
        (make-posix-rule (vector std-off #f std-name) #f #f #f)
        (let* ([dst-name (name!)]
               [dst-off (if (and (peek) (not (char=? (peek) #\,))) (- (hms!)) (+ std-off 3600))]
               [rules (if (eqv? (peek) #\,)
                        (let* ([_ (next!)] [start (date-time!)]
                               [_ (unless (eqv? (next!) #\,) (bad-tz s))]
                               [end (date-time!)])
                          (cons start end))
                        ;; The POSIX default, the United States rules.
                        (cons (cons '(M 3 2 0) 7200) (cons '(M 11 1 0) 7200)))])
          (when (peek) (bad-tz s))
          (make-posix-rule (vector std-off #f std-name) (vector dst-off #t dst-name)
                           (car rules) (cdr rules))))))

  ;; The day number of a rule date in YEAR.
  (define (rule-day date year)
    (case (car date)
      [(J) (let ([k (cadr date)])
             (+ (civil->days year 1 1) (- k 1) (if (and (leap-year? year) (>= k 60)) 1 0)))]
      [(N) (+ (civil->days year 1 1) (cadr date))]
      [else
       (let* ([m (cadr date)] [w (caddr date)] [d (cadddr date)]
              [first (civil->days year m 1)]
              [first-d (+ first (mod (- d (days->weekday first)) 7))]
              [day (+ first-d (* 7 (- w 1)))])
         (if (>= day (+ first (days-in-month year m))) (- day 7) day))]))

  (define (rule-lookup rule t)
    (let ([std (posix-rule-std rule)] [dst (posix-rule-dst rule)])
      (if (not dst)
        std
        (let* ([year (let-values ([(y m d) (days->civil (div (+ t (vector-ref std 0)) 86400))]) y)]
               [dst-at (lambda (y)
                         ;; Start is given in standard time, the end in daylight time.
                         (let ([start (posix-rule-start rule)] [end (posix-rule-end rule)])
                           (values (- (+ (* 86400 (rule-day (car start) y)) (cdr start)) (vector-ref std 0))
                                   (- (+ (* 86400 (rule-day (car end) y)) (cdr end)) (vector-ref dst 0)))))])
          (let-values ([(start end) (dst-at year)])
            (if (< start end)
              (if (and (>= t start) (< t end)) dst std)
              ;; Southern hemisphere: daylight time spans the new year.
              (if (or (>= t start) (< t end)) dst std)))))))

  ;; ---- Lookup ----------------------------------------------------------------------------

  ;; The offset, DST flag and abbreviation in effect at T, seconds since the epoch.
  (define (tzdata-lookup tz t)
    (let* ([tr (tzdata-transitions tz)] [n (vector-length tr)]
           [type (cond
                   [(or (= n 0) (< t (vector-ref tr 0)))
                    (if (and (= n 0) (tzdata-rule tz))
                      (rule-lookup (tzdata-rule tz) t)
                      (tzdata-initial tz))]
                   [(and (>= t (vector-ref tr (- n 1))) (tzdata-rule tz))
                    (rule-lookup (tzdata-rule tz) t)]
                   [else
                    ;; The last transition at or before t.
                    (let loop ([lo 0] [hi (- n 1)])
                      (if (= lo hi)
                        (vector-ref (tzdata-types tz) lo)
                        (let ([mid (div (+ lo hi 1) 2)])
                          (if (<= (vector-ref tr mid) t) (loop mid hi) (loop lo (- mid 1))))))])])
      (values (vector-ref type 0) (vector-ref type 1) (vector-ref type 2)))))
