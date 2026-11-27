;; Proleptic Gregorian calendar arithmetic on day numbers.
;;
;; A day number counts days from 1970-01-01 (day 0); it is negative before. The conversions are
;; Howard Hinnant's `days_from_civil` and `civil_from_days`, exact for every year.
;;
;;   (civil->days 2000 3 1)   => 11017
;;   (days->civil 11017)      => 2000 3 1   (three values)
;;   (days->weekday 11017)    => 3          (Wednesday; Sunday is 0)
(library (capy time civil)
  (export civil->days days->civil days->weekday leap-year? days-in-month days-in-year
          year-day ordinal->days iso-week-date iso-week->days)
  (import (rnrs))

  (define (leap-year? y)
    (and (zero? (mod y 4)) (or (not (zero? (mod y 100))) (zero? (mod y 400)))))

  (define (days-in-year y) (if (leap-year? y) 366 365))

  (define (days-in-month y m)
    (case m
      [(1 3 5 7 8 10 12) 31]
      [(4 6 9 11) 30]
      [(2) (if (leap-year? y) 29 28)]
      [else (assertion-violation 'days-in-month "month out of range" m)]))

  (define (civil->days y m d)
    (let* ([y (if (<= m 2) (- y 1) y)]
           [era (div y 400)]
           [yoe (- y (* era 400))]
           [mp (if (> m 2) (- m 3) (+ m 9))]
           [doy (+ (div (+ (* 153 mp) 2) 5) (- d 1))]
           [doe (+ (* yoe 365) (div yoe 4) (- (div yoe 100)) doy)])
      (+ (* era 146097) doe -719468)))

  (define (days->civil z)
    (let* ([z (+ z 719468)]
           [era (div z 146097)]
           [doe (- z (* era 146097))]
           [yoe (div (- doe (div doe 1460) (- (div doe 36524)) (div doe 146096)) 365)]
           [y (+ yoe (* era 400))]
           [doy (- doe (- (+ (* 365 yoe) (div yoe 4)) (div yoe 100)))]
           [mp (div (+ (* 5 doy) 2) 153)]
           [d (+ (- doy (div (+ (* 153 mp) 2) 5)) 1)]
           [m (if (< mp 10) (+ mp 3) (- mp 9))])
      (values (if (<= m 2) (+ y 1) y) m d)))

  ;; 0 is Sunday, 6 is Saturday. Day 0 was a Thursday.
  (define (days->weekday z) (mod (+ z 4) 7))

  ;; The day of the year, 1 for January 1st.
  (define (year-day y m d) (+ 1 (- (civil->days y m d) (civil->days y 1 1))))

  (define (ordinal->days y yday) (+ (civil->days y 1 1) (- yday 1)))

  ;; ISO 8601 week date of a day number: (values week-year week weekday), weekday 1 = Monday.
  (define (iso-week-date z)
    (let-values ([(y m d) (days->civil z)])
      (let* ([wd (let ([w (days->weekday z)]) (if (= w 0) 7 w))]
             ;; The Thursday of this week decides the year.
             [thu (+ z (- 4 wd))])
        (let-values ([(ty tm td) (days->civil thu)])
          (values ty (+ 1 (div (- thu (civil->days ty 1 1)) 7)) wd)))))

  ;; The day number of ISO week date YEAR-Wweek-WEEKDAY.
  (define (iso-week->days year week weekday)
    (let* ([jan4 (civil->days year 1 4)]
           [wd4 (let ([w (days->weekday jan4)]) (if (= w 0) 7 w))]
           [monday1 (- jan4 (- wd4 1))])
      (+ monday1 (* 7 (- week 1)) (- weekday 1)))))
