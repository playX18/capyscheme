;; Tests for (capy signal). Signals are sent to this process with libc kill(2).

(import (rnrs)
        (srfi 64)
        (capy channel)
        (capy select)
        (capy signal)
        (core foreign)
        (core foreign-library))

(test-begin "capy signal")

(define libc (load-foreign-library #f))
(define c-kill
  (foreign-library-function libc "kill" #:return int #:arguments (list int int) #:blocking? #f))
(define c-getpid
  (foreign-library-function libc "getpid" #:return int #:arguments '() #:blocking? #f))

(define (raise-signal! sig)
  (c-kill (c-getpid) (signal-number sig)))

;; The next value on CH, or 'timeout.
(define (next ch)
  (select
    [(recv ch) => (lambda (v ok?) v)]
    [(timeout 2.0) 'timeout]))

(test-group "delivery"
  (test-equal "a notified channel receives the signal name"
    'usr1
    (let ([ch (signal-notify (make-channel 4) 'usr1)])
      (raise-signal! 'usr1)
      (let ([v (next ch)]) (signal-stop ch) v)))

  (test-equal "every subscribed channel receives it"
    '(usr2 usr2)
    (let ([a (signal-notify (make-channel 1) 'usr2)]
          [b (signal-notify (make-channel 1) 'usr2)])
      (raise-signal! 'usr2)
      (let ([result (list (next a) (next b))])
        (signal-stop a)
        (signal-stop b)
        result)))

  (test-equal "one channel can listen for several signals"
    '(usr1 usr2)
    (let ([ch (signal-notify (make-channel 4) 'usr1 'usr2)])
      (raise-signal! 'usr1)
      (let ([first (next ch)])
        (raise-signal! 'usr2)
        (let ([result (list first (next ch))])
          (signal-stop ch)
          result))))

  (test-equal "a full channel drops signals instead of blocking"
    'usr1
    (let ([ch (signal-notify (make-channel 1) 'usr1)])
      (raise-signal! 'usr1)
      (raise-signal! 'usr1)
      (raise-signal! 'usr1)
      (let ([first (next ch)])
        (signal-stop ch)
        first))))

(test-group "stopping"
  (test-equal "a stopped channel hears nothing more"
    'timeout
    (let ([ch (signal-notify (make-channel 1) 'usr1)]
          [other (signal-notify (make-channel 1) 'usr2)])
      (signal-stop ch)
      (raise-signal! 'usr2)
      (next other)
      (signal-stop other)
      (select
        [(recv ch) => (lambda (v ok?) v)]
        [(timeout 0.1) 'timeout]))))

(test-group "names"
  (test-equal "names map to numbers and back"
    'winch
    (signal-name (signal-number 'winch)))

  (test-equal "unknown names are rejected"
    'error
    (guard (e [#t 'error]) (signal-number 'bogus)))

  (test-equal "signals the runtime owns cannot be caught"
    'error
    (guard (e [#t 'error])
      (signal-notify (make-channel 1) 9))))

(test-end "capy signal")
