(import (scheme base)
  (scheme read)
  (core conditions)
  (srfi 64)
  (capy))

(test-begin "boot macro expansion trace")

(define (alist-ref alist key)
  (let ((entry (assq key alist)))
    (and entry (cdr entry))))

(define-syntax macro-trace-fail
  (lambda (stx)
    (syntax-violation 'macro-trace-fail "boom" stx)))

(define captured
  (guard (exn
          [#t exn])
    (macroexpand (read-syntax (open-input-string "(macro-trace-fail)")))))

(test-assert "macro expansion raises syntax violation"
  (syntax-violation? captured))

(test-assert "syntax violation carries expansion trace"
  (expansion-trace? captured))

(let* ((frames (condition-expansion-trace captured))
       (frame (and (pair? frames) (car frames))))
  (test-assert "expansion trace has a frame" (pair? frame))
  (test-equal "expansion frame records macro name"
    'macro-trace-fail
    (alist-ref frame 'macro))
  (test-assert "expansion frame records use-site source"
    (vector? (alist-ref frame 'use-site))))

(let ((rendered
        (call-with-output-string
          (lambda (port)
            ((current-exception-printer) captured port)))))
  (test-assert "printed condition includes expansion trace"
    (string-contains rendered "frames:"))
  (test-assert "printed trace names macro"
    (string-contains rendered "while expanding macro-trace-fail")))

(test-end "boot macro expansion trace")
