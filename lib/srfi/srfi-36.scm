(define-library (srfi srfi-36)
  (export &i/o-error i/o-error? make-i/o-error
    &i/o-port-error
    i/o-port-error?
    make-i/o-port-error
    i/o-error-port
    &i/o-read-error
    i/o-read-error?
    make-i/o-read-error
    &i/o-write-error
    i/o-write-error?
    make-i/o-write-error
    &i/o-closed-error
    i/o-closed-error?
    make-i/o-closed-error
    &i/o-filename-error
    i/o-filename-error?
    make-i/o-filename-error
    i/o-error-filename
    &i/o-malformed-filename-error
    i/o-malformed-filename-error?
    make-i/o-malformed-filename-error
    &i/o-file-protection-error
    i/o-file-protection-error?
    make-i/o-file-protection-error
    &i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    make-i/o-file-is-read-only-error
    &i/o-file-already-exists-error
    i/o-file-already-exists-error?
    make-i/o-file-already-exists-error
    &i/o-no-such-file-error
    i/o-no-such-file-error?
    make-i/o-no-such-file-error)
  (import (capy)))
