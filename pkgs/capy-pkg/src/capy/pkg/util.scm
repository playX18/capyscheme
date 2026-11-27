;;; Small helpers shared by the package manager.
(library (capy pkg util)
  (export unique-suffix
    write-file-atomically!)
  (import (rnrs)
    (only (capy) file-exists? rename-file microsecond random))

  ;; A string that differs between processes and between calls: the clock and a random number.
  (define (unique-suffix)
    (string-append (number->string (microsecond)) "-" (number->string (random 1000000000))))

  ;; Writes PATH by calling (WRITER port) on a temporary file in the same directory and renaming
  ;; it over PATH. A reader sees the old file or the new one, never a half written one, and PATH
  ;; is untouched when WRITER or the write fails; the temporary file is deleted then.
  (define (write-file-atomically! path writer)
    (let ([tmp (string-append path ".tmp-" (unique-suffix))])
      (guard (e [#t
                 (when (file-exists? tmp) (guard (e2 [#t #f]) (delete-file tmp)))
                 (raise e)])
        (call-with-output-file tmp writer)
        (rename-file tmp path)))))
