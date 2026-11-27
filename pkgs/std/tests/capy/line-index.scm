(import (rnrs)
  (srfi 64)
  (capy text line-index))

(test-begin "capy text line-index")

(define (pos idx off enc)
  (let-values ([(line ch) (line-index-offset->position idx off enc)])
    (list line ch)))

;; a, U+1F600 (4 bytes in UTF-8, a surrogate pair in UTF-16), b, CRLF, x, y, lone CR, z, LF.
;; Offsets:  a0 😀1 b2 \r3 \n4 x5 y6 \r7 z8 \n9, length 10.
(define text "a\x1F600;b\r\nxy\rz\n")
(define idx (make-line-index text))

(test-group "lines"
  (test-equal "line count (CRLF, lone CR, final empty line)" 4 (line-index-line-count idx))
  (test-equal "line starts" '(0 5 8 10) (map (lambda (l) (line-index-line-start idx l)) '(0 1 2 3)))
  (test-equal "line ends exclude the terminator" '(3 7 9 10)
    (map (lambda (l) (line-index-line-end idx l)) '(0 1 2 3)))
  (test-equal "line of offset" '(0 0 0 0 0 1 1 1 2 2 3)
    (map (lambda (o) (line-index-offset->line idx o)) '(0 1 2 3 4 5 6 7 8 9 10)))
  (test-equal "empty text has one line" '(1 0 0)
    (let ([e (make-line-index "")])
      (list (line-index-line-count e) (line-index-line-start e 0) (line-index-line-end e 0))))
  (test-equal "text without terminator" 1 (line-index-line-count (make-line-index "abc")))
  (test-equal "only terminators" 4 (line-index-line-count (make-line-index "\n\r\n\r")))
  (test-equal "length and byte length" '(10 13) (list (line-index-length idx) (line-index-byte-length idx))))

(test-group "UTF-32"
  (test-equal "start" '(0 0) (pos idx 0 'utf-32))
  (test-equal "after the emoji" '(0 2) (pos idx 2 'utf-32))
  (test-equal "end of line 0" '(0 3) (pos idx 3 'utf-32))
  (test-equal "line 1" '(1 1) (pos idx 6 'utf-32))
  (test-equal "line 2 after lone CR" '(2 1) (pos idx 9 'utf-32))
  (test-equal "EOF" '(3 0) (pos idx 10 'utf-32))
  (test-equal "position to offset" '(2 3 6 9 10)
    (map (lambda (p) (line-index-position->offset idx (car p) (cadr p) 'utf-32))
      '((0 2) (0 3) (1 1) (2 1) (3 0)))))

(test-group "UTF-16 and surrogate pairs"
  (test-equal "after a" '(0 1) (pos idx 1 'utf-16))
  (test-equal "after the emoji counts two units" '(0 3) (pos idx 2 'utf-16))
  (test-equal "end of line 0" '(0 4) (pos idx 3 'utf-16))
  (test-equal "next line is unaffected" '(1 2) (pos idx 7 'utf-16))
  (test-equal "position to offset: before the emoji" 1 (line-index-position->offset idx 0 1 'utf-16))
  (test-equal "position to offset: after the emoji" 2 (line-index-position->offset idx 0 3 'utf-16))
  (test-equal "inside the pair rounds down" 1 (line-index-position->offset idx 0 2 'utf-16))
  (test-equal "end of line" 3 (line-index-position->offset idx 0 4 'utf-16))
  (test-equal "round trip at every offset"
    '(0 1 2 3 5 6 7 8 9 10)
    (map (lambda (o)
          (let-values ([(l c) (line-index-offset->position idx o 'utf-16)])
            (line-index-position->offset idx l c 'utf-16)))
      '(0 1 2 3 5 6 7 8 9 10))))

(test-group "UTF-8"
  (test-equal "after a" '(0 1) (pos idx 1 'utf-8))
  (test-equal "after the emoji" '(0 5) (pos idx 2 'utf-8))
  (test-equal "end of line 0" '(0 6) (pos idx 3 'utf-8))
  (test-equal "position to offset" 2 (line-index-position->offset idx 0 5 'utf-8))
  (test-equal "inside the emoji rounds down" 1 (line-index-position->offset idx 0 3 'utf-8)))

(test-group "bytes"
  (test-equal "offset to byte" '(0 1 5 6 8 11 13)
    (map (lambda (o) (line-index-offset->byte idx o)) '(0 1 2 3 5 8 10)))
  (test-equal "byte to offset" '(0 1 2 3 5 8 10)
    (map (lambda (b) (line-index-byte->offset idx b)) '(0 1 5 6 8 11 13)))
  (test-equal "byte inside a character rounds down" '(1 1 1)
    (map (lambda (b) (line-index-byte->offset idx b)) '(2 3 4)))
  (test-equal "bytes are clamped" '(0 10)
    (map (lambda (b) (line-index-byte->offset idx b)) '(-5 999))))

(test-group "clamping"
  (test-equal "character past the end of the line stops before CRLF" 3
    (line-index-position->offset idx 0 100 'utf-32))
  (test-equal "past the end of a line ending in a lone CR" 7
    (line-index-position->offset idx 1 100 'utf-16))
  (test-equal "past the end of the last line" 10 (line-index-position->offset idx 3 7 'utf-8))
  (test-equal "line past the end is the last line" 10 (line-index-position->offset idx 99 0 'utf-32))
  (test-equal "line past the end, large character" 10 (line-index-position->offset idx 99 99 'utf-16))
  (test-equal "negative line and character" 0 (line-index-position->offset idx -1 -1 'utf-32))
  (test-equal "offset past the end" '(3 0) (pos idx 500 'utf-32))
  (test-equal "negative offset" '(0 0) (pos idx -3 'utf-16))
  (test-equal "line-end of an out-of-range line" 10 (line-index-line-end idx 50))
  (test-equal "unknown encoding" 'error
    (guard (e [#t 'error]) (pos idx 0 'ucs-2))))

(test-group "end-of-line positions"
  (let ([crlf (make-line-index "ab\r\ncd")])
    (test-equal "end of first line" 2 (line-index-position->offset crlf 0 2 'utf-16))
    (test-equal "EOL position of line 0 reads back" '(0 2) (pos crlf 2 'utf-16))
    (test-equal "start of second line" '(1 0) (pos crlf 4 'utf-16))
    (test-equal "end of text" '(1 2) (pos crlf 6 'utf-16))
    (test-equal "just before the LF" '(0 3) (pos crlf 3 'utf-16)))
  (let ([e (make-line-index "a\n")])
    (test-equal "empty last line" '(1 0) (pos e 2 'utf-32))
    (test-equal "position on the empty last line" 2 (line-index-position->offset e 1 0 'utf-32))))

;; Updating gives the same index as building a new one.
(define (same-index? a b)
  (and (string=? (line-index-text a) (line-index-text b))
    (= (line-index-line-count a) (line-index-line-count b))
    (= (line-index-byte-length a) (line-index-byte-length b))
    (let loop ([l 0])
      (or (= l (line-index-line-count a))
        (and (= (line-index-line-start a l) (line-index-line-start b l))
          (= (line-index-line-end a l) (line-index-line-end b l))
          (loop (+ l 1)))))
    (let loop ([o 0])
      (or (> o (line-index-length a))
        (and (= (line-index-offset->byte a o) (line-index-offset->byte b o))
          (loop (+ o 1)))))))

(test-group "update"
  (test-assert "replace inside a line"
    (same-index? (line-index-update idx 1 2 "XYZ") (make-line-index "aXYZb\r\nxy\rz\n")))
  (test-equal "multi-line insertion" 7
    (line-index-line-count (line-index-update idx 5 5 "p\nq\nr\n")))
  (test-equal "deleting a terminator joins lines" 3
    (line-index-line-count (line-index-update idx 3 5 "")))
  (test-equal "a CR before the edit pairs with an inserted LF" 3
    (line-index-line-count (line-index-update (make-line-index "a\rb\n") 2 2 "\n")))
  (test-equal "a LF after the edit pairs with an inserted CR" 2
    (line-index-line-count (line-index-update (make-line-index "a\nb") 1 1 "\r")))
  (test-assert "insert at EOF"
    (same-index? (line-index-update idx 10 10 "tail\n") (make-line-index "a\x1F600;b\r\nxy\rz\ntail\n")))
  (test-assert "exhaustive small edits"
    (let* ([alphabet '("a" "\r" "\n" "\x1F600;" "\r\n" "")]
           [bases '("" "a\r\nb" "\n\n" "\r\r\n\n\r" "a\x1F600;\nb\rc")])
      (for-all
        (lambda (base)
          (let ([n (string-length base)] [i (make-line-index base)])
            (let loop-s ([s 0])
              (or (> s n)
                (and
                  (let loop-e ([e s])
                    (or (> e n)
                      (and
                        (for-all
                          (lambda (ins)
                            (same-index?
                              (line-index-update i s e ins)
                              (make-line-index
                                (string-append (substring base 0 s) ins (substring base e n)))))
                          alphabet)
                        (loop-e (+ e 1)))))
                  (loop-s (+ s 1)))))))
        bases)))
  (test-assert "the original index is not changed"
    (begin
      (line-index-update idx 0 10 "")
      (same-index? idx (make-line-index text)))))

(test-group "large text"
  (let* ([line "let x = \x3bb; in \x1F600; x\n"]
         [big (make-line-index (call-with-string-output-port
                           (lambda (out) (do ([i 0 (+ i 1)]) ((= i 20000)) (put-string out line)))))]
         [n (string-length line)])
    (test-equal "line count" 20001 (line-index-line-count big))
    (test-equal "position of a late offset" '(15000 3)
      (pos big (+ (* 15000 n) 3) 'utf-32))
    (test-equal "utf-16 counts the pair" '(15000 15)
      (pos big (+ (* 15000 n) 14) 'utf-16))
    (test-equal "update keeps the tables"
      (list 20003 (+ (* 10000 n) 2))
      (let ([u (line-index-update big (* 10000 n) (* 10000 n) "\n\n")])
        (list (line-index-line-count u) (line-index-line-start u 10002))))))

(test-end)
