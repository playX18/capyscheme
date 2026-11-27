(import (rnrs)
  (srfi 64)
  (capy text line-index)
  (capy text edit))

(test-begin "capy text edit")

(define (edit! doc sl sc el ec text)
  (document-apply-edit! doc (make-text-edit sl sc el ec text)))

(test-group "document"
  (let ([doc (make-document "abc\n" 3)])
    (test-equal "text" "abc\n" (document-text doc))
    (test-equal "version" 3 (document-version doc))
    (test-equal "default encoding" 'utf-32 (document-encoding doc))
    (test-equal "line index" 2 (line-index-line-count (document-line-index doc))))
  (test-equal "encoding is checked" 'error
    (guard (e [#t 'error]) (make-document "x" 1 'latin-1))))

(test-group "incremental edits"
  (let ([doc (make-document "hello\nworld\n" 1)])
    (edit! doc 1 0 1 5 "there")
    (test-equal "replace a whole word" "hello\nthere\n" (document-text doc))
    (edit! doc 0 0 0 0 ">> ")
    (test-equal "insert at the start" ">> hello\nthere\n" (document-text doc))
    (edit! doc 0 3 0 8 "HI")
    (test-equal "replace inside a line" ">> HI\nthere\n" (document-text doc))
    (edit! doc 0 2 1 2 "\n---\n")
    (test-equal "multi-line replace" ">>\n---\nere\n" (document-text doc))
    (test-equal "line count follows" 4 (line-index-line-count (document-line-index doc)))
    (edit! doc 0 0 3 0 "")
    (test-equal "delete across all lines" "" (document-text doc))
    (test-equal "one line left" 1 (line-index-line-count (document-line-index doc))))
  (let ([doc (make-document "one\ntwo\n" 1)])
    (edit! doc 2 0 2 0 "three")
    (test-equal "insert at EOF (empty last line)" "one\ntwo\nthree" (document-text doc))
    (edit! doc 2 5 2 5 "\nfour")
    (test-equal "append at the end of the text" "one\ntwo\nthree\nfour" (document-text doc))
    (edit! doc 50 50 50 50 "!")
    (test-equal "a position past the end clamps to EOF" "one\ntwo\nthree\nfour!" (document-text doc))
    (test-equal "positions follow the edits" '(3 5)
      (let-values ([(l c) (line-index-offset->position (document-line-index doc)
                           (string-length (document-text doc)) 'utf-32)])
        (list l c))))
  (let ([doc (make-document "a\r\nb\r\nc" 1)])
    (edit! doc 0 1 1 0 "")
    (test-equal "deleting a CRLF joins lines" "ab\r\nc" (document-text doc))
    (edit! doc 0 2 0 2 "\n")
    (test-equal "insert a newline before CRLF" "ab\n\r\nc" (document-text doc))
    (test-equal "three lines" 3 (line-index-line-count (document-line-index doc))))
  (let ([doc (make-document "x" 1)])
    (edit! doc 0 1 0 0 "y")
    (test-equal "an end before the start is treated as empty" "xy" (document-text doc)))
  (let ([doc (make-document "old\ntext" 1)])
    (document-apply-edit! doc (make-full-text-edit "brand new\n") 2)
    (test-equal "full replacement" "brand new\n" (document-text doc))
    (test-equal "version" 2 (document-version doc))
    (test-equal "index rebuilt" 2 (line-index-line-count (document-line-index doc)))))

(test-group "sequences of changes"
  (let ([doc (make-document "abcdef\nghi\n" 1)])
    (document-apply-edits! doc
      (list (make-text-edit 0 1 0 3 "")    ; "adef"
            (make-text-edit 0 2 0 2 "XY")  ; "adXYef": positions are those of the text after the first edit
            (make-text-edit 1 0 1 1 "G"))
      4)
    (test-equal "applied one after the other" "adXYef\nGhi\n" (document-text doc))
    (test-equal "version" 4 (document-version doc))))

(test-group "encodings"
  ;; U+1F600 is 2 UTF-16 units, 4 UTF-8 bytes, 1 code point.
  (let ([doc (make-document "a\x1F600;b\nxyz" 1 'utf-16)])
    (edit! doc 0 3 0 4 "B")
    (test-equal "utf-16: the character after the emoji is at 3" "a\x1F600;B\nxyz" (document-text doc))
    (edit! doc 0 2 0 3 "")
    (test-equal "utf-16: a range that starts inside the pair starts before it" "aB\nxyz" (document-text doc)))
  (let ([doc (make-document "a\x1F600;b" 1 'utf-8)])
    (edit! doc 0 5 0 6 "!")
    (test-equal "utf-8: bytes" "a\x1F600;!" (document-text doc)))
  (let ([doc (make-document "a\x1F600;b" 1 'utf-32)])
    (edit! doc 0 2 0 3 "!")
    (test-equal "utf-32: code points" "a\x1F600;!" (document-text doc))))

(test-group "TextEdit arrays (all in the original positions)"
  (let ([doc (make-document "one\ntwo\nthree\n" 1)])
    (document-apply-text-edits! doc
      (list (make-text-edit 2 0 2 5 "3")
            (make-text-edit 0 0 0 3 "1")
            (make-text-edit 1 3 1 3 "!"))
      9)
    (test-equal "applied from the end" "1\ntwo!\n3\n" (document-text doc))
    (test-equal "version" 9 (document-version doc)))
  (let ([doc (make-document "ab" 1)])
    (document-apply-text-edits! doc
      (list (make-text-edit 0 1 0 1 "X") (make-text-edit 0 1 0 1 "Y")))
    (test-equal "inserts at one place keep their order" "aXYb" (document-text doc))))

(test-group "line index stays right"
  (let ([doc (make-document "αβγ\nδεζ\n" 1 'utf-16)])
    (edit! doc 0 1 1 1 "\n\x1F600;\n")
    (let ([fresh (make-line-index (document-text doc))]
          [idx (document-line-index doc)])
      (test-equal "text" "α\n\x1F600;\nεζ\n" (document-text doc))
      (test-equal "line count" (line-index-line-count fresh) (line-index-line-count idx))
      (test-equal "byte length" (line-index-byte-length fresh) (line-index-byte-length idx))
      (test-equal "offset to byte"
        (line-index-offset->byte fresh 5)
        (line-index-offset->byte idx 5)))))

(test-end)
