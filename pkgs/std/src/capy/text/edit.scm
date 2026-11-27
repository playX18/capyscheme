;;; Text documents that change by LSP text edits: the text, its version and its line index.
;;;
;;;   (define doc (make-document "let x = 1\n" 1))
;;;   (document-apply-edit! doc (make-text-edit 0 4 0 5 "y"))   ; replace `x` (line 0, chars 4..5)
;;;   (document-text doc)    => "let y = 1\n"
;;;   (document-apply-edit! doc (make-full-text-edit "new text"))  ; a change without a range
;;;
;;; A range is (start-line start-character end-line end-character); characters count in the
;;; document's position encoding ('utf-32 unless told otherwise), see (capy text line-index)
;;; for how out-of-range positions are clamped. `document-apply-edits!` applies the edits of a
;;; `textDocument/didChange` one after the other, each against the text the one before left, as
;;; the protocol says; `document-apply-text-edits!` applies a `TextEdit[]` whose ranges all refer
;;; to the document as it is now (they must not overlap). The line index is updated by each edit
;;; without scanning the rest of the file.

(library (capy text edit)
  (export make-text-edit
    make-full-text-edit
    text-edit?
    text-edit-range
    text-edit-new-text
    make-document
    document?
    document-text
    document-version
    document-encoding
    document-line-index
    document-range->offsets
    document-apply-edit!
    document-apply-edits!
    document-apply-text-edits!)
  (import (rnrs)
    (capy text line-index))

  ;; RANGE is #f (the whole document) or a list (start-line start-char end-line end-char).
  (define-record-type (<text-edit> %make-text-edit text-edit?)
    (fields
      (immutable range text-edit-range)
      (immutable new-text text-edit-new-text)))

  (define (make-text-edit start-line start-char end-line end-char new-text)
    (%make-text-edit (list start-line start-char end-line end-char) new-text))

  (define (make-full-text-edit new-text)
    (%make-text-edit #f new-text))

  (define-record-type (<document> %make-document document?)
    (fields
      (mutable index %index %set-index!)
      (mutable version document-version %set-version!)
      (immutable encoding document-encoding)))

  (define make-document
    (case-lambda
      [(text version) (make-document text version 'utf-32)]
      [(text version encoding)
        (unless (position-encoding? encoding)
          (assertion-violation 'make-document "not a position encoding" encoding))
        (%make-document (make-line-index text) version encoding)]))

  (define (document-line-index doc) (%index doc))

  (define (document-text doc) (line-index-text (%index doc)))

  ;; The character offsets [start, end) of RANGE in DOC, the end never before the start.
  (define (document-range->offsets doc range)
    (let* ([idx (%index doc)]
           [enc (document-encoding doc)]
           [start (line-index-position->offset idx (car range) (cadr range) enc)]
           [end (line-index-position->offset idx (caddr range) (cadddr range) enc)])
      (values start (max start end))))

  (define (apply-one! doc edit)
    (let ([idx (%index doc)] [range (text-edit-range edit)])
      (if range
        (let-values ([(start end) (document-range->offsets doc range)])
          (%set-index! doc (line-index-update idx start end (text-edit-new-text edit))))
        (%set-index! doc (make-line-index (text-edit-new-text edit))))))

  ;; Applies EDIT to DOC; with VERSION (an integer) the document gets that version.
  (define document-apply-edit!
    (case-lambda
      [(doc edit) (apply-one! doc edit)]
      [(doc edit version) (apply-one! doc edit) (%set-version! doc version)]))

  ;; Applies the list EDITS in order, each to the result of the one before.
  (define document-apply-edits!
    (case-lambda
      [(doc edits) (for-each (lambda (e) (apply-one! doc e)) edits)]
      [(doc edits version) (document-apply-edits! doc edits) (%set-version! doc version)]))

  ;; Applies a list of edits that all use the positions of the document as it is now.
  (define document-apply-text-edits!
    (case-lambda
      [(doc edits)
        (let* ([located
                (map (lambda (e)
                      (let ([range (text-edit-range e)])
                        (if range
                          (let-values ([(s t) (document-range->offsets doc range)])
                            (list s t (text-edit-new-text e)))
                          (list 0 (line-index-length (%index doc)) (text-edit-new-text e)))))
                  edits)]
               ;; From the end of the text to the start, so that offsets stay valid; edits at the
               ;; same place go last-first, so that the result keeps their order.
               [sorted (list-sort (lambda (a b) (> (car a) (car b))) (reverse located))])
          (for-each
            (lambda (l) (%set-index! doc (line-index-update (%index doc) (car l) (cadr l) (caddr l))))
            sorted))]
      [(doc edits version) (document-apply-text-edits! doc edits) (%set-version! doc version)]))
  )
