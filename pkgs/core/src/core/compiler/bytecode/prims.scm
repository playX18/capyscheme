;; primitive / runtime-procedure tables for the bytecode lowering.
;;
;; Exports:
;;   (prim-id prim)                -> the `Primitive::id()` discriminant (u16)
;;   (prim-id? prim)               -> #t when prim is a known primitive
;;   (id->prim id)                 -> symbol or #f
;;   (prim-count)                  -> number of primitives (226)
;;   (fused-jump-kinds prim arity) -> (yes . no) pair of branch-kind symbols
;;                                    or #f
;;   (runtime-proc-id name)        -> id of RuntimeProc: 'slot-ref 0, 'slot-set! 1
;;   (runtime-proc-name id)        -> name symbol or #f
;;   runtime-proc-count            -> 2
;;
;; The id table is a vector, the name->id lookup an eq hashtable used for
;; lookup only (never iterated), so output stays deterministic.

(library (core compiler bytecode prims)
  (export prim-id prim-id? id->prim prim-count fused-jump-kinds
          runtime-proc-id runtime-proc-name runtime-proc-count)
  (import (core) (capy))

  ;; Primitive enum order of def.rs, as `Primitive::name()` prints them.
  (define prim-names
    '#(
      %class-id?
      %refptr
      usize->value
      cache-ref
      cache-set!
      immediate?
      heap-object?
      variable-bound?
      variable-ref
      variable-set!
      make-variable
      capy-variable
      variable?
      set-car!
      set-cdr!
      cons
      reverse
      eof-object?
      null?
      unspecified
      unspecified?
      pair?
      list?
      append
      list
      memq
      memv
      car
      cdr
      length
      vector?
      bytevector?
      vector
      tuple
      vector-ref
      vector-set!
      bytevector-length
      bytevector-u8-ref
      tuple-size
      tuple-ref
      tuple-set!
      tuple?
      fx=?
      ash
      logand
      logior
      lognot
      expt
      abs
      sqrt
      cos
      sin
      tan
      atan
      asin
      acos
      ceiling
      floor
      truncate
      +
      -
      *
      /
      =
      <
      >
      >=
      <=
      exact->inexact
      inexact->exact
      even?
      odd?
      zero?
      quotient
      remainder
      modulo
      procedure?
      string?
      boolean?
      symbol?
      eq?
      eqv?
      equal?
      exact-integer?
      integer?
      char?
      number?
      complex?
      nan?
      real?
      rational?
      inexact?
      exact?
      not
      make-tuple
      make-vector
      string-length
      string-ref
      integer->char
      char->integer
      |.breakpoint|
      symbol->string
      string->symbol
      push-cframe
      current-continuation-marks
      $set-attachments!
      $winders
      make-syntax
      |#%default-retk|
      fixnum?
      flonum?
      fx+
      fx-
      fx*
      fx+/ovf?
      fx-/ovf?
      fx*/ovf?
      fx<?
      fx<=?
      fx>?
      fx>=?
      fxand
      fxior
      fxxor
      fxnot
      fxarithmetic-shift-left
      fxarithmetic-shift-right
      fxzero?
      fxpositive?
      fxnegative?
      fxodd?
      fxeven?
      fxmin
      fxmax
      fl+
      fl-
      fl*
      fl/
      fl<?
      fl<=?
      fl>?
      fl>=?
      fl=?
      flzero?
      flpositive?
      flnegative?
      flnan?
      flinfinite?
      flfinite?
      flmin
      flmax
      flabs
      flfloor
      flceiling
      fltruncate
      flround
      flsin
      flcos
      fltan
      flexp
      fllog
      flasin
      flacos
      flsqrt
      flatan
      slot-ref
      slot-set!
      $attachment-push!
      $attachment-pop!
      $attachment-set!
      $attachment-consume!
      $attachment-consume-reify!
      $update-mark
      bytevector-u8-set!
      bytevector-s8-ref
      bytevector-s8-set!
      bytevector-u16-native-ref
      bytevector-u16-native-set!
      bytevector-s16-native-ref
      bytevector-s16-native-set!
      bytevector-u32-native-ref
      bytevector-u32-native-set!
      bytevector-s32-native-ref
      bytevector-s32-native-set!
      bytevector-u64-native-ref
      bytevector-u64-native-set!
      bytevector-s64-native-ref
      bytevector-s64-native-set!
      bytevector-ieee-single-native-ref
      bytevector-ieee-single-native-set!
      bytevector-ieee-double-native-ref
      bytevector-ieee-double-native-set!
      pointer-ref/u8
      pointer-set!/u8
      pointer-ref/s8
      pointer-set!/s8
      pointer-ref/u16
      pointer-set!/u16
      pointer-ref/s16
      pointer-set!/s16
      pointer-ref/u32
      pointer-set!/u32
      pointer-ref/s32
      pointer-set!/s32
      pointer-ref/u64
      pointer-set!/u64
      pointer-ref/s64
      pointer-set!/s64
      pointer-ref/f32
      pointer-set!/f32
      pointer-ref/f64
      pointer-set!/f64
      vector-length
      make-syntax-transformer
      string-set!
      %core-hash-ref/2
      %core-hash-ref/3
      %core-hash-contains?
      %core-hash-put!
      %core-hash-try-set!
      %core-hash-plain?
      %fluid-ref
      %fluid-set!
      %case-lambda))

  (define prim-table
    (let ([t (make-eq-hashtable)])
      (do ([i 0 (+ i 1)]) ((= i (vector-length prim-names)) t)
        (hashtable-set! t (vector-ref prim-names i) i))))

  (define (prim-count) (vector-length prim-names))
  (define (prim-id prim)
    (or (hashtable-ref prim-table prim #f)
        (assertion-violation 'prim-id "unknown primitive" prim)))
  (define (prim-id? prim) (and (hashtable-ref prim-table prim #f) #t))
  (define (id->prim id)
    (and (>= id 0) (< id (vector-length prim-names)) (vector-ref prim-names id)))

  ;; fused_jump_kinds: (prim, arity) -> (yes . no).  Branch kinds are the
  ;; symbols null nnull pair npair fixnum nfixnum undefined nundefined eq neq
  ;; lt le numeq ge gt nlt nle nnumeq nge ngt.
  (define (fused-jump-kinds prim arity)
    (case arity
      [(1)
       (case prim
         [(null?) '(null . nnull)]
         [(pair?) '(pair . npair)]
         [(fixnum?) '(fixnum . nfixnum)]
         [(unspecified?) '(undefined . nundefined)]
         [else #f])]
      [(2)
       (case prim
         [(eq?) '(eq . neq)]
         [(<) '(lt . nlt)]
         [(<=) '(le . nle)]
         [(=) '(numeq . nnumeq)]
         [(>=) '(ge . nge)]
         [(>) '(gt . ngt)]
         [else #f])]
      [else #f]))

  (define runtime-proc-count 2)
  (define (runtime-proc-id name)
    (case name
      [(slot-ref) 0]
      [(slot-set!) 1]
      [else (assertion-violation 'runtime-proc-id "unknown runtime procedure" name)]))
  (define (runtime-proc-name id)
    (case id [(0) 'slot-ref] [(1) 'slot-set!] [else #f]))
)
