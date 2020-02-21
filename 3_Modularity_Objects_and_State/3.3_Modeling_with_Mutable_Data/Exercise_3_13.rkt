#lang racket/base

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z (make-cycle (mcons 'a (mcons 'b (mcons 'c null)))))
(last-pair z)
; infinite loop

;       |-----------------|
;      \|/                |
; z -> ⬛⬛->⬛⬛->⬛⬛--|
;      |     |     |
;      a     b     c