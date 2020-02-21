#lang racket/base

(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x)) 1)))

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(count-pairs (mcons 'a (mcons 'b (mcons 'c null))))
; 3

(define count-4-list (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcar! count-4-list (last-pair count-4-list))
(count-pairs count-4-list)
; 4
; 🛑 is null
;   ____________
;  |          \|/
; ⬛⬛->⬛⬛->⬛🛑
;       |     |
;       b     c

(define count-7-list (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcar! count-7-list (mcdr count-7-list))
(set-mcar! (mcdr count-7-list) (last-pair count-7-list))
(count-pairs count-7-list)
; 7
; 1 + (1 + 1 + 1) + (1 + 1 + 1)
;         ______
;        |    \|/
; ⬛⬛->⬛⬛->⬛🛑
; |    /|\    |
; ------      c

(define infinite-list (mcons 'a (mcons 'b (mcons 'c null))))
(let ([last (last-pair count-4-list)])
      (set-mcdr! last last))
(count-pairs infinite-list)
; infinite loop
;               ___
;             \|/ |
; ⬛⬛->⬛⬛->⬛⬛
