#lang racket/base

; https://github.com/xxyzz/nand2tetris/blob/master/02/Add16.hdl

(define (ripple-carry-adder A-wire-list B-wire-list S-wire-list C-wire)
  (let ([carry (make-wire)])
    (full-adder (car A-wire-list) (car B-wire-list) C-wire (car S-wire-list) carry)
    (if (null? (cdr A-wire-list))
        (set-signal! carry 0)
        (ripple-carry-adder (cdr A-wire-list) (cdr B-wire-list) (cdr S-wire-list) carry))))
