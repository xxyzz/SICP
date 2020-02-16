#lang racket/base

(define (make-accumulator sum)
  (lambda (value)
    (set! sum (+ sum value))
    sum))

(define A (make-accumulator 5))
(A 10)
; 15
(A 10)
; 25
