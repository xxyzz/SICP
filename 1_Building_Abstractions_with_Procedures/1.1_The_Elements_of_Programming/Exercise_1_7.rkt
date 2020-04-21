#lang racket/base

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.00000000001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
