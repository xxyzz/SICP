#lang racket

(define (square x) (* x x))

(define (f a b c)
    (cond ((and (> a c) (> b c)) (+ (square a) (square b)))
          ((and (> a b) (> c b)) (+ (square a) (square c)))
          ((and (> b a) (> c a)) (+ (square b) (square c)))
    ))

(f 3 2 1)
; 14