#lang racket

(define (double x) (+ x x))

; divides an (even) integer by 2
(define (halve x) (/ x 2))

(define (mlp a b)
    (cond [(zero? b) 0]
          [(even? b) (mlp (double a) (halve b))]
          [else (+ a (mlp a (sub1 b)))]))

(mlp 6 4)
; 24
