#lang racket/base

(define (compose f g)
    (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

((compose square inc) 6)
; (* (+ 6 1) (+ 6 1))
; 49
