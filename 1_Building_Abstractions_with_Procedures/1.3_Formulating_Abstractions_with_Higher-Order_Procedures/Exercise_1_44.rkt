#lang racket/base
(require racket/function) ; identity

(define dx 1)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
    (if (< n 1)
        identity
        (compose f (repeated f (sub1 n)))))

(define (smooth f)
    (lambda (x)
        (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
    ((repeated smooth n) f))

((n-fold-smooth square 1) 1)
; (f(0) + f(1) + f(2)) / 3
; 5 / 3

((n-fold-smooth square 2) 1)
; ((f(-1) + f(0) + f(1)) / 3 + (f(0) + f(1) + f(2)) / 3 + (f(1) + f(2) + f(3)) / 3) / 3
; 21 / 9 = 7 / 3
