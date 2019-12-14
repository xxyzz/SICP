#lang racket/base

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(car (cons x y))
(car (lambda (m) (m x y)))
(lambda (lambda (p q) p) ((lambda (p q) p) x y))
(lambda (x y) x)
; x

(define (cdr z)
    (z (lambda (p q) q)))

(car (cons 1 2))
; 1
(cdr (cons 1 2))
; 2
