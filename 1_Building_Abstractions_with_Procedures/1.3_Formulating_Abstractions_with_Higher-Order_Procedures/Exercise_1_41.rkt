#lang racket/base

(define (double f)
    (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(((double (double double)) inc) 5)
; 5 + 16 = 21
(((double double) ((double double) inc)) 5)
((double (double (double (double inc)))) 5)
