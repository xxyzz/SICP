#lang racket/base

; recursive
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b))))

; iterative
(define (accumulate combiner null-value term a next b)
    (define (iter x result)
        (if (> x b)
            result
            (iter (next x) (combiner result (term x)))))
    (iter a null-value))

(define (self x) x)

(define (sum a b)
    (accumulate + 0 self a add1 b))

(define (product a b)
    (accumulate * 1 self a add1 b))

(sum 1 5)
; 15

(product 1 5)
; 120
