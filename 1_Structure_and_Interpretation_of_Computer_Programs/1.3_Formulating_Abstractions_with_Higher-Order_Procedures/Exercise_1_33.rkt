#lang racket/base
(require math/number-theory) ; prime?

; recursive
(define (filtered-accumulate combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner
                (term a)
                (filtered-accumulate combiner null-value term (next a) next b filter))
            (filtered-accumulate combiner null-value term (next a) next b filter))))

; iterative
(define (filtered-accumulate combiner null-value term a next b filter)
    (define (iter x result)
        (if (> x b)
            result
            (if (filter x)
                (iter (next x) (combiner result (term x)))
                (iter (next x) result))))
    (iter a null-value))

(define (prime-square-sum a b)
    (define (square x) (* x x))
    (filtered-accumulate + 0 square 2 add1 b prime?))

(define (relative-prime-product n)
    (define (self x) x)
    (define (filter x) (= (gcd x n) 1))
    (filtered-accumulate * 1 self 2 add1 n filter))

(prime-square-sum 1 5)
; 38 = 2^2 + 3^3 + 5^2

(relative-prime-product 5)
; 24 = 2 * 3 * 4
