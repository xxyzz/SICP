#lang racket/base

(define (sum term a next b k n)
    (define (coefficient x)
        (cond [(zero? x) 1]
              [(= x n) 1]
              [(even? x) 2]
              [else 4]))
    (if (> k n)
        0
        (+  (* (coefficient k) (term a))
            (sum term (next (add1 k)) next b (add1 k) n))))

(define (cube n) (* n n n))

(define (integral f a b n)
    (define h (/ (- b a) n))
    (define (next k)
        (+ a (* k h)))
    (* (/ h 3) (sum f a next b 0 n)))

(integral cube 0 1 100)
; 1 / 4

(integral cube 0 1 1000)
; 1 / 4

(integral cube 0 1.0 100)
; 0.24999999999999992

(integral cube 0 1.0 1000)
; 0.2500000000000003

(define (old-sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (old-sum term (next a) next b))))

(define (old-integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
        dx))

(old-integral cube 0 1 0.01)
; 0.24998750000000042

(old-integral cube 0 1 0.001)
; 0.249999875000001
