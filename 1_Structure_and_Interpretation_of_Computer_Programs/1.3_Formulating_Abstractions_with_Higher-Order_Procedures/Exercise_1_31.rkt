#lang racket/base

; recursive
(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

(define (factorial n)
    (define (term x) x)
    (define (next x) (add1 x))
    (product term 1 next n))

(factorial 4)
; 24

(define (f n)
    (cond [(odd? n) (/ (+ n 1) (+ n 2))]
          [else (/ (+ n 2) (+ n 1))]))

(define (pi n)
    (* 4 (product f 1 add1 n)))

(* (pi 10000) 1.0)
; 3.1417497057380523

; iterative
(define (product term a next b result)
    (if (> a b)
        result
        (product term (next a) next b (* result (term a)))))

(define (factorial n)
    (define (term x) x)
    (define (next x) (add1 x))
    (product term 1 next n 1))

(factorial 4)
; 24

(define (pi n)
    (* 4 (product f 1 add1 n 1)))

(* (pi 10000) 1.0)
; 3.1417497057380523
