#lang racket/base

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (<  (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (let ([next (f guess)])
            (if (close-enough? guess next)
                next
                (try next))))
(try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; 1.6180327868852458

; golden ratio: φ = (1 + √5) / 2 ≈ 1.6180
; φ^2 = φ + 1
; divide by φ on both sides
; φ = 1 + 1 / φ
