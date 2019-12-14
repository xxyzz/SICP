#lang racket/base
(require racket/math) ; exact-ceiling

(define (mycons a b)
    (* (expt 2 a) (expt 3 b)))

(define (iter-devide x base devisor)
    (let ([quotient (/ x devisor)])
        (if (integer? quotient)
            (iter-devide quotient base devisor)
            (exact-ceiling (log x base)))))

(define (mycar x)
    (iter-devide x 2 3))
; remove all the 3, then log with 2 get a

(define (mycdr x)
    (iter-devide x 3 2))

(define pair (mycons 1 2))
(mycar pair)
; 1
(mycdr pair)
; 2
