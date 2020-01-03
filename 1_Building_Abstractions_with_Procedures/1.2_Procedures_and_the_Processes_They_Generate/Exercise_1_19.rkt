#lang racket

(define (fib n)
    (fib-iter 0 1 0 1 n))

(define (fib-iter a b x y n)
    (cond [(zero? n) x]
          [(even? n)
            (fib-iter
                (+ (* a a) (* b b))
                (+ (* 2 a b) (* b b))
                x
                y
                (/ n 2))]
          [else
            (fib-iter
                a
                b
                (+ (* a x) (* b y))
                (+ (* b x) (* a y) (* b y))
                (- n 1))]))

(fib 3)
; 2