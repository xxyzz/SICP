#lang racket

(define (recursive_f n)
    (if (< n 3)
        n
        (+ (recursive_f (- n 1))
           (* 2 (recursive_f (- n 2)))
           (* 3 (recursive_f (- n 3))))))

(define (iterative_f n) 
    (if (< n 3)
        n
        (f-iter n 2 1 0)))
(define (f-iter n a b c)
; a = f-iter(n - 1), b = f-iter(n - 2), c = f-iter(n - 3)
    (if (< n 3)
        a
        (f-iter (- n 1) (+ a (* 2 b) (* 3 c)) a b)))
