#lang racket/base

(define (sum term next n)
    (define (coefficient k)
        (cond [(zero? k) 1]
              [(= k n) 1]
              [(even? k) 2]
              [else 4]))
    (define (iter k result)
        (if (> k n)
            result
            (iter
                (add1 k)
                (+ result (* (coefficient k) (term (next k)))))))
    (iter 0 0))

(define (cube n) (* n n n))

(define (integral f a b n)
    (define h (/ (- b a) n))
    (define (next k)
        (+ a (* k h)))
    (* (/ h 3) (sum f next n)))

(integral cube 0 1 100)
; 1 / 4