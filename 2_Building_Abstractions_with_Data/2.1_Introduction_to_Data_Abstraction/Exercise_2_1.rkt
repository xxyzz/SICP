#lang racket/base

(define (make-rat n d)
    (define (helper a b)
        (let ([g (gcd a b)])
            (cons (/ a g) (/ b g))))
    (if (positive? (* n d))
        (helper (abs n) (abs d))
        (helper (- (abs n)) (abs d))))

(make-rat 6 9)
(make-rat 6 -9)
(make-rat -6 9)
(make-rat -6 -9)
