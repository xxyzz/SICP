#lang racket/base

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
    (max (car interval) (cdr interval)))

(define (lower-bound interval)
    (min (car interval) (cdr interval)))

(define (add-interval x y)
    (make-interval  (+ (lower-bound x) (lower-bound y))
                    (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
    (add-interval
        a
        (make-interval
            (- (lower-bound b))
            (- (upper-bound b)))))

(define a (make-interval 3 4))
(define b (make-interval 1 2))
(sub-interval a b)
; '(1 . 3)
; new lower bound = a's lower bound + (- b's upper bound)
; new upper bound = a's upper bound + (- b's lower bound)
