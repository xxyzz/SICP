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

(define (mul-interval x y)
    (let ([lx (lower-bound x)]
          [ux (upper-bound x)]
          [ly (lower-bound y)]
          [uy (upper-bound y)])
    (cond [(and (positive? lx)
                (positive? ux)
                (positive? ly)
                (positive? uy))
            (make-interval (* lx ly) (* ux uy))]
          [(and (negative? lx)
                (negative? ux)
                (negative? ly)
                (negative? uy))
            (make-interval (* ux uy) (* lx ly))]
          [(and (positive? lx)
                (positive? ux)
                (negative? ly)
                (negative? uy))
            (make-interval (* ux ly) (* lx uy))]
          [(and (negative? lx)
                (negative? ux)
                (positive? ly)
                (positive? uy))
            (make-interval (* lx uy) (* ux ly))]
          [(and (positive? lx)
                (positive? ux)
                (negative? ly)
                (positive? uy))
            (make-interval (* ux ly) (* ux uy))]
          [(and (negative? lx)
                (positive? ux)
                (positive? ly)
                (positive? uy))
            (make-interval (* lx uy) (* ux uy))]
          [(and (negative? lx)
                (positive? ux)
                (negative? ly)
                (positive? uy))
            (make-interval (min (* lx uy) (* ux ly)) (max (* ux uy) (* lx ly)))]
          [(and (negative? lx)
                (positive? ux)
                (negative? ly)
                (negative? uy))
            (make-interval (* ux ly) (* lx ly))]
          [(and (negative? lx)
                (negative? ux)
                (negative? ly)
                (positive? uy))
            (make-interval (* lx uy) (* lx ly))])))

(define a (make-interval -3 -4))
(define b (make-interval -1 -1))
(mul-interval a b)
; '(3 . 4)
