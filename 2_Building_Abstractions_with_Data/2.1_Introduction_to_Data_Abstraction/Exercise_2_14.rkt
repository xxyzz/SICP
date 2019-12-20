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
    (let ([p1 (* (lower-bound x) (lower-bound y))]
          [p2 (* (lower-bound x) (upper-bound y))]
          [p3 (* (upper-bound x) (lower-bound y))]
          [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
    (when (= (interval-width y) 0)
        (error "can't divide a zero length interval"))
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

(define (interval-width interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (make-center-percent c p)
    (let ([tolerance (* c (/ p 100))])
        (make-interval (- c tolerance) (+ c tolerance))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* (/ (width i) (center i)) 100))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
        one (add-interval (div-interval one r1)
                          (div-interval one r2)))))

(define a (make-center-percent 2 50))
(define b (make-center-percent 3 50))

(center  (par1 a a))
; 2.3333333333333335
(percent (par1 a a))
; 92.85714285714285
(center  (par2 a a))
; 1.0
(percent (par2 a a))
; 50.0
; part 2 is better, interval arithmetic is different.
