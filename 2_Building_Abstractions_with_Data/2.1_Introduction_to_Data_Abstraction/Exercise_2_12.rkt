#lang racket/base

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
    (max (car interval) (cdr interval)))

(define (lower-bound interval)
    (min (car interval) (cdr interval)))

(define (make-center-percent c p)
    (let ([tolerance (* c (/ p 100))])
        (make-interval (- c tolerance) (+ c tolerance))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* (/ (width i) (center i)) 100))

(make-center-percent 2 50)
; '(1 . 3)

(percent (make-center-percent 2 50))
; 50
