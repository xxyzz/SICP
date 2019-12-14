#lang racket/base

(define (make-segment start-point end-point)
    (cons start-point end-point))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment segment)
    (let   ([start-point (start-segment segment)]
            [end-point   (end-segment segment)])
        (make-point
            (average (x-point start-point) (x-point end-point))
            (average (y-point start-point) (y-point end-point)))))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(print-point
    (midpoint-segment
        (make-segment
            (make-point 0 0)
            (make-point 2 2))))
; (1,1)
