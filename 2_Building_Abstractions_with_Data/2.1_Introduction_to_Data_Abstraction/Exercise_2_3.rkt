#lang racket/base
(require racket/math) ; sqr

(define (make-segment start-point end-point)
    (cons start-point end-point))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (segment-length segment)
    (point-distance (start-segment segment) (end-segment segment)))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (point-distance first-point second-point)
    (sqrt (+
            (sqr (- (x-point first-point) (x-point second-point)))
            (sqr (- (y-point first-point) (y-point second-point))))))

(define (make-rect first-seg second-seg)
    (cons first-seg second-seg))

(define (rect-width rect)
    (segment-length (car rect)))

(define (rect-height rect)
    (segment-length (cdr rect)))

(define (rect-perimeter rect)
    (* 2 (+ (rect-width rect) (rect-width rect))))

(define (rect-area rect)
    (* (rect-width rect) (rect-width rect)))

; second implemention
(define (make-rect bottom-left-point top-right-point)
    (cons bottom-left-point top-right-point))

(define (rect-width rect)
    (abs (- (x-point (car rect)) (x-point (cdr rect)))))

(define (rect-height rect)
    (abs (- (y-point (car rect)) (y-point (cdr rect)))))

(define point-a (make-point 0 0))
(define point-b (make-point 0 1))
(define point-c (make-point 1 1))
(define seg-a   (make-segment point-a point-b))
(define seg-b   (make-segment point-b point-c))
(define rect    (make-rect seg-a seg-b))
; (define rect  (make-rect point-a point-c))
(rect-perimeter rect)
; 4
(rect-area rect)
; 1
