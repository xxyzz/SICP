#lang racket/base
(require sicp-pict)
; (require racket/function) ; identity
; https://github.com/racket/racket/blob/master/racket/collects/racket/function.rkt#L10

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ([m (frame-coord-map frame)])
            (let ([new-origin (m origin)])
                (painter (make-frame new-origin
                                     (vector-sub (m corner1) new-origin)
                                     (vector-sub (m corner2) new-origin)))))))

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1 0)
                       (make-vect 0 0)
                       (make-vect 1 1)))

(paint (flip-horiz einstein))

; rotate90, rotate180, rotate270
(define (rotate90 painter)
    (transform-painter painter
                       (make-vect 1 0)
                       (make-vect 1 1)
                       (make-vect 0 0)))

(paint (rotate90 einstein))

(define (identity x) x)
; or
(define identity
    (lambda (x) x))

(define (compose f g) ; exercise 1.42
    (lambda (x) (f (g x))))

(define (repeated f n) ; exercise 1.43
    (define (iter g i)
        (if (< i 1)
            g
            (iter (compose f g) (sub1 i))))
    (iter identity n))

(define rotate180 (repeated rotate90 2))

(paint (rotate180 einstein))

(define rotate270 (repeated rotate90 3))

(paint (rotate270 einstein))
