#lang racket/base

(define (make-segment vect-a vect-b)
    (cons vect-a vect-b))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))
