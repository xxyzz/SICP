#lang racket/base

(define (square x) (* x x))

(define (square-list items)
    (if (null? items)
        null
        (cons (square (car itmes)) (square-list (cdr itmes)))))

(define (square-list items)
    (map square items))

(square-list (list 1 2 3 4))
; '(1 4 9 16)
