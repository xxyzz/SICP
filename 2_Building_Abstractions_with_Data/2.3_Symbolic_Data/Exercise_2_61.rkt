#lang racket/base

(define (adjoin-set x set)              
    (cond [(null? set) (list x)]
          [(< x (car set)) (cons x set)]
          [(> x (car set)) (cons (car set) (adjoin-set x (cdr set)))]
          [else set]))

(adjoin-set 3 (list 1 2 4))
; '(1 2 3 4)
