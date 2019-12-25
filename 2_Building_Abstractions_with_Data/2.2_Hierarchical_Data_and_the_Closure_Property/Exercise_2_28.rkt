#lang racket/base

(define (fringe tree)
    (define (iter current-node result)
        (cond [(null? current-node) result]
              [(pair? current-node) (iter (car current-node) (iter (cdr current-node) result))]
              [else (cons current-node result)]))
    (iter tree null))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
; '(1 2 3 4)
(fringe (list x x))
; '(1 2 3 4 1 2 3 4)
