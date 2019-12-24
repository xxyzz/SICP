#lang racket/base

(define (deep-reverse input-list)
    (define (iter-list current-list result-list)
        (if (null? current-list)
            result-list
            (iter-list (cdr current-list)
                       (let ([car-value (car current-list)])
                            (if (pair? car-value)
                                (cons (deep-reverse car-value) result-list)
                                (cons car-value result-list))))))
    (iter-list input-list null))

(define x (list (list 1 2) (list 3 4)))
; '((1 2) (3 4))

(deep-reverse x)
; '((4 3) (2 1))

(deep-reverse (list 1 2 3 4))
; '(4 3 2 1)
