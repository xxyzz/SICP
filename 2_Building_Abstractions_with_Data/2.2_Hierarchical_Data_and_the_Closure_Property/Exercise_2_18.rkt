#lang racket/base

(define (reverse input-list)
    (define (iter-list current-list result-list)
        (if (null? current-list)
            result-list
            (iter-list (cdr current-list)
                       (cons (car current-list)
                             result-list))))
    (iter-list input-list null))

(reverse (list 1 4 9 16 25))
; '(25 16 9 4 1)
