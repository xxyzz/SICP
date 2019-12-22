#lang racket/base

(define (last-pair input-list)
    (define (iter-list current-list)
        (if (null? (cdr current-list))
            current-list
            (iter-list (cdr current-list))))
    (iter-list input-list))

(last-pair (list 23 72 149 34))
; '(34)
