#lang racket/base

(define (reverse input-list)
    (define (iter-list current-list result-list)
        (if (null? current-list)
            result-list
            (iter-list (cdr current-list)
                       (cons (car current-list)
                             result-list))))
    (iter-list input-list null))

(define (same-parity first . rest)
    (define (iter rest-list check-parity result-list)
        (if (null? rest-list)
            (reverse result-list)
            (if (check-parity (car rest-list))
                (iter (cdr rest-list) check-parity (cons (car rest-list) result-list))
                (iter (cdr rest-list) check-parity result-list))))
    (if (even? first)
        (iter rest even? (list first))
        (iter rest odd? (list first))))

(same-parity 1 2 3 4 5 6 7)
; '(1 3 5 7)

(same-parity 2 3 4 5 6 7)
; '(2 4 6)
