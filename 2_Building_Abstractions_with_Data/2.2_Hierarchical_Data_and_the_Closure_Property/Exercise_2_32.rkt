#lang racket/base

(define (subsets s)
    (if (null? s)
        (list null)
        (let ([rest (subsets (cdr s))])
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets null)
; '(())

; insert 3 to '(()) then append to '(())
(subsets (list 3))
; '(() (3))

; insert 2 to each elements of (() (3)) then append to '(() (3))
(subsets (list 2 3))
; '(() (3) (2) (2 3))

; insert 1 to each elements of (() (3) (2) (2 3)) then append
(subsets (list 1 2 3))
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
