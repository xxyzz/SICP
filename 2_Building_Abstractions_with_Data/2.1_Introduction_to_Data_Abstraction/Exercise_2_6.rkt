#lang racket/base

(define zero (lambda (f) (lambda (x) x)))
; same as (define (zero f) (lambda (x) x))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
; (lambda (f) (lambda (x) (f ((n f) x)))) ; n is zero
; (lambda (f) (lambda (x) (f x))) ; one

(define one (lambda (f) (lambda (x) (f x)))) ; apply f on x one time

(define two (lambda (f) (lambda (x) (f (f x))))) ; apply f on x two times

(define (myadd a b)
    (lambda (f)
        (lambda (x)
            ((a f) ((b f) x)))))
; apply f on x b times then a times

(((myadd one two) (lambda (x) (println x) x)) "wow ")
; "wow " ; printed
; "wow " ; printed
; "wow " ; printed
; "wow " ; returned

(((myadd one two) add1) 2)
; 5
