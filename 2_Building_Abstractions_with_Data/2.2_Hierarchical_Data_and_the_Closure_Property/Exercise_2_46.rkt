#lang racket/base

(define (make-vect x y)
    (cons x y))

(define (xcor-vect vect)
    (car vect))

(define (ycor-vect vect)
    (cdr vect))

(define (add-vect vect-a vect-b)
    (make-vect (+ (xcor-vect vect-a) (xcor-vect vect-b))
               (+ (ycor-vect vect-a) (ycor-vect vect-b))))
            
(define (sub-vect vect-a vect-b)
    (make-vect (- (xcor-vect vect-a) (xcor-vect vect-b))
               (- (ycor-vect vect-a) (ycor-vect vect-b))))

(define (scale-vect s vect)
    (make-vect (* s (xcor-vect vect))
               (* s (ycor-vect vect))))

(define a (make-vect 1 1))
(define b (make-vect 2 2))
(add-vect a b)
; '(3 . 3)
(sub-vect a b)
; '(-1 . -1)
(scale-vect 2 a)
; '(2 . 2)
