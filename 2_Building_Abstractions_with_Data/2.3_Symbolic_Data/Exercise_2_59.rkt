#lang racket/base

(equal? (list 1) (list 1))
; #t

(eq? (list 1) (list 1))
; #f

(define (element-of-set? x set)
    (cond [(null? set) #f]
          [(equal? x (car set)) #t]
          [else (element-of-set? x (cdr set))]))

(define (union-set set1 set2)
    (cond [(null? set1) set2]
          [(null? set2) set1]
          [(element-of-set? (car set2) set1) (union-set set1 (cdr set2))]
          [else (union-set (cons (car set2) set1) (cdr set2))]))

(union-set (list 1 2 3) (list 2 3 4))
; '(4 1 2 3)
