#lang racket/base

; n
(define (element-of-set? x set)
    (cond [(null? set) #f]
          [(equal? x (car set)) #t]
          [else (element-of-set? x (cdr set))]))

; n -> 1
(define (adjoin-set x set)
    (cons x set))

; n^2 -> n
(define (union-set set1 set2)
    (append set1 set2))

; n^2
(define (intersection-set set1 set2)
    (cond [(or (null? set1) (null? set2)) '()]
          [(element-of-set? (car set1) set2)
              (cons (car set1) (intersection-set (cdr set1) set2))]
          [else (intersection-set (cdr set1) set2)]))
