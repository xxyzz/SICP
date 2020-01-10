#lang racket/base

(define (equal? a b)
    (cond [(and (symbol? a) (symbol? b)) (eq? a b)]
          [(and (number? a) (number? b)) (= a b)]
          [(and (null? a) (null? b)) #t]
          [(or (and (not (null? a)) (null? b))
               (and (null? a) (not (null? b))))
            #f]
          [(and (list? a) (list? b))
            (if (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))
                #f)]
          [else #f]))

(define (equal? a b)
    (cond [(and (not (pair? a)) (not (pair? b))) (eq? a b)]
          [(and (pair? a) (pair? b))
              (and (equal? (car a) (car b))
                   (equal? (cdr a) (cdr b)))]
          [else #f]))

(equal? 1 1)
; #t

(equal? 1 2)
; #f

(equal? 'apple 'apple)
; #t

(equal? 'apple 'pear)
; #f

(equal? (list 1 2 3) (list 1 2 3))
; #t

(equal? (list 1 2 3) (list 1 (list 2 3)))
; #f
