#lang racket/base

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define (find-in-list item l)
  (cond [(null? l) #f]
        [(eq? item (mcar l)) #t]
        [else (find-in-list item (mcdr l))]))

(define (contain-cycle x)
  (define (iter pair counted-list)
    (cond [(null? pair) #f]
          [(find-in-list (mcar pair) counted-list) #t]
          [else (iter (mcdr pair) (mcons (mcar pair) counted-list))]))
  (iter x null))

(define z (make-cycle (mcons 'a (mcons 'b (mcons 'c null)))))
(define y (mcons 'a (mcons 'b (mcons 'c null))))

(contain-cycle y)
; #f
(contain-cycle z)
; #t
