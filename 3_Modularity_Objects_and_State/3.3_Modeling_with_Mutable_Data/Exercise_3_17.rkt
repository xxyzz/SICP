#lang racket/base

(define (find-in-list item l)
  (cond [(null? l) #f]
        [(eq? item (mcar l)) #t]
        [else (find-in-list item (mcdr l))]))

(define (count-pairs x)
  (define (iter pair counted-list count)
    (cond [(null? pair) count]
          [(find-in-list (mcar pair) counted-list) count]
          [(not (mpair? pair)) 0]
          [else (iter (mcdr pair) (mcons (mcar pair) counted-list) (add1 count))]))
  (iter x null 0))

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(count-pairs (mcons 'a (mcons 'b (mcons 'c null))))
; 3

(define count-4-list (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcar! count-4-list (last-pair count-4-list))
(count-pairs count-4-list)
; 3

(define count-7-list (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcar! count-7-list (mcdr count-7-list))
(set-mcar! (mcdr count-7-list) (last-pair count-7-list))
(count-pairs count-7-list)
; 3

(define infinite-list (mcons 'a (mcons 'b (mcons 'c null))))
(let ([last (last-pair count-4-list)])
      (set-mcdr! last last))
(count-pairs infinite-list)
; 3
