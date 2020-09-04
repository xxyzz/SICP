#lang racket/base
(require math/number-theory) ;; prime?
;; https://github.com/racket/math/blob/master/math-lib/math/private/number-theory/number-theory.rkt

;; add prime? to primitive-procedures

(define (require p) (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
(define (prime-sum-pair list1 list2)
  (let ([a (an-element-of list1)]
        [b (an-element-of list2)])
    (require (prime? (+ a b)))
    (list a b)))
(let ([pairs '()])
  (if-fail
   (let ([p (prime-sum-pair '(1 3 5 8)
                            '(20 35 110))])
     (permanent-set! pairs (cons p pairs))
     (amb))
   pairs))

;; ((8 35) (3 110) (3 20))
