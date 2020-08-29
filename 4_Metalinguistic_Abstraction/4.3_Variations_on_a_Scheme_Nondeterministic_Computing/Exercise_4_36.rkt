#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (pythagorean-triple)
  (let* ([k (an-integer-starting-from 1)]
         [j (an-integer-between 1 k)]
         [i (an-integer-between 1 j)])
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

(define (a-pythagorean-triple)
  (let* ([i (an-integer-starting-from 1)]
         [j (an-integer-starting-from i)]
         [k (an-integer-starting-from j)]) ;; struck here with i and j are 1
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

(pythagorean-triple)
;; (3 4 5)
