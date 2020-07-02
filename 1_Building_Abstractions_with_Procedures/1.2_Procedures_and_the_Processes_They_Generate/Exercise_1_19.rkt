#lang racket/base

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond [(= count 0) b]
        [(even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))   ;; compute p′
                   (+ (* 2 p q) (* q q)) ;; compute q′
                   (/ count 2))]
        [else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))]))

(fib 3)
;; 2

;; T = | p + q, q |
;;     | q    , p |
;;
;; T * |a| = | bq + aq + ap |
;;     |b|   | bp + aq      |
;;
;; | 1 1 | * |a| = | a + b |
;; | 1 0 |   |b|   | a     |
;;
;; T^2 = | (p + q)^2 + q^2, 2pq + q^2 |
;;       | 2pq + q^2      , q^2 + p^2 |
