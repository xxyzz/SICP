#lang racket/base

;; the ultimate version of Fibonacci procedure(exercise 1.19)
;; uses several times of it's parameters, it will benefit
;; from this memorization
;; see exercise 3.27 for another memoization approach

(define (fib n)
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
  (fib-iter 1 0 0 1 n))

(define (user-print object)
  (define start (current-inexact-milliseconds))
  (if (compound-procedure? object)
      (displayln (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)
                       '<procedure-env>))
      (displayln object))
  (define end (current-inexact-milliseconds))
  (displayln (format "ms: ~a" (- end start))))

;; no memorize
(fib 1000)
;; ms: 0.27294921875
(fib 10000)
;; wait for it if you have nothing to do

;; memorize
(fib 1000)
;; ms: 0.090087890625
(fib 10000)
;; ms: 0.1337890625

(define (square x) (* x x))
;; square runs faster with memoization enabled
;; x is evaluated only once

;; no memorize
(square (id 10))
;; 100
count
;; 2

;; memorize
(square (id 10))
;; 100
count
;; 1

