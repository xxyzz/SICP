#lang racket/base

;; a
;; tree recursive
((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (fib k)
      (cond [(zero? k) 0]
            [(= k 1) 1]
            [else (+ (fib fib (- k 1))
                     (fib fib (- k 2)))]))))
 5)
;; 5

;; linear recursive
((lambda (n)
   ((lambda (fib) (fib fib 1 0 n))
    (lambda (fib a b k)
      (if (zero? k)
          b
          (fib fib (+ a b) a (sub1 k))))))
 5)
;; 5

;; b
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (sub1 n))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (sub1 n))))))
(f 3)
;; #f
