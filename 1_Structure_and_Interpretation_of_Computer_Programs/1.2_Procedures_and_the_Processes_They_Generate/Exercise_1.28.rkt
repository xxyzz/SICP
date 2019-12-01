#lang racket/base

(define (square x) (* x x))

(define (expmod base exp m)
    (cond [(= exp 0) 1]
          [(even? exp)
              (check-nontrivial-root
                  (expmod base (/ exp 2) m)
                  m)]
          [else
              (remainder
                  (* base (expmod base (- exp 1) m))
                  m)]))

(define (check-nontrivial-root x m)
    (if (and
            (not (= x 1))
            (not (= x (sub1 m)))
            (= (remainder (square x) m) 1))
        0
        (remainder (square x) m)))

(define (miller–rabin-test n)
    (define (try-it a)
        (= (expmod a n n) a))
(try-it (add1 (random (sub1 n)))))

(define (test-prime? n times)
    (cond [(zero? times) #t]
          [(miller–rabin-test n) (test-prime? n (sub1 times))]
          [else #f]))

(test-prime? 3 3)
; #t

(test-prime? 4 3)
; #f

(test-prime? 561 3)
; #f

(test-prime? 1105 3)
; #f

(test-prime? 1729 3)
; #f

(test-prime? 2465 3)
; #f

(test-prime? 2821 3)
; #f

(test-prime? 6601 3)
; #f
