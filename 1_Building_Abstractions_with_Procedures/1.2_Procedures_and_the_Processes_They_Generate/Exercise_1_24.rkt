#lang racket/base

(define (square x) (* x x))

; (x * y) % m = [(x % m) * (y % m)] % m
; so when exp is even, base^exp % m = (base^(exp / 2) % m)^2 % m
; (3^4) % 5  = 81 % 5 = 1
; (3^2 % 5)^2 % 5 = (9 % 5)^2 % 5 = 4^2 % 5 = 16 % 5 = 1
(define (expmod base exp m)
    (cond [(= exp 0) 1]
          [(even? exp)
              (remainder
                  (square (expmod base (/ exp 2) m))
                  m)]
          [else
              (remainder
                  (* base (expmod base (- exp 1) m))
                  m)]))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
(try-it (add1 (random (sub1 n)))))

(define (fast-prime? n times)
    (cond [(zero? times) #t]
          [(fermat-test n) (fast-prime? n (sub1 times))]
          [else #f]))

(define (timed-prime-test n count)
    (start-prime-test n (* (current-inexact-milliseconds) 1000) count))

(define (start-prime-test n start-time count)
    (if (fast-prime? n 3)
        (report-prime (- (* (current-inexact-milliseconds) 1000) start-time) n count)
        (timed-prime-test (+ n 2) count)))

(define (report-prime elapsed-time n count)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (when (< count 2)
        (timed-prime-test (+ n 2) (add1 count))))

(define (search-for-primes n)
    (if (even? n)
        (timed-prime-test (add1 n) 0)
        (timed-prime-test (+ n 2) 0)))

(search-for-primes 1000)
; 1009 *** 4.0 microseconds
; 1013 *** 3.0
; 1019 *** 4.0

(search-for-primes 1000000)
; 1000003 *** 8.0 microseconds
; 1000033 *** 6.0
; 1000037 *** 6.0

; log(1000000) / log(1000) = log(1000000) (base 1000) = 2
