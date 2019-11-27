#lang racket/base

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (add1 test-divisor))]))

(define (divides? a b) (zero? (remainder b a)))
(define (square x) (* x x)) 

(define (prime? n)
    (= n (smallest-divisor n)))

(define (timed-prime-test n count)
    (start-prime-test n (* (current-inexact-milliseconds) 1000) count))

(define (start-prime-test n start-time count)
    (if (prime? n)
        (report-prime (- (* (current-inexact-milliseconds) 1000) start-time) n count)
        (timed-prime-test (+ n 2) count)))

(define (report-prime elapsed-time n count)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (when (< count 2)
        (timed-prime-test (+ n 2) (add1 count))))

; find the three smallest primes larger than n
(define (search-for-primes n)
    (if (even? n)
        (timed-prime-test (add1 n) 0)
        (timed-prime-test (+ n 2) 0)))

(search-for-primes 1000)
; 1009 *** 3.0 microseconds
; 1013 *** 2.0
; 1019 *** 2.0

(search-for-primes 10000)
; 10007 *** 7.0
; 10009 *** 7.0
; 10037 *** 7.0

(search-for-primes 100000)
; 100003 *** 19.0
; 100019 *** 19.0
; 100043 *** 19.0

(search-for-primes 1000000)
; 1000003 *** 60.0
; 1000033 *** 60.0
; 1000037 *** 60.0

; (3 + 2 + 2) / 3 * √10 ≈ 7.379
; 7  * √10 ≈ 22.136
; 19 * √10 ≈ 60.083
