#lang racket/base

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (next test-divisor))]))

(define (next n)
    (cond [(= n 2) 3]
          [else (+ n 2)]))

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

(define (search-for-primes n)
    (if (even? n)
        (timed-prime-test (add1 n) 0)
        (timed-prime-test (+ n 2) 0)))

(search-for-primes 1000)
; before modification
; 1009 *** 3.0 microseconds
; 1013 *** 2.0
; 1019 *** 2.0

; after
; 1009 *** 1.0
; 1013 *** 1.0
; 1019 *** 2.0

(search-for-primes 10000)
; before modification
; 10007 *** 7.0
; 10009 *** 7.0
; 10037 *** 7.0

; after
; 10007 *** 4.0
; 10009 *** 4.0
; 10037 *** 4.0

(search-for-primes 100000)
; before modification
; 100003 *** 19.0
; 100019 *** 19.0
; 100043 *** 19.0

; after
; 100003 *** 13.0
; 100019 *** 12.0
; 100043 *** 12.0

(search-for-primes 1000000)
; before modification
; 1000003 *** 60.0
; 1000033 *** 60.0
; 1000037 *** 60.0

; after
; 1000003 *** 36.0
; 1000033 *** 36.0
; 1000037 *** 36.0

; We add a new step: `if`. So that's not half the steps. The rate is about 1.6.

; by checking n equals to two only once, we get half the time.
(define (smallest-divisor n) 
    (if (divides? n 2)
        2
        (find-divisor n 3)))

(define (find-divisor n test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (+ test-divisor 2))]))

(search-for-primes 1000)
; before modification
; 1009 *** 3.0 microseconds
; 1013 *** 2.0
; 1019 *** 2.0

; after
; 1009 *** 1.0
; 1013 *** 1.0
; 1019 *** 1.0

(search-for-primes 10000)
; before modification
; 10007 *** 7.0
; 10009 *** 7.0
; 10037 *** 7.0

; 10007 *** 4.0
; 10009 *** 3.0
; 10037 *** 3.0

(search-for-primes 100000)
; before modification
; 100003 *** 19.0
; 100019 *** 19.0
; 100043 *** 19.0

; 100003 *** 10.0
; 100019 *** 9.0
; 100043 *** 9.0

(search-for-primes 1000000)
; before modification
; 1000003 *** 60.0
; 1000033 *** 60.0
; 1000037 *** 60.0

; 1000003 *** 30.0
; 1000033 *** 30.0
; 1000037 *** 30.0
