#lang racket/base

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (cond [(= amount 0) 1]
          [(or (< amount 0) (no-more? coin-values)) 0]
          [else
            (+ (cc amount
                    (except-first-denomination coin-values))
                (cc (- amount
                        (first-denomination coin-values))
                    coin-values))]))

(define (no-more? coin-list)
    (null? coin-list))

(define (first-denomination coin-list)
    (car coin-list))

(define (except-first-denomination coin-list)
    (cdr coin-list))

(cc 100 us-coins)
; 292

(define reverse-us-coins (list 1 5 10 25 50))
(cc 100 reverse-us-coins)
; 292

(define random-us-coins (list 10 5 1 50 25))
(cc 100 random-us-coins)
; 292

; cc will try all the combination of coins no matter what order they are in.
