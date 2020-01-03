#lang racket/base

; recursive
(define (cont-frac n d k)
    (define (recur m)
        (if (> m k)
            0
            (/ (n m) (+ (d m) (recur (add1 m))))))
(recur 1))

; iterative
(define (cont-frac n d k)
    (define (iter result m)
        (if (< m 1)
            result
            (iter (/ (n m) (+ (d m) result)) (sub1 m)))) ; wow!
(iter (/ (n k) (d k)) (sub1 k)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
; 0.6179775280898876

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
; 0.6180555555555556
