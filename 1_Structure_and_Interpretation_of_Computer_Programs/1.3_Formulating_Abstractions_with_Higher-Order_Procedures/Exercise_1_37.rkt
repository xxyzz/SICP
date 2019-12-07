#lang racket/base

; recursive
(define (cont-frac n d k)
    (if (< k 1)
        0
        (/ (n k) (+ (d k) (cont-frac n d (sub1 k))))))

; iterative
(define (cont-frac n d k)
    (define (iter result m)
        (if (< m 2)
            result
            (iter (/ (n k) (+ (d k) result)) (sub1 m)))) ; wow!
(iter (/ (n k) (d k)) k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
; 0.6179775280898876

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
; 0.6180555555555556
