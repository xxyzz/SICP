#lang racket/base
(require racket/math) ; pi

; recursive
(define (cont-frac n d k)
    (define (recur m)
        (if (> m k)
            0
            (/ (n m) (- (d m) (recur (add1 m))))))
(recur 1))

; iterative
(define (cont-frac n d k)
    (define (iter result m)
        (if (< m 1)
            result
            (iter (/ (n m) (- (d m) result)) (sub1 m))))
(iter (/ (n k) (d k)) (sub1 k)))

(define (tan-cf x k)
    (cont-frac  (lambda (i)
                    (if (= i 1)
                        x
                        (* x x)))
                (lambda (i)
                    (- (* 2 i) 1))
                k))

(tan-cf (/ pi 4) 9)
; 1.0

(tan-cf (/ pi 2) 11)
; +inf.0
