#lang racket/base
(require math/number-theory) ; prime?

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append null (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda (i)
                (map (lambda (j) (list i j))
                    (enumerate-interval 1 (sub1 i))))
             (enumerate-interval 1 n)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 3)
; '((2 1 3) (3 2 5))
