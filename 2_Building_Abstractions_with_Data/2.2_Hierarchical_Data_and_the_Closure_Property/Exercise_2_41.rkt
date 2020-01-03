#lang racket/base

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

(define (unique-triples n)
    (flatmap (lambda (i)
                (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval 1 (sub1 j))))
                         (enumerate-interval 1 (sub1 i))))
             (enumerate-interval 1 n)))

(define (equal-sum-pairs n s)
    (filter (lambda (triple)
                (= (accumulate + 0 triple) s))
            (unique-triples n)))

(unique-triples 4)
; '((3 2 1) (4 2 1) (4 3 1) (4 3 2))
(equal-sum-pairs 4 6)
; '((3 2 1))
