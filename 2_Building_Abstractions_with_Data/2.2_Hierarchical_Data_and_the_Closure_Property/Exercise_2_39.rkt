#lang racket/base

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))

(define (reverse-right sequence)
    (fold-right (lambda (x y) (append y (list x)))
                null
                sequence))

(define (reverse-left sequence)
    (fold-left (lambda (x y) (cons y x))
               null
               sequence))

(reverse-right (list 1 2 3))
; (append (append (append null (list 3)) (list 2)) (list 1))
; '(3 2 1)
(reverse-left (list 1 2 3))
; (cons 3 (cons 2 (cons 1 null)))
; '(3 2 1)
