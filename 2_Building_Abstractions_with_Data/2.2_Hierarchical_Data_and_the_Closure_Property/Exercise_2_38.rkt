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

(fold-right / 1 (list 1 2 3))
; (/ 1 (/ 2 (/ 3 1)))
; 3/2

(fold-left / 1 (list 1 2 3))
; (/ (/ (/ 1 1) 2) 3)
; 1/6

(fold-right list null (list 1 2 3))
; (list 1 (list 2 (list 3 null)))
; '(1 (2 (3 ())))

(fold-left list null (list 1 2 3))
; (list (list (list null 1) 2) 3)
; '(((() 1) 2) 3)

(fold-right * 1 (list 1 2 3))
; (* 1 (* 2 (* 3 1)))
; 6
(fold-left * 1 (list 1 2 3))
; (* (* (* 1 1) 2) 3)
; 6

(fold-right + 1 (list 1 2 3))
; (+ 1 (+ 2 (+ 3 1)))
; 7
(fold-left + 1 (list 1 2 3))
; (+ (+ (+ 1 1) 2) 3)
; 7
