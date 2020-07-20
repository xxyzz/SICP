#lang racket/base

(define (text-of-quotation exp env)
  (define (iter vars)
    (if (null? vars)
        null
        (list 'cons (list 'quote (car vars)) (iter (cdr vars)))))
  (let ([value (cadr exp)])
    (if (pair? value)
        (eval (iter value) env)
        value)))

;; edit eval quoted? branch

;; input
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(car '(a b c))
;; a
