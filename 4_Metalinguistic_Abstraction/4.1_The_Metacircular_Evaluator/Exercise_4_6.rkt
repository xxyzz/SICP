#lang racket/base

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let->combination exp env)
  (let ([var-list (map car (cadr exp))]
        [exp-list (map cadr (cadr exp))]
        [body (caddr exp)])
    (cons (make-lambda var-list body) exp-list)))

(define (eval-let exp env) (eval (eval-let exp env) env))
