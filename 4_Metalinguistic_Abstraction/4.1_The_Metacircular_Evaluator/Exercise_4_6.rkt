#lang racket/base

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let->combination exp)
  (let* ([bindings (cadr exp)]
         [var-list (map car bindings)]
         [exp-list (map cadr bindings)]
         [body (cddr exp)])
    (cons (make-lambda var-list body) exp-list)))
; see exercise 2.26 for the difference of cons and list

(define (eval-let exp env) (eval (let->combination exp) env))
