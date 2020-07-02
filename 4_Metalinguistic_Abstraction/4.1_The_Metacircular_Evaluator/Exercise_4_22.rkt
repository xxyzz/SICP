#lang racket/base

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (analyze-let exp)
  (let* ([bindings (cadr exp)]
         [var-list (map car bindings)]
         [exp-list (map cadr bindings)]
         [body (cddr exp)]
         [let->lambda (analyze (cons (make-lambda var-list body) exp-list))])
    (lambda (env)
      (let->lambda env))))
