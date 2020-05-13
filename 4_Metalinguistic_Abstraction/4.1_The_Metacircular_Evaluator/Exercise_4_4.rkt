#lang racket/base

; special forms:
(define (and-expressions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (eval-and-exps exps env)
  (if (null? exps)
      #t
      (let ([current-val (eval (first-exp exps) env)])
        (cond [(not (true? current-val)) #f]
              [(last-exp? exps) current-val]
              [else (eval-and-exps (rest-exps exps) env)]))))
(define (eval-and exp env)
  (eval-and-exps (and-expressions exp) env))

(define (or-expressions exp) (cdr exp))
(define (eval-or-exps exps env)
  (if (null? exps)
      #f
      (let ([current-val (eval (first-exp exps) env)])
        (cond [(true? current-val) current-val]
              [(last-exp? exps) #f]
              [else (eval-or-exps (rest-exps exps) env)]))))
(define (eval-or exp env)
  (eval-or-exps (or-expressions exp) env))

; derived expressions:
