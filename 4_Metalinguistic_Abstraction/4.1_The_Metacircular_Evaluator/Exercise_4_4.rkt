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
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and->if exp) (expand-and-expressions (and-expressions exp)))
(define (expand-and-expressions exps)
  (if (null? exps)
      #t
      (make-if (cons not (first-exp exps))
               #f
               (if (last-exp? exps)
                   (first-exp exps)
                   (expand-and-expressions (rest-exps exps))))))
(define (eval-and exp env)
  (eval (and->if exp) env))

(define (or->if exp) (expand-or-expressions (or-expressions exp)))
(define (expand-or-expressions exps)
  (if (null? exps)
      #f
      (make-if (first-exp exps)
               (first-exp exps)
               (if (last-exp? exps)
                   #f
                   (expand-or-expressions (rest-exps exps))))))
(define (eval-or exp env)
  (eval (or->if exp) env))
