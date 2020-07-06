#lang racket/base

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless->if exp)
  (make-if (cadr exp)
           (cadddr exp)
           (caddr exp)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'map map)))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(unless? exp) (analyze (unless->if exp))]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(analyze-eval '(define (factorial n)
                 (unless (= n 1)
                   (* n (factorial (- n 1))) 1))
              the-global-environment)
;; 'ok
(analyze-eval '(factorial 3) the-global-environment)
;; 6
(analyze-eval '(map unless '(#t #f #t) '(1 2 3) '(4 5 6)) the-global-environment)
;; Unbound variable unless
;; `unless` is not a procedure
