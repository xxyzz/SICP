#lang racket/base

; origin version
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; evaluates from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ([left (eval (first-operand exps) env)]
            [right (list-of-values (rest-operands exps) env)])
        (cons left right))))

; evaluates from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ([right (list-of-values (rest-operands exps) env)]
            [left (eval (first-operand exps) env)])
        (cons left right))))
