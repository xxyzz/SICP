#lang racket/base

eval-dispatch
(test (op self-evaluating?) (reg exp))
(branch (label ev-self-eval))
(test (op variable?) (reg exp))
(branch (label ev-variable))
(test (op quoted?) (reg exp))
(branch (label ev-quoted))
(test (op assignment?) (reg exp))
(branch (label ev-assignment))
(test (op definition?) (reg exp))
(branch (label ev-definition))
(test (op if?) (reg exp))
(branch (label ev-if))
(test (op lambda?) (reg exp))
(branch (label ev-lambda))
(test (op begin?) (reg exp))
(branch (label ev-begin))
(test (op cond?) (reg exp)) ;; ***
(branch (label ev-cond))
(test (op let?) (reg exp)) ;; ***
(branch (label ev-let))
(test (op application?) (reg exp))
(branch (label ev-application))
(goto (label unknown-expression-type))

ev-cond
(assign exp (op cond->if) (reg exp)) ;; page 506
(goto (label eval-dispatch))

ev-let
(assign exp (op let->combination) (reg exp)) ;; Exercise 4.6
(goto (label ev-application))

;; tests
(cond [(= 1 1) 1]
      [else 2])
;; 1

(cond [else 2])
;; 2
(define a 1)
(cond [(= a 1) a]
      [(= 1 1) 1]
      [else a])

(let ([a 1]
      [b 2])
  (+ a b))
;; 3
