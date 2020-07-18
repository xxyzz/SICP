#lang racket/base

;; origin eval-sequence:
(define (eval-sequence exps env)
  (cond [(last-exp? exps)
         (eval (first-exp exps) env)]
        [else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env)]))

;; Cy's version:
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

;; a:
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
;; proc is evaluated before calling apply
;; items and x will be evaluated because car and cdr are
;; primitive procedures
;; newline and display are procedure not thunk
;; no need to use actual-value

;; b:
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p1 1)
;; both return '(1 2)
(p2 1)
;; thunk `e` will never be evaluated, origin code will return 1
;; new version will return '(1 2)

;; c:
;; they will be evaluated anyway, using actual-value doesn't matter

;; d:
;; cy's code is fine, if don't evaluate these thunks some side effect
;; procedures just meaningless
