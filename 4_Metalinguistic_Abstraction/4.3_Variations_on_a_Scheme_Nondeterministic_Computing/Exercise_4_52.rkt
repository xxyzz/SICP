#lang racket/base

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (analyze-if-fail exp)
  (let ([first-exp (analyze (cadr exp))]
        [second-exp (analyze (caddr exp))])
    (lambda (env succeed fail)
      (first-exp env
                 succeed
                 (lambda ()
                   (second-exp env
                               succeed
                               fail))))))

;; add even? to primitive-procedures
;; add if-fail? to analyze
