#lang racket/base

;; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "variable unassigned")
                 (car vals))]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; b
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (scan-out-defines body)
  (define (body-loop exps vars vals rest-exps)
    (cond [(null? exps)
           (if (null? vars)
               rest-exps
               (list (make-let
                      (map (lambda (x) (list x ''*unassigned*)) vars)
                      (append (map (lambda (x y) (list 'set! x y)) vars vals)
                              rest-exps))))]
          [(definition? (car exps))
           (let* ([current-exp (car exps)]
                  [var (definition-variable current-exp)]
                  [val-list (definition-value current-exp)])
             (body-loop (cdr exps)
                        (if (null? vars)
                            (list var)
                            (append vars (list var)))
                        (if (null? vals)
                            (list val-list)
                            (append vals (list val-list)))
                        rest-exps))]
          [else (body-loop (cdr exps)
                           vars
                           vals
                           (if (null? rest-exps)
                               (list (car exps))
                               (append rest-exps (list (car exps)))))]))
  (body-loop body null null null))

;; test
(scan-out-defines '((define a 1) (define b 2) (+ a b)))
;; '((let ((a '*unassigned*) (b '*unassigned*)) (set! a 1) (set! b 2) (+ a b)))
(scan-out-defines '((define (derp x) (add1 x)) (derp 1)))
;; '((let ((derp '*unassigned*)) (set! derp (lambda (x) (add1 x))) (derp 1)))

;; c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
