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
  (define (body-loop b vars vals restp)
    (cond [(null? b)
           (if (null? vars)
               restp
               (make-let (map (lambda (x) (list x '*unassigned*)) vars)
                         (append (map (lambda (x y) (list 'set! x y)) vars vals)
                                 restp)))]
          [(and (pair? (car b))
                (= (length (car b)) 3)
                (eq? 'define (caar b)))
           (body-loop (cdr b)
                      (cons (cadar b) vars)
                      (cons (caddar b) vals)
                      restp)]
          [else (body-loop (cdr b)
                           vars
                           vals
                           (if (null? rest-exp)
                               (list (car body))
                               (append rest-exp (list (car body)))))]))
  (body-loop body null null null))

;; test
(scan-out-defines '((define a 1) (define b 2) (+ a b)))
;; '(let ((b *unassigned*) (a *unassigned*)) (set! b 2) (set! a 1) (+ a b))

;; c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
