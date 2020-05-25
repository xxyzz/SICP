#lang racket/base

(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
; named let is equivalent to:
(let ⟨bindings⟩
  (define (⟨var⟩ bindings-var-list) body)
  (⟨var⟩ bindings-var-list))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (make-define id expr)
  (cons 'define (cons id expr)))

(define (let->combination exp)
  (let* ([bindings (let-bindings exp)]
         [var-list (map car bindings)]
         [exp-list (map cadr bindings)]
         [body (let-body exp)])
    (if (named-let? exp)
        (make-let bindings
                  (list (make-define (cons (cadr exp) var-list) body)
                        (cons (cadr exp) var-list)))
        (cons (make-lambda var-list body) exp-list))))

(define (eval-let exp env) (eval (let->combination exp) env))

(let->combination '(let test ((a 0) (b 1)) (+ a b)))
; '(let ((a 0) (b 1)) (define (test a b) (+ a b)) (test a b))

(let->combination '(let ((a 0) (b 1)) (+ a b)))
; '((lambda (a b) (+ a b)) 0 1)
