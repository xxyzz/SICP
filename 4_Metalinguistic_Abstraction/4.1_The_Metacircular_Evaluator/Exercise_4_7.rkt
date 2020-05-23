#lang racket/base

(let* ([x 3]
       [y (+ x 2)]
       [z (+ x y 5)])
  (* x z))

; transform to nested lets:
(let ([x 3])
  (let ([y (+ x 2)])
    (let ([z (+ x y 5)])
      (* x z))))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

; derived expression
(define (let*->nested-lets exp)
  (let ([bindings (cadr exp)]
        [body (cddr exp)])
    (define (iter bindings-list)
      (if (null? bindings-list)
          body
          (make-let (car bindings-list) (iter (cdr bindings-list)))))
    (iter bindings)))

(define (eval-let* exp env) (eval (let*->nested-lets exp) env))

((lambda (x)
   ((lambda (y)
      ((lambda (z)
         (* x z))
       (+ x y 5)))
    (+ x 2)))
 3)

; I think explicitly expand let* is same as nested let expressions.
