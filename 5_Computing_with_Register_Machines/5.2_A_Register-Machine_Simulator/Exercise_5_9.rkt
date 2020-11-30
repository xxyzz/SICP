#lang racket/base

(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp)
                         operations)]
        [aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Invalid operation argument: ASSEMBLE")))
              (operation-exp-operands exp))])
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
