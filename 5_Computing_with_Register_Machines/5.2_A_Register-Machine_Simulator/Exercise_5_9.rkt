#lang racket/base

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
                (if (and (not (register-exp? e))
                         (not (constant-exp? e)))
                    (error "Invalid operation argument: ASSEMBLE")
                    (make-primitive-exp e  machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
