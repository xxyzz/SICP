#lang racket/base

;; a:
(define (lookup-variable-value var env)
  (travsersing-env
   (lambda (env) (lookup-variable-value var (enclosing-environment env)))
   (lambda (current-pair) (frame-unit-value current-pair))
   (lambda (var) (list 'unbound-variable-error var)) ;; ***
   env
   var))

(define (unbound-error? val) ;; add it to eceval-operations
  (tagged-list? val 'unbound-variable-error))

ev-variable
(assign val (op lookup-variable-value) (reg exp) (reg env))
(test (op unbound-error?) (reg val)) ;; ***
(branch (label signal-error))
(goto (reg continue))

;; b:
(define (apply-in-underlying-scheme proc args)
  (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) (list 'primitive-error exn))])
    (apply proc args)))

(define (primitive-error? val) ;; add it to eceval-operations
  (tagged-list? val 'primitive-error))

primitive-apply
(assign val (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
(test (op primitive-error?) (reg val)) ;; ***
(branch (label signal-error))
(restore continue)
(goto (reg continue))
