#lang racket/base

;; use exercise 4.16's scan-out-defines
(define (compile-lambda-body exp proc-entry compile-env)
  (let ([formals (lambda-parameters exp)])
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  (assign env (op compiled-procedure-env) (reg proc))
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) ;; ***
                       'val
                       'return
                       (cons formals compile-env)))))
;; test
(define test
  (make-machine
   all-regs
   (list (list '+ +)
         (list 'make-compiled-procedure make-compiled-procedure)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'extend-environment extend-environment)
         (list 'lexical-address-lookup lexical-address-lookup)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'list list)
         (list 'cons cons)
         (list 'lexical-address-set! lexical-address-set!))
   (statements
    (compile
     '((lambda ()
         (define a 1)
         (define b 2)
         (+ a b)))
     'val
     'next
     the-empty-environment))))
(start test)
(get-register-contents test 'val)
;; 3
