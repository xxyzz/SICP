#lang racket/base

(define (open-code? exp compile-env)
  (let* ([var (operator exp)]
         [address (find-variable var compile-env)])
    (if (eq? address 'not-found)
        (memq var '(= * - +))
        #f)))

;; test
(define test
  (make-machine
   all-regs
   (list (list '- -)
         (list 'make-compiled-procedure make-compiled-procedure)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'extend-environment extend-environment)
         (list 'lexical-address-lookup lexical-address-lookup)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'get-global-environment get-global-environment)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'list list)
         (list 'cons cons)
         (list 'lexical-address-set! lexical-address-set!))
   (statements
    (compile
     '((lambda (+ a b)
         (+ a b))
       - 1 2)
     'val
     'next
     the-empty-environment))))
(start test)
(get-register-contents test 'val)
;; -1
