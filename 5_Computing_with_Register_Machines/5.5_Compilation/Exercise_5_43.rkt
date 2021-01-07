#lang racket/base

(define (scan-out-defines body)
  (define (body-loop exp vars vals rest-exp)
    (cond [(null? exp)
           (if (null? vars)
               rest-exp
               (list
                (cons
                 (make-lambda vars
                              (append (map (lambda (x y)
                                             (list 'set! x y))
                                           vars
                                           vals)
                                      rest-exp))
                 (map (lambda (x) ''*unassigned*) vars))))]
          [(definition? (car exp))
           (let* ([current-exp (car exp)]
                  [value (definition-value current-exp)])
             (body-loop (cdr exp)
                        (cons (definition-variable current-exp) vars)
                        (if (null? vals)
                            (list value)
                            (cons vals (list value)))
                        rest-exp))]
          [else (body-loop (cdr exp)
                           vars
                           vals
                           (if (null? rest-exp)
                               (list (car exp))
                               (append rest-exp (list (car exp)))))]))
  (body-loop body null null null))

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
(scan-out-defines '((define a 1) (define b 2) (+ a b) (- a b)))
;; '(((lambda (b a) (set! b 2) (set! a 1) (+ a b) (- a b))
;;   '*unassigned* '*unassigned*))
(scan-out-defines '((define (derp x) (add1 x)) (derp 1)))
;; '(((lambda (derp)
;;      (set! derp (lambda (x) (add1 x)))
;;      (derp 1)) '*unassigned*))

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
