#lang racket/base

(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

(define (compile-and-run-exp exp)
  (cadr exp))

(define (compile-and-run expression)
  (assemble
   (statements
    (compile expression 'val 'return the-empty-environment))
   eceval))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev arg1 arg2 compapp)
   eceval-operations ;; add compile-and-run?, compiled-and-run, compiled-and-run-exp
   '((assign compapp (label compound-apply))
     read-eval-print-loop
     (perform (op initialize-stack))
     ;; ...
     eval-dispatch
     ;; ...
     (test (op compile-and-run?) (reg exp)) ;; ***
     (branch (label compile-and-run)) ;; ***
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
     ;; ...
     compile-and-run ;; ***
     (assign val (op compile-and-run-exp) (reg exp))
     (assign val (op compile-and-run) (reg val))
     (goto (reg val)))))

;; test
(start-eceval)
;;EC-Eval input:
(+ 1 1)
(total-pushes = 8 maximum-depth = 5)

;;EC-Eval value:
2

;;EC-Eval input:
(compile-and-run (define (f n) (- n 1)))

(total-pushes = 0 maximum-depth = 0)

;;EC-Eval value:
ok

;;EC-Eval input:
(f 2)

(total-pushes = 5 maximum-depth = 3)

;;EC-Eval value:
1
