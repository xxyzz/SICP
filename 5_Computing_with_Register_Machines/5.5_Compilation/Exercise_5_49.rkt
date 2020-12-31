#lang racket/base

(define eceval
  (make-machine
   all-regs
   eceval-operations
   '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;EC-Eval input:"))
     (assign val (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op print-stack-statistics)) ;; added instruction
     (perform (op announce-output) (const ";;EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     eval-dispatch ;; ***
     (assign val (op compile-and-run) (reg val))
     (goto (reg val)))))
