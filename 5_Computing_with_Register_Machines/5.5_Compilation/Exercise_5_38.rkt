#lang racket/base

;; a:
(define all-regs '(env proc val argl continue arg1 arg2))

(define (spread-arguments operand-list)
  (list (compile (car operand-list) 'arg1 'next)
        (compile (cadr operand-list) 'arg2 'next)))

;; b:
(define (compile exp target linkage)
  (cond [(self-evaluating? exp)
         (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-quoted exp target linkage)]
        [(variable? exp)
         (compile-variable exp target linkage)]
        [(assignment? exp)
         (compile-assignment exp target linkage)]
        [(definition? exp)
         (compile-definition exp target linkage)]
        [(if? exp) (compile-if exp target linkage)]
        [(lambda? exp) (compile-lambda exp target linkage)]
        [(begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage)]
        [(cond? exp)
         (compile (cond->if exp) target linkage)]
        [(open-code? exp) ;; ***
         (compile-open-code exp target linkage)]
        [(application? exp)
         (compile-application exp target linkage)]
        [else
         (error "Unknown expression type: COMPILE" exp)]))

(define (open-code? exp)
  (memq (operator exp) '(= * - +)))

(define (compile-open-code exp target linkage)
  (let ([operand-codes (spread-arguments (operands exp))])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      (car operand-codes)
      (preserving
       '(arg1)
       (cadr operand-codes)
       (make-instruction-sequence
        '(arg1 arg2)
        (list target)
        `((assign ,target
                  (op ,(operator exp))
                  (reg arg1)
                  (reg arg2)))))))))

;; tests
(compile
 '(+ 1 2)
 'val
 'next)
'(()
  (arg1 arg2 val)
  ((assign arg1 (const 1))
   (assign arg2 (const 2))
   (assign val (op +) (reg arg1) (reg arg2))))

(compile
 '(+ (add1 a)
     (+ b 2))
 'val
 'next)
'((env)
  (proc argl continue arg1 arg2 val)
  ((save env)
   (assign proc (op lookup-variable-value) (const add1) (reg env))
   (assign val (op lookup-variable-value) (const a) (reg env))
   (assign argl (op list) (reg val))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch1))
   compiled-branch2
   (assign continue (label proc-return4))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   proc-return4
   (assign arg1 (reg val))
   (goto (label after-call3))
   primitive-branch1
   (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call3
   (restore env)
   (save arg1)
   (assign arg1 (op lookup-variable-value) (const b) (reg env))
   (assign arg2 (const 2))
   (assign arg2 (op +) (reg arg1) (reg arg2))
   (restore arg1)
   (assign val (op +) (reg arg1) (reg arg2))))

;; c:
(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)
'((env)
  (val)
  ((assign val (op make-compiled-procedure) (label entry1) (reg env))
   (goto (label after-lambda2))
   entry1
   (assign env (op compiled-procedure-env) (reg proc))
   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
   (assign arg1 (op lookup-variable-value) (const n) (reg env))
   (assign arg2 (const 1))
   (assign val (op =) (reg arg1) (reg arg2))
   (test (op false?) (reg val))
   (branch (label false-branch4))
   true-branch3
   (assign val (const 1))
   (goto (reg continue))
   false-branch4
   (save continue)
   (save env)
   (assign proc (op lookup-variable-value) (const factorial) (reg env))
   (assign arg1 (op lookup-variable-value) (const n) (reg env))
   (assign arg2 (const 1))
   (assign val (op -) (reg arg1) (reg arg2))
   (assign argl (op list) (reg val))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch6))
   compiled-branch7
   (assign continue (label proc-return9))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   proc-return9
   (assign arg1 (reg val))
   (goto (label after-call8))
   primitive-branch6
   (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call8
   (restore env)
   (assign arg2 (op lookup-variable-value) (const n) (reg env))
   (assign val (op *) (reg arg1) (reg arg2))
   (restore continue)
   (goto (reg continue))
   after-if5
   after-lambda2
   (perform (op define-variable!) (const factorial) (reg val) (reg env))
   (assign val (const ok))))
