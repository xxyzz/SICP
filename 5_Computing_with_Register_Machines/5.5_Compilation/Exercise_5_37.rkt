#lang racket/base

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ([first-reg (car regs)])
        ;; don't check first-reg is actually needed by seq2
        ;; and modified by seq1
        (preserving (cdr regs)
                    (make-instruction-sequence
                     (list-union (list first-reg)
                                 (registers-needed seq1))
                     (list-difference (registers-modified seq1)
                                      (list first-reg))
                     (append `((save ,first-reg))
                             (statements seq1)
                             `((restore ,first-reg))))
                    seq2))))

(compile
 '(if (eq? "Винни-Пух" "der Große Steuermann")
      "Пока есть государство, нет свободы. Когда будет свобода, не будет государства."
      "Ein Volk, ein Reich, ein Führer.")
 'val
 'next)

;; intact version:
'((env)
  (env proc argl continue val)
  ((assign proc (op lookup-variable-value) (const eq?) (reg env))
   (assign val (const "der Große Steuermann"))
   (assign argl (op list) (reg val))
   (assign val (const "Винни-Пух"))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch4))
   compiled-branch5
   (assign continue (label after-call6))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch4
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call6
   (test (op false?) (reg val))
   (branch (label false-branch2))
   true-branch1
   (assign val (const "Пока есть государство, нет свободы. Когда будет свобода, не будет государства."))
   (goto (label after-if3))
   false-branch2
   (assign val (const "Ein Volk, ein Reich, ein Führer."))
   after-if3))

;; new version:
'((env continue)
  (proc argl val)
  ((save continue)
   (save env)
   (save continue)
   (save env)
   (save continue)
   (assign proc (op lookup-variable-value) (const eq?) (reg env))
   (restore continue)
   (restore env)
   (restore continue)
   (save continue)
   (save proc)
   (save env)
   (save continue)
   (assign val (const "der Große Steuermann"))
   (restore continue)
   (assign argl (op list) (reg val))
   (restore env)
   (save argl)
   (save continue)
   (assign val (const "Винни-Пух"))
   (restore continue)
   (restore argl)
   (assign argl (op cons) (reg val) (reg argl))
   (restore proc)
   (restore continue)
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch4))
   compiled-branch5
   (assign continue (label after-call6))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch4
   (save continue)
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (restore continue)
   after-call6
   (restore env)
   (restore continue)
   (test (op false?) (reg val))
   (branch (label false-branch2))
   true-branch1
   (save continue)
   (assign val (const "Пока есть государство, нет свободы. Когда будет свобода, не будет государства."))
   (restore continue)
   (goto (label after-if3))
   false-branch2
   (save continue)
   (assign val (const "Ein Volk, ein Reich, ein Führer."))
   (restore continue)
   after-if3))
