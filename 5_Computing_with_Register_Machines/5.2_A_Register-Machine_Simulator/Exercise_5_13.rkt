#lang racket/base

(define (make-machine ops controller-text) ;; remove register-names
  (let ([machine (make-new-machine)])
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; inside make-new-machine:
(define (lookup-register name)
  (let ([val (assoc name register-table)])
    (if val
        (cadr val)
        (begin ;; create new register if not exist
          (allocate-register name)
          (lookup-register name)))))

;; if each register has it's own stack:
;; fetch stacks when need it, otherwise it's null
(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [ops (machine 'operations)])
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag ops))) ;; remove stack
     insts)))

(define (make-execution-procedure
         inst labels machine pc flag ops) ;; remove stack
  (cond [(eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc)]
        [(eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc)]
        [(eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc)]
        [(eq? (car inst) 'goto)
         (make-goto inst machine labels pc)]
        [(eq? (car inst) 'save)
         (make-save inst machine pc)] ;; ***
        [(eq? (car inst) 'restore) (make-restore inst machine pc)] ;; ***
        [(eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc)]
        [else
         (error "Unknown instruction type: ASSEMBLE"
                inst)]))

(define (make-save inst machine pc) ;; ***
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stacks (machine 'stacks)] ;; ***
         [stack (find-reg-stack reg-name stacks)])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine pc) ;; ***
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stacks (machine 'stacks)] ;; ***
         [stack (find-reg-stack reg-name stacks)])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
