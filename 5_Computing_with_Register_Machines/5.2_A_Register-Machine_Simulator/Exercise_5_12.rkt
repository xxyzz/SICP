#lang racket/base

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-machine register-names ops controller-text)
  (let ([machine (make-new-machine)])
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine)) machine))

(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value) (set! contents value))]
            [else
             (error "Unknown request: REGISTER" message)]))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ([s '()])
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ([top (car s)])
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'initialize) (initialize)]
            [else (error "Unknown request: STACK" message)]))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks null]
        [the-instruction-sequence '()]
        [sorted-insts null] ;; new lists
        [entry-point-regs null]
        [saved-restored-regs null]
        [reg-sources null])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (for-each (lambda (stack)
                                     (stack 'initialize))
                                   stacks))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! stacks
                    (cons (cons name (make-stack))
                          stacks))
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))))
        'register-allocated)
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register)
               allocate-register]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops)))]
              [(eq? message 'stacks) stacks]
              [(eq? message 'operations) the-ops]
              [(eq? message 'sorted-insts) sorted-insts] ;; get and set new lists
              [(eq? message 'set-sorted-insts!)
               (lambda (inst-list)
                 (set! sorted-insts inst-list))]
              [(eq? message 'entry-point-regs) entry-point-regs]
              [(eq? message 'set-entry-point-regs!)
               (lambda (entry-point-list)
                 (set! entry-point-regs entry-point-list))]
              [(eq? message 'saved-restored-regs) saved-restored-regs]
              [(eq? message 'set-saved-restored-regs!)
               (lambda (saved-restored-list)
                 (set! saved-restored-regs saved-restored-list))]
              [(eq? message 'reg-sources) reg-sources]
              [(eq? message 'set-reg-sources!)
               (lambda (reg-source-list)
                 (set! reg-sources reg-source-list))]
              [else (error "Unknown request: MACHINE"
                           message)]))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ([next-inst (car text)])
           (if (symbol? next-inst)
               (begin
                 (duplicate-label? next-inst labels)
                 (receive insts
                     (cons (make-label-entry next-inst
                                             insts)
                           labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (duplicate-label? label-name labels)
  (when (assoc label-name labels)
    (error "Duplicated label: ASSEMBLE"
           label-name)))

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stacks (machine 'stacks)]
        [ops (machine 'operations)])
    (for-each
     (lambda (inst)
       ;; add instruction to sorted instructions list
       (let ([sorted-insts (machine 'sorted-insts)])
         (when (not (member inst sorted-insts))
           (let* ([new-insts (cons inst sorted-insts)]
                  [new-sorted-insts
                   (sort new-insts
                         (lambda (x y)
                           (string<?
                            (symbol->string (car x))
                            (symbol->string (car y))))
                         #:key mcar)])
             ((machine 'set-sorted-insts!) new-sorted-insts))))
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stacks ops)))
     insts)))

(define (make-instruction text) (mcons text null))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond [(eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc)]
        [(eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc)]
        [(eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc)]
        [(eq? (car inst) 'goto)
         (make-goto inst machine labels pc)]
        [(eq? (car inst) 'save)
         (make-save inst machine stack pc)]
        [(eq? (car inst) 'restore) (make-restore inst machine stack pc)]
        [(eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc)]
        [else
         (error "Unknown instruction type: ASSEMBLE"
                inst)]))

(define (make-assign inst machine labels operations pc)
  (let* ([reg-name (assign-reg-name inst)]
         [target
          (get-register machine reg-name)]
         [value-exp (assign-value-exp inst)]
         [reg-sources (machine 'reg-sources)]
         [target-sources (assoc reg-name reg-sources)]
         [new-reg-sources
          (cond [(not target-sources)
                 (cons (list reg-name value-exp)
                       reg-sources)]
                [(not (member value-exp (cdr target-sources)))
                 (cons (list reg-name
                             (cons value-exp (cdr target-sources)))
                       (remove target-sources reg-sources))]
                [else null])])
    ;; add register value source
    (when (not (null? new-reg-sources))
      ((machine 'set-reg-sources!) new-reg-sources))
    (let ([value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))])
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
        (let ([condition-proc
               (make-operation-exp
                condition machine labels operations)])
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
        (let ([insts
               (lookup-label
                labels
                (label-exp-label dest))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond [(label-exp? dest)
           (let ([insts (lookup-label
                         labels
                         (label-exp-label dest))])
             (lambda () (set-contents! pc insts)))]
          [(register-exp? dest)
           (let* ([reg-name (register-exp-reg dest)]
                  [reg (get-register
                        machine
                        reg-name)]
                  [entry-point-regs (machine 'entry-point-regs)])
             ;; add entry point register to list
             (when (not (assoc reg-name entry-point-regs))
               ((machine 'set-entry-point-regs!)
                (cons (list reg-name reg)
                      entry-point-regs)))
             (lambda ()
               (set-contents! pc (get-contents reg))))]
          [else (error "Bad GOTO instruction: ASSEMBLE" inst)])))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (find-reg-stack reg-name stacks)
  (let ([stack-pair (assoc reg-name stacks)])
    (cdr stack-pair)))

(define (make-save inst machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stack (find-reg-stack reg-name stacks)]
         [saved-restored-regs (machine 'saved-restored-regs)])
    ;; add saved register to list
    (when (not (assoc reg-name saved-restored-regs))
      ((machine 'set-saved-restored-regs!)
       (cons (list reg-name reg)
             saved-restored-regs)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stack (find-reg-stack reg-name stacks)]
         [saved-restored-regs (machine 'saved-restored-regs)])
    ;; add restored register to list
    (when (not (assoc reg-name saved-restored-regs))
      ((machine 'set-saved-restored-regs!)
       (cons (list reg-name reg)
             saved-restored-regs)))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
        (let ([action-proc
               (make-operation-exp
                action machine labels operations)])
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond [(constant-exp? exp)
         (let ([c (constant-exp-value exp)])
           (lambda () c))]
        [(label-exp? exp)
         (let ([insts (lookup-label
                       labels
                       (label-exp-label exp))])
           (lambda () insts))]
        [(register-exp? exp)
         (let ([r (get-register machine (register-exp-reg exp))])
           (lambda () (get-contents r)))]
        [else (error "Unknown expression type: ASSEMBLE" exp)]))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp)
                         operations)]
        [aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Invalid operation argument: ASSEMBLE")))
              (operation-exp-operands exp))])
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ([val (assoc symbol operations)])
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

;; test
(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                 ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop))  ; perform recursive call
     afterfib-n-1     ; upon return, val contains Fib(n - 1)
     (restore n)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)               ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2     ; upon return, val contains Fib(n - 2)
     (restore n)    ; n now contains Fib(n - 1) and (reg val) now contains Fib(n - 2)
     (restore continue)
     (assign val                ; Fib(n - 1) + Fib(n - 2)
             (op +) (reg val) (reg n))
     (goto (reg continue))      ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))       ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done)))

(set-register-contents! fib-machine 'n 5)
;; 'done
(start fib-machine)
;; 'done
(get-register-contents fib-machine 'val)
;; 5

(displayln "Sorted instructions:")
(fib-machine 'sorted-insts)
;; (list
;;  (mcons '(assign val (reg n)) #<procedure>)
;;  (mcons '(assign val (op +) (reg val) (reg n)) #<procedure>)
;;  (mcons '(assign continue (label afterfib-n-2)) #<procedure>)
;;  (mcons '(assign n (op -) (reg n) (const 2)) #<procedure>)
;;  (mcons '(assign n (op -) (reg n) (const 1)) #<procedure>)
;;  (mcons '(assign continue (label afterfib-n-1)) #<procedure>)
;;  (mcons '(assign continue (label fib-done)) #<procedure>)
;;  (mcons '(branch (label immediate-answer)) #<procedure>)
;;  (mcons '(goto (reg continue)) #<procedure>)
;;  (mcons '(goto (reg continue)) #<procedure>)
;;  (mcons '(goto (label fib-loop)) #<procedure>)
;;  (mcons '(goto (label fib-loop)) #<procedure>)
;;  (mcons '(restore continue) #<procedure>)
;;  (mcons '(restore n) #<procedure>)
;;  (mcons '(restore n) #<procedure>)
;;  (mcons '(save val) #<procedure>)
;;  (mcons '(save n) #<procedure>)
;;  (mcons '(save continue) #<procedure>)
;;  (mcons '(test (op <) (reg n) (const 2)) #<procedure>))

(displayln "Entry point registers:")
(fib-machine 'entry-point-regs)
;; '((continue #<procedure:dispatch>))

(displayln "Saved and restored registers:")
(fib-machine 'saved-restored-regs)
;; '((val #<procedure:dispatch>) (n #<procedure:dispatch>) (continue #<procedure:dispatch>))

(displayln "Register sources:")
(fib-machine 'reg-sources)
;; '((val (((reg n)) ((op +) (reg val) (reg n)))) (continue (((label afterfib-n-2)) (((label afterfib-n-1)) ((label fib-done))))) (n (((op -) (reg n) (const 2)) ((op -) (reg n) (const 1)))))
