#lang racket/base
(require racket/list)

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()]
        [sorted-insts null] ;; new lists
        [entry-point-regs null]
        [saved-restored-regs null]
        [reg-sources null])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
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
              [(eq? message 'stack) stack]
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

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
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
                            (symbol->string x)
                            (symbol->string y)))
                         #:key car)])
             ((machine 'set-sorted-insts!) new-sorted-insts))))
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (mcons text null))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

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

(define (make-save inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [saved-restored-regs (machine 'saved-restored-regs)])
    ;; add saved register to list
    (when (not (assoc reg-name saved-restored-regs))
      ((machine 'set-saved-restored-regs!)
       (cons (list reg-name reg)
             saved-restored-regs)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [saved-restored-regs (machine 'saved-restored-regs)])
    ;; add restored register to list
    (when (not (assoc reg-name saved-restored-regs))
      ((machine 'set-saved-restored-regs!)
       (cons (list reg-name reg)
             saved-restored-regs)))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (make-assign inst machine labels operations pc)
  (let* ([reg-name (assign-reg-name inst)]
         [target
          (get-register machine reg-name)]
         [value-exp (assign-value-exp inst)]
         [reg-sources (machine 'reg-sources)]
         [target-sources (assoc reg-name reg-sources)]
         [new-reg-sources
          (cond [(null? target-sources)
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

;; need to test later
