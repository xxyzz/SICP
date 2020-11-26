#lang racket/base

;; new syntax add1, add one to register
;; (add1 register-name)

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
        [(eq? (car inst) 'add1)
         (make-add1 inst machine pc)]
        (else
         (error "Unknown instruction type: ASSEMBLE"
                inst))))

(define (make-add1 inst machine pc)
  (let ([target (get-register machine (add1-reg-name inst))])
    (lambda ()
      (set-contents! target (add1 (get-contents target)))
      (advance-pc pc))))

(define (add1-reg-name add1-instruction)
  (cadr add1-instruction))
