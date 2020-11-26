#lang racket/base

;; a: use restore as assign
;; afterfib-n-2     ; upon return, val contains Fib(n − 2)
;;  (assign n (reg val))       ; n now contains Fib(n − 2)
;;  (restore val)              ; val now contains Fib(n − 1)
;;
;; afterfib-n-2
;;   (restore n) ;; n now contains Fib(n - 1) and (reg val) now contains Fib(n - 2)
;;   (restore continue)
;;   (assign val                ;; still Fib(n − 1) + Fib(n − 2)
;;           (op +) (reg val) (reg n))
;;   (goto (reg continue))      ; return to caller, answer is in val

;; b: check popped register name
(define (make-save inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)])
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)])
    (lambda ()
      (let* ([pop-reg (pop stack)]
             [pop-reg-name (car pop-reg)]
             [pop-reg-value (cdr pop-reg)])
        (if (eq? pop-reg-name reg-name)
            (begin
              (set-contents! reg pop-reg-value)
              (advance-pc pc))
            (error "Popped inconsistent register:" reg-name pop-reg-name))))))

;; c: each register has it's own stack
(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks null] ;; ***
        [the-instruction-sequence '()])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin ;; ***
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
              [(eq? message 'allocate-register) allocate-register]
              [(eq? message 'get-register) lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops)))]
              [(eq? message 'stacks) stacks] ;; ***
              [(eq? message 'operations) the-ops]
              [else (error "Unknown request: MACHINE"
                           message)]))
      dispatch)))

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stacks (machine 'stacks)] ;; ***
        [ops (machine 'operations)])
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stacks ops))) ;; ***
     insts)))

(define (find-reg-stack reg-name stacks)
  (let ([stack-pair (assoc reg-name stacks)])
    (cdr stack-pair)))

(define (make-save inst machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stack (find-reg-stack reg-name stacks)])
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))
(define (make-restore inst machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name inst)]
         [reg (get-register machine reg-name)]
         [stack (find-reg-stack reg-name stacks)])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
