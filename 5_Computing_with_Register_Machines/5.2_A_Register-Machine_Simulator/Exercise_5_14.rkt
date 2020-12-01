#lang racket/base

(define (make-stack)
  (let ([s '()]
        [number-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ([top (car s)])
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (displayln (list 'total-pushes  '= number-pushes
                       'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'initialize) (initialize)]
            [(eq? message 'print-statistics)
             (print-statistics)]
            [else (error "Unknown request: STACK" message)]))
    dispatch))

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics ;; ***
                       (lambda () (stack 'print-statistics))))]
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
              [else (error "Unknown request: MACHINE"
                           message)]))
      dispatch)))

;; test
(define factorial-machine
  (make-machine
   '(n val continue)
   (list (list 'read read)
         (list '= =)
         (list '- -)
         (list '* *))
   '(controller
     (perform (op initialize-stack))
     (assign continue (label fact-done)) ;set up final return address
     (assign n (op read))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
     (goto (reg continue))                 ;return to caller
     base-case
     (assign val (const 1))                ;base case: 1! = 1
     (goto (reg continue))                 ;return to caller
     fact-done
     (perform (op print-stack-statistics))
     (goto (label controller)))))

(start factorial-machine)
;; input 2:
;; (total-pushes = 2 maximum-depth = 2)

;; input 3:
;; (total-pushes = 4 maximum-depth = 4)

;; input 4:
;; (total-pushes = 6 maximum-depth = 6)

;; input 5:
;; (total-pushes = 8 maximum-depth = 8)

;; 2(n - 1)
