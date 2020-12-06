#lang racket/base

;; append:
(define append-machine
  (make-machine
   (list (list 'cons cons)
         (list 'car car)
         (list 'cdr cdr)
         (list 'null? null?))
   '(controller
     (assign continue (label done))
     append
     (test (op null?) (reg x))
     (branch (label x-is-null))
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label after-cdr-x))
     (goto (label append))
     after-cdr-x
     (restore x)
     (assign x (op car) (reg x))
     (assign y (op cons) (reg x) (reg y))
     (restore continue)
     (goto (reg continue))
     x-is-null
     (goto (reg continue))
     done)))
(set-register-contents! append-machine 'x (list 'a 'b))
(set-register-contents! append-machine 'y (list 'c 'd))
(start append-machine)
(get-register-contents append-machine 'y)

;; append!
(define append!-machine
  (make-machine
   (list (list 'mcons mcons)
         (list 'mcar mcar)
         (list 'mcdr mcdr)
         (list 'null? null?)
         (list 'set-mcdr! set-mcdr!))
   '(controller
     (assign x-copy (reg x))
     last-pair
     (assign temp (op mcdr) (reg x-copy))
     (test (op null?) (reg temp))
     (branch (label x-is-last-pair))
     (assign x-copy (op mcdr) (reg x-copy))
     (goto (label last-pair))
     x-is-last-pair
     (perform (op set-mcdr!) (reg x-copy) (reg y)))))
(set-register-contents! append!-machine 'x (mcons 'a (mcons 'b null)))
(set-register-contents! append!-machine 'y (mcons 'c (mcons 'd null)))
(start append!-machine)
(get-register-contents append!-machine 'x)
