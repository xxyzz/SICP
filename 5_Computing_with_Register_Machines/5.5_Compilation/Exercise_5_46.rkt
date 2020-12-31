#lang racket/base

(compile-and-go
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))

;;EC-Eval input:
(fib 0)

(total-pushes = 7 maximum-depth = 3)

;;EC-Eval value:
0

;;EC-Eval input:
(fib 1)

(total-pushes = 7 maximum-depth = 3)

;;EC-Eval value:
1

;;EC-Eval input:
(fib 2)

(total-pushes = 14 maximum-depth = 4)

;;EC-Eval value:
1

;;EC-Eval input:
(fib 3)

(total-pushes = 21 maximum-depth = 6)

;;EC-Eval value:
2

;;EC-Eval input:
(fib 4)

(total-pushes = 35 maximum-depth = 8)

;;EC-Eval value:
3

;;EC-Eval input:
(fib 5)

(total-pushes = 56 maximum-depth = 10)

;;EC-Eval value:
5

;; special-purpose:
(define fib-machine
  (make-machine
   '(val continue n)
   (list (list 'read read)
         (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
     (perform (op initialize-stack))
     (assign continue (label fib-done))
     (assign n (op read))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                       ; upon return, val contains Fib(n - 1)
     (restore n)
     ;; (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     ;; (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                       ; upon return, val contains Fib(n - 2)
     (assign n (reg val))               ; n now contains Fib(n - 2)
     (restore val)                      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done
     (perform (op print-stack-statistics))
     (perform (op displayln) (reg val))
     (goto (label controller)))))
(start fib-machine)
;; input 0:
(total-pushes = 0 maximum-depth = 0)
0

;; input 1:
(total-pushes = 0 maximum-depth = 0)
1

;; input 2:
(total-pushes = 3 maximum-depth = 2)
1

;; input 3:
(total-pushes = 6 maximum-depth = 4)
2

;; input 4:
(total-pushes = 12 maximum-depth = 6)
3

;; input 5:
(total-pushes = 21 maximum-depth = 8)
5

;; input 6:
(total-pushes = 36 maximum-depth = 10)
8

;; compile fib:
;; total-pushes: 3F(n+1)
;; maximum-depth: 2n

;; special-purpose fib:
;; total-pushes: 3F(n+1) - 3
;; maximum-depth: 2(n - 1)

;; interpreted fib:
;; total-pushes: 56F(n+1) - 40
;; maximum-depth: 5n + 3
