#lang racket/base

;; a:
(define machine-a
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list '+ +)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
     (assign continue (label done))
     count-leaves
     (test (op null?) (reg tree))
     (branch (label empty-tree))
     (test (op pair?) (reg tree))
     (branch (label not-leaf))
     (assign count (const 1)) ;; leaf
     (goto (reg continue))
     not-leaf
     (save tree)
     (save continue)
     (assign tree (op car) (reg tree))
     (assign continue (label left-leaf))
     (goto (label count-leaves))
     left-leaf
     (save count)
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label right-leaf))
     (goto (label count-leaves))
     right-leaf
     (assign temp (reg count)) ;; right tree leaves
     (restore count) ;; left tree leaves
     (assign count (op +) (reg temp) (reg count))
     (restore continue)
     (goto (reg continue))
     empty-tree
     (assign count (const 0))
     (goto (reg continue))
     done)))
;; test a
(define tree (cons (cons 1 (cons 2 3))
                   (cons 4 5)))
(set-register-contents! machine-a 'tree tree)
(start machine-a)
(get-register-contents machine-a 'count)

;; b:
(define machine-b
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list '+ +)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
     (assign n (const 0))
     (assign continue (label done))
     counter-iter
     (test (op null?) (reg tree))
     (branch (label empty-tree))
     (test (op pair?) (reg tree))
     (branch (label not-leaf))
     (assign n (op +) (reg n) (const 1)) ;; leaf
     (goto (reg continue))
     not-leaf
     (save tree)
     (save continue)
     (assign tree (op car) (reg tree))
     (assign continue (label left-tree))
     (goto (label counter-iter))
     left-tree
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (restore continue)
     (goto (label counter-iter))
     empty-tree
     (goto (reg continue))
     done)))
;; test b
(set-register-contents! machine-b 'tree tree)
(start machine-b)
(get-register-contents machine-b 'n)
