#lang racket/base

(define (tree-map f tree)
    (cond [(null? tree) null]
        [(not (pair? tree)) (f tree)]
        [else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree)))]))

(define (tree-map f tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map f sub-tree)
                (f sub-tree)))
         tree))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
; '(1 (4 (9 16) 25) (36 49))
