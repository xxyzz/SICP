#lang racket/base

(define (get-key tree) (car tree))
(define (value tree) (cadr tree))
(define (left-branch tree) (caddr tree))
(define (right-branch tree) (cadddr tree))
(define (make-tree key value left right)
    (list key value left right))

(define (lookup key tree)
    (cond [(null? tree) #f]
          [(= key (get-key tree)) (value tree)]
          [(< key (get-key tree))
              (lookup key (left-branch tree))]
          [(> key (get-key tree))
              (lookup key (right-branch tree))]))

(lookup 1 '(2 2 (1 1 () ()) (3 3 () ())))
; 1
