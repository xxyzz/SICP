#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (tree->list-1 tree)
    (display "tree->list-1\n")
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (display "copy-to-list\n")
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                (cons (entry tree)
                      (copy-to-list (right-branch tree) result-list)))))
    (copy-to-list tree '()))

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;      7
;     / \
;    3   9
;   / \   \
;  1   5   11

(tree->list-1 tree1)
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; tree->list-1
; '(1 3 5 7 9 11)

(tree->list-2 tree1)
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; copy-to-list
; '(1 3 5 7 9 11)

; they have the same result and same (2n + 1) steps
