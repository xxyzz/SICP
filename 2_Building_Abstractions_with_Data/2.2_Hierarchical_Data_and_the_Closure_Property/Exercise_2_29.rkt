#lang racket/base

(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

(define (branch? node)
    (number? (car node)))

(define (total-weight mobile)
    (define (iter node result)
        (cond [(null? node) result]
              [(branch? node)
                (let ([structure (branch-structure node)])
                    (if (number? structure)
                        (+ structure result)
                        (iter structure result)))]
              [else (iter (left-branch node) (+ (iter (right-branch node) result)))]))
    (iter mobile 0))

(define branch-a (make-branch 1 1))
(define branch-b (make-branch 1 2))
(define branch-c (make-branch 1 3))
(define mobile-a (make-mobile branch-a branch-b))

(branch-length branch-a)
; 1
(branch-structure branch-b)
; 2
(left-branch mobile-a)
; '(1 1)
(right-branch mobile-a)
; '(1 2)

(define mobile-b (make-mobile branch-a mobile-a))
(total-weight mobile-a)
; 3
(total-weight mobile-b)
; 4
