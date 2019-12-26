#lang racket/base

(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

; using cons
(define (make-mobile left right)
    (cons left right))

(define (make-branch length structure)
    (cons length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

; cons
(define (right-branch mobile)
    (cdr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

; cons
(define (branch-structure branch)
    (cdr branch))

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

(define (balanced? mobile)
    (= (weight (left-branch mobile)) (weight (right-branch mobile))))

(define (weight branch)
    (if (number? (branch-structure branch))
        (* (branch-length branch) (branch-structure branch))
        (* (branch-length branch) (total-weight (branch-structure branch)))))

(define branch-a (make-branch 1 1))
(define branch-b (make-branch 1 2))
(define mobile-a (make-mobile branch-a branch-b))
(define branch-c (make-branch 1 mobile-a))
(define mobile-b (make-mobile branch-a branch-c))
(define mobile-c (make-mobile branch-a branch-a))

(branch-length branch-a)
; 1
(branch-structure branch-b)
; 2
(left-branch mobile-a)
; '(1 1)
(right-branch mobile-a)
; '(1 2)
(total-weight mobile-a)
; 3
(total-weight mobile-b)
; 4
(balanced? mobile-a)
; #f
(balanced? mobile-b)
; #f
(balanced? mobile-c)
; #t
