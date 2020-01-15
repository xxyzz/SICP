#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (tree->list tree)
    (if (null? tree)
        '()
        (append (tree->list (left-branch tree))
                (cons (entry tree)
                      (tree->list (right-branch tree))))))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (display "partial-tree")
    (if (= n 0)
        (cons '() elts)
        (let ([left-size (quotient (- n 1) 2)])
            (let ([left-result (partial-tree elts left-size)])
                (let ([left-tree (car left-result)]
                      [non-left-elts (cdr left-result)]
                      [right-size (- n (+ left-size 1))])
                    (let ([this-entry (car non-left-elts)]
                          [right-result
                              (partial-tree
                                  (cdr non-left-elts)
                                  right-size)])
                        (let ([right-tree (car right-result)]
                              [remaining-elts (cdr right-result)])
                             (cons (make-tree this-entry
                                              left-tree
                                              right-tree)
                                   remaining-elts))))))))

(define (adjoin-set x set)              
    (cond [(null? set) (list x)]
          [(< x (car set)) (cons x set)]
          [(> x (car set)) (cons (car set) (adjoin-set x (cdr set)))]
          [else set]))

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
                (cond [(< x1 x2) (cons x1 (union-set (cdr set1) set2))]
                      [(> x1 x2) (cons x2 (union-set set1 (cdr set2)))]
                      [else (cons x1 (union-set (cdr set1) (cdr set2)))]))]))

(define (adjoin-tree-set x set)
    (list->tree (adjoin-set x (tree->list set))))

(define (union-tree-set set1 set2)
    (list->tree (union-set (tree->list set1)
                           (tree->list set2))))

(adjoin-tree-set 3 '(2 (1 () ()) (4 () ())))
; '(2 (1 () ()) (3 () (4 () ())))

(union-tree-set '(4 (2 () ()) (6 () ())) '(3 (1 () ()) (5 () ())))
; '(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))
