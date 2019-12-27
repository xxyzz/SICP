#lang racket/base

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; see exercise 2.28, SICP page 159
(define (count-leaves t)
    (accumulate + 0 (map (lambda (node) 
                            (cond [(null? node) 0]
                                  [(pair? node) (count-leaves node)]
                                  [else 1]))
                         t)))

(count-leaves (list 1 (list 2 (list 3 4)) 5))
; 5
