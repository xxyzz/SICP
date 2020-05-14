#lang racket/base

(define (expand-clauses clauses)
  (if (null? clauses)
      #f    ; no clause
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                        clauses))
            (make-if (cond-predicate first)
                     (if (eq? (cadr first) '=>)
                         (cons (caddr first) (cond-predicate first))
                         (sequence->exp (cond-actions first)))
                      (expand-clauses rest))))))
