#lang racket/base

ev-cond
(assign exp (op cond-clauses) (reg exp))

ev-cond-loop
(test (op null?) (reg exp))
(branch (label no-clause))
(assign unev (op cdr) (reg exp)) ;; rest clauses
(assign exp (op car) (reg exp)) ;; first clause
(test (op cond-else-clause?) (reg exp))
(branch (label cond-else-clause))
(save continue) ;; not else
(assign continue (label test-cond-predicate))
(save env)
(save unev)
(save exp)
(assign exp (op cond-predicate) (reg exp))
(goto (label eval-dispatch))

no-clause
(assign val (const #f))
(goto (reg continue))

cond-else-clause
(test (op null?) (reg unev))
(branch (label run-action))
(assign val (const "ELSE clause isn't last"))
(goto (label signal-error))

test-cond-predicate
(restore exp)
(restore unev)
(restore env)
(restore continue)
(test (op true?) (reg val))
(branch (label run-action))
(assign exp (reg unev)) ;; rest clauses
(goto (label ev-cond-loop))

run-action
(assign unev (op cond-actions) (reg exp))
(save continue)
(goto (label ev-sequence))

;; add these to eceval-operations
(list 'cond-clauses cond-clauses)
(list 'cond-else-clause? cond-else-clause?)
(list 'cond-predicate cond-predicate)
(list 'cond-actions cond-actions)
(list 'null? null?)
(list 'car car)
(list 'cdr cdr)

;; add to eval-dispatch
(test (op cond?) (reg exp))
(branch (label ev-cond))
