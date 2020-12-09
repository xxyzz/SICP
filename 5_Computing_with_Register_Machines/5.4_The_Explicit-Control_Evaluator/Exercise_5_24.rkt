#lang racket/base

ev-cond
(assign exp (op cond-clauses) (reg exp))

ev-cond-loop
(test (op null?) (reg exp))
(branch (label no-clause))
(save exp)
(assign exp (op car) (reg exp)) ;; first clause
(test (op cond-else-clause?) (reg exp))
(branch (label cond-else-clause))
(save continue) ;; not else
(assign continue (label test-cond-predicate))
(save exp)
(save env)
(assign exp (op cond-predicate) (reg exp))
(goto (label eval-dispatch))

no-clause
(assign val (const #f))
(goto (reg continue))

cond-else-caluse
(save exp)
(assign exp (op cdr) (reg exp))
(test (op null?) (reg exp))
(branch (label run-cond-action))
(perform (op error) (const "ELSE clause isn't last:") (reg exp))

test-cond-predicate
(test (op true?) (reg val))
(branch (label run-cond-action))
(restore env)
(restore exp)
(assign exp (op cdr) (reg exp)) ;; rest clauses
(restore continue)
(goto (label ev-cond-loop))

run-cond-action
(restore exp)
(assign exp (op cond-actions) (reg exp))
(restore continue)
(goto (label eval-dispatch))
