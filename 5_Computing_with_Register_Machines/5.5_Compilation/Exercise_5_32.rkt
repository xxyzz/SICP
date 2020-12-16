#lang racket/base

;; a:
ev-application
(save continue)
(assign unev (op operands) (reg exp))
(assign exp (op operator) (reg exp))
(test (op variable?) (reg exp)) ;; ***
(branch (label ev-appl-symbol-operator))
(save env)
(save unev)
(assign continue (label ev-appl-did-operator))
(goto (label eval-dispatch))

ev-appl-symbol-operator
(assign continue (label ev-appl-did-symbol-operator))
(goto (label ev-variable))

ev-appl-did-operator
(restore unev) ;; the operands
(restore env)

ev-appl-did-symbol-operator
(assign argl (op empty-arglist))
(assign proc (reg val)) ;; the operator
(test (op no-operands?) (reg unev))
(branch (label apply-dispatch))
(save proc)

;; b:
;; it's a pain in the ass to write register machine code than
;; high level code and compiler will know all the code in advance
;; so it defiantly does more optimizations than evaluator
;; besides evaluator runs these optimization code at runtime for each
;; procedure but compiler run them when compiling just once
