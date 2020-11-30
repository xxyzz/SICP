#lang racket/base

(define (make-machine ops controller-text) ;; remove register-names
  (let ([machine (make-new-machine)])
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; inside make-new-machine:
(define (lookup-register name)
  (let ([val (assoc name register-table)])
    (if val
        (cadr val)
        (begin ;; create new register if not exist
          (allocate-register name)
          (lookup-register name)))))
