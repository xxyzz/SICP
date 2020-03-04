#lang racket/base

(define (or-gate a1 a2 ouput)
  (define (or-action-procedure)
    (let ([new-value
           (logical-or (get-signal a1) (get-signal a2))])
      (after-delay
       or-gate-dalay
       (lambda () (set-signal! ouput new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (if (or (and (not (= a 1)) (not (= a 0)))
          (and (not (= b 1)) (not (= b 0))))
      (error "Invalid signal" a b)
      (if (or (= a 1) (= b 1))
          1
          0)))
