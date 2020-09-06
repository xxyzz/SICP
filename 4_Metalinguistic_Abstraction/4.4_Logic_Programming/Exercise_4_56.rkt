#lang racket/base

;; a:
(and (supervisor ?person (Bitdiddle Ben))
     (address ?persion ?where))

;; b:
(and (salary (Bitdiddle Ben) ?wizard-salary)
     (and (salary ?person ?amount)
          (lisp-value < ?amount ?wizard-salary)))

;; c:
(and (supervisor ?x ?boss)
     (not (job ?boss (computer . ?position)))
     (job ?boss ?boss-position))
