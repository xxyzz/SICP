#lang racket/base

;; use `delay` can delay eval some dumb queries which
;; returns infinite results

(dumb 1)
(rule (dumb ?x) (dumb ?x))
(rule (stupid ?x)
      (or (dumb ?x)
          (stupid ?x)))
