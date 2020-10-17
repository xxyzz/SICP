#lang racket/base

;; use `delay` can delay eval some dumb queries which
;; returns infinite results

(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y) (married ?y ?x)))
(married Mickey ?who)

;; without `delay`: infinite loop, nothing come out
;; with `delay`: infinite output (married Mickey Minnie)
