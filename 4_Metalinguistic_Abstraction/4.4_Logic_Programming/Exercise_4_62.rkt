#lang racket/base

(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))

;; http://community.schemewiki.org/?sicp-ex-4.62

(last-pair (3) ?x)
;;; Query results:
(last-pair (3) (3))

(last-pair (1 2 3) ?x)
;;; Query results:
(last-pair (1 2 3) (3))

(last-pair (2 ?x) (3))
;;; Query results:
(last-pair (2 3) (3))

(last-pair ?x (3))
;;; Query results:
;; program can't complete
