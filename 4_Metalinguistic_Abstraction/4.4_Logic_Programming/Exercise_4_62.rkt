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
;; it will generate infinite frames
;; (last-pair ?1-v (?1-x)) -> (last-pair (?2-u . ?2-v) (?2-x))
;; if we change the order of the two rules
;; (rule (last-pair (?x) (?x))) will applied first
;; the other rule will delayed then infinite results will printed
