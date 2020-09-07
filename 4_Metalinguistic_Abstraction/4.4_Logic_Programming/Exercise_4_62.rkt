#lang racket/base

(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))

;; http://community.schemewiki.org/?sicp-ex-4.62
