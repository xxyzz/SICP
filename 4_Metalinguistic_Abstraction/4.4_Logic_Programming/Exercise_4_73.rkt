#lang racket/base

(assert! (completes Batman Joker))
(assert! (completes Batman Bane))
(assert! (rule (completes ?x ?y) (completes ?y ?x)))
(and (completes ?who Joker)
     (completes ?who Bane))

;; the program won't get out of `flatten-stream`
;; when the stream is infinite
;; therefore nothing prints out
