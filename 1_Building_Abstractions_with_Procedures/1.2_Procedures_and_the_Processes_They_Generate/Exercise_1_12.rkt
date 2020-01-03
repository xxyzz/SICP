#lang racket

(define (pascal row col)
  (if (or (= col 0) (= col row)) 1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))
