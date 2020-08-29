#lang sicp
;; https://docs.racket-lang.org/sicp-manual/Installation.html

(define (require p) (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(an-integer-between 1 5)
;; 1
