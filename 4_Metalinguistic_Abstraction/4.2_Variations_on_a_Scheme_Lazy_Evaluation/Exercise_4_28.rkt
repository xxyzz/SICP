#lang racket/base

(define (add1 x) (+ x 1))

(define (add1-to-list proc l)
  (if (null? l)
      l
      (cons (proc (car l)) (cdr l))))

(add1-to-list add1 (list 1 2 3))
;; if use `eval` instead of `actual-val` in the `application?` branch of `eval`, `myapply` will get a thunk of `proc` as a prcedure. Thus we get unknown procedure type error.
