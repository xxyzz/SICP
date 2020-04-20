#lang racket/base
(require racket/stream)

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream->list (stream-take (expand 1 7 10) 7))
; '(1 4 2 8 5 7 1)
; (expand 1 7 10): 1 4 2 8 5 7 1 4 2 8 5 7 ...
; 1/7 = 0.142857142857...

(stream->list (stream-take (expand 3 8 10) 5))
; '(3 7 5 0 0)
; (expand 3 8 10): 3 7 5 0 0 0 0 ...
; 3/8 = 0.375000...
