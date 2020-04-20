#lang racket/base
(require racket/stream)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define fibs
  (stream-cons
    0
    (stream-cons 1 (add-streams (stream-rest fibs) fibs))))

; without cache
; f(2) = f(1) + f(0)   1
; f(3) = f(2) + f(1) = f(1) + f(0) + f(1)    3
; f(4) = f(3) + f(2) = f(1) + f(0) + f(1) + f(1) + f(0)    4
; f(5) = f(4) + f(3)    7
; f(6)    11

; with cache
; f(2) = f(1) + f(0)   1
; f(3) = f(2) + f(1) = f(1) + f(0) + f(1)    2
; f(4) = f(3) + f(2) = f(2) + f(1) + f(2) 
;      = f(1) + f(0) + f(1) + f(2)           3
; f(5) = f(4) + f(3) = f(3) + f(2) + f(3)
;      = f(2) + f(1) + f(2) + f(3)
;      = f(1) + f(0) + f(1) + f(2) + f(3)    4
