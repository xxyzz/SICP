#lang racket/base

(define (or-gate a1 a2 ouput)
  (let ([c (make-wire)]
        [d (make-wire)]
        [e (make-wire)])
    (inverter a1 c)
    (inverter a2 d)
    (and-gate c d e)
    (inverter e ouput)
    'ok))

; A and B = not ((not A) and (not B))
; delay time = 2 * inverter-delay + and-gate-delay
