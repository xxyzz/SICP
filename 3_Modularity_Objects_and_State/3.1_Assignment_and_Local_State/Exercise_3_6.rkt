#lang racket/base

(define (rand input)
    (cond [(eq? input 'generate) (random)]
          [(eq? input 'reset)
            (lambda (k) (random-seed k))]
          [else (error "Unknown request: RAND" input)]))

((rand 'reset) 0)
(rand 'generate)
; 0.8571568490678037
(rand 'generate)
; 0.6594215608573717
((rand 'reset) 0)
(rand 'generate)
; 0.8571568490678037
