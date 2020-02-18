#lang racket/base

(define f
  (let ([old-value 0]
        [temp-value 0])
    (lambda (x)
      (set! temp-value old-value)
      (set! old-value x)
      temp-value)))

(+ (f 0) (f 1))
; 0
