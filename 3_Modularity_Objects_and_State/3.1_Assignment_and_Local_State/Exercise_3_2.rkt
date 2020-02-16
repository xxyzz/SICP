#lang racket/base

(define (make-monitored f)
  (let ([calls_count 0])
    (define (mf input)
      (cond [(eq? input 'how-many-calls?) calls_count]
            [(eq? input 'reset-count) (set! calls_count 0)]
            [else
             (set! calls_count (add1 calls_count))
             (f input)]))
    mf))

(define s (make-monitored sqrt))
(s 100)
; 10
(s 100)
; 10
(s 'how-many-calls?)
; 2
(s 'reset-count)
(s 'how-many-calls?)
; 0