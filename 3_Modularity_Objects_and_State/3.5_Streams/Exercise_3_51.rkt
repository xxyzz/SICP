#lang racket/base
(require racket/stream)

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (show x)
  (displayln x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
; 5 printed
; 5 returned
(stream-ref x 7)
; 7 printed
; 7 returned

; results of book's implementation
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
; 0 is printed, the rest values are delayed
; stream x returned
(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5 printed
; 5 returned
(stream-ref x 7)
; 6
; 7 printed
; 7 returned
; 1 - 5 are cached
