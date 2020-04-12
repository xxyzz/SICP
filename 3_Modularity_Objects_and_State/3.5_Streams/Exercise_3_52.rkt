#lang racket/base
(require racket/stream)

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))
       
(define (display-stream s)
  (stream-for-each displayln s))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
; sum = 0
; book: sum = 1
; book without cache: sum = 1, first element is not delayed
(define y (stream-filter even? seq))
; sum = 0
; book: sum = 1 + 2 + 3 = 6
; book without cache: sum = 6
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; sum = 0
; book: sum = 10
; book without cache: sum = 6 + 2 + 3 + 4 = 15
(stream-ref y 7)
; 136
; sum = 136
; book: sum = 136
; book without cache: sum = 15 + 4 + 5 + ... + 17 = 162
(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; sum = 210

; seq: 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
; y: 6 10 28 36 66 78 120 136 190 210

; http://community.schemewiki.org/?sicp-ex-3.52

