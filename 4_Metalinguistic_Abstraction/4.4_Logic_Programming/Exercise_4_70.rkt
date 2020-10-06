#lang racket/base
(require racket/stream)

(define ones (stream-cons 1 ones))
(set! ones (stream-cons 2 ones))
(stream-first ones)
;; 2
(stream-first (stream-rest ones))
;; 2

(let ([tmp ones])
  (set! ones (stream-cons 3 tmp)))

(stream-first ones)
;; 3
(stream-first (stream-rest ones))
;; 2
