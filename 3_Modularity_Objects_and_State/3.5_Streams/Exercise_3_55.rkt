#lang racket/base
(require racket/stream)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (stream-cons 1 ones))
(define integers
  (stream-cons 1 (add-streams ones integers)))

(define (partial-sums s)
  (define result (stream-cons (stream-first s) (add-streams (stream-rest s) result)))
  result)

(stream->list (stream-take (partial-sums integers) 10))
; '(1 3 6 10 15 21 28 36 45 55)
