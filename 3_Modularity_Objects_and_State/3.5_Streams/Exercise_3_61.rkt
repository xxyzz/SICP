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
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2)
                                          (stream-first s1))
                            (mul-series (stream-rest s1)
                                        s2))))

(define (invert-unit-series s)
  (define X (stream-cons 1 (mul-series (scale-stream (stream-rest s) -1)
                                       X)))
  X)

; test
(define ones (stream-cons 1 ones))
(define integers
  (stream-cons 1 (add-streams ones integers)))

(stream->list (stream-take integers 5))
; '(1 2 3 4 5)
(stream->list (stream-take (invert-unit-series integers) 5))
; '(1 -2 1 0 0)
(stream->list (stream-take (mul-series integers (invert-unit-series integers)) 5))
; '(1 0 0 0 0)
