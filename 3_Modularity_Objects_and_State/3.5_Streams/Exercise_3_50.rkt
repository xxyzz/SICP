#lang racket/base
(require racket/stream)
; https://docs.racket-lang.org/reference/streams.html

(define (stream-mapp proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-mapp
              (cons proc (map stream-rest argstreams))))))

; test
(define stream_a (stream-cons 1 (list 2)))
(define stream_b (stream-cons 3 (list 4)))
(define stream_c (stream-cons 5 (list 6)))
(stream->list (stream-mapp + stream_a stream_b stream_c))
; '(9 12)
