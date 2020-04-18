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
(define s (stream-cons 1 (add-streams s s)))
(stream->list (stream-take s 11))
; '(1 2 4 8 16 32 64 128 256 512 1024)