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

; a:
(define (integrate-series s)
  (stream-map / s integers))
(stream->list (stream-take (integrate-series integers) 5))
; '(1 1 1 1 1)

; b:
; (cos x)' = - sin x => cos x = ∫- sin x
; (sin x)' = cos x => sin x = ∫cos x
(define cosine-series (stream-cons 1 (integrate-series (stream-map - sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

(stream->list (stream-take cosine-series 5))
; '(1 0 -1/2 0 1/24 0)

(stream->list (stream-take sine-series 5))
; '(0 1 0 -1/6 0 1/120)
