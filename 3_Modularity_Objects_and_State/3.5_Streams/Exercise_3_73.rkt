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

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define ones (stream-cons 1 ones))

(define integers
  (stream-cons 1 (add-streams ones integers)))

(stream->list (stream-take (integral integers 0 0.5) 5))
; '(0 0.5 1.5 3.0 5.0)

(define (RC R C dt)
  (define (RC-circuit currents-stream initial-capacitor-voltage)
    (add-streams (scale-stream currents-stream R)
                 (integral (scale-stream currents-stream (/ 1 C)) initial-capacitor-voltage dt)))
  RC-circuit)

(define RC1 (RC 5 1 0.5))

(stream->list (stream-take (RC1 integers 0) 5))
; '(5 10.5 16.5 23.0 30.0)
