#lang racket/base
(require racket/stream)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (sign-change-detector current before)
  (cond [(and (< current 0) (> before 0)) 1]
        [(and (> current 0) (< before 0)) -1]
        [else 0]))

(define ones (stream-cons 1 ones))
(define minus-ones (stream-cons -1 minus-ones))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (smooth input-stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
              input-stream
              (stream-cons 0 input-stream)))

(define sense-data (smooth (interleave ones minus-ones)))

(define zero-crossings-ator
  (stream-map sign-change-detector
              sense-data
              (stream-cons 0 sense-data)))

(stream->list (stream-take zero-crossings-ator 6))
; '(0 0 0 0 0 0)
