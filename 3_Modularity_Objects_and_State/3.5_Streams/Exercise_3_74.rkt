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

(define sense-data (interleave ones minus-ones))

; Alyssa’s code:
(define (make-zero-crossings input-stream last-value)
  (stream-cons
   (sign-change-detector
    (stream-first input-stream)
    last-value)
   (make-zero-crossings
    (stream-rest input-stream)
    (stream-first input-stream))))
(define zero-crossings-alyssa
  (make-zero-crossings sense-data 0))

; Ator's code:
(define zero-crossings-ator
  (stream-map sign-change-detector
              sense-data
              (stream-cons 0 sense-data)))

(stream->list (stream-take zero-crossings-alyssa 6))
; '(0 1 -1 1 -1 1)
(stream->list (stream-take zero-crossings-ator 6))
; '(0 1 -1 1 -1 1)
