#lang racket/base
(require racket/stream)

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

; Louis Reasoner's code
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-first s) x))
               t)
   (pairs (stream-rest s) (stream-rest t))))

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

(stream->list (stream-take (pairs integers integers) 20))
; his code will evaluate both parmeters of interleave immediately, that's infinite recursion.