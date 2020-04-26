#lang racket/base
(require racket/stream)
(require racket/list)

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map (lambda (x) (cons (stream-first s) x))
                (pairs (stream-rest t) (stream-rest u)))
    (triples (stream-rest s) (stream-rest t) (stream-rest u)))))

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

(define pythagorean-triples
  (stream-filter (lambda (x) (= (+ (expt (first x) 2) (expt (second x) 2))
                                (expt (third x) 2)))
                 (triples integers integers integers)))

(stream->list (stream-take pythagorean-triples 3))
; '((3 4 5) (6 8 10) (5 12 13))
