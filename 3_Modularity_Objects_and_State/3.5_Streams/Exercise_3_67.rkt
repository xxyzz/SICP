#lang racket/base
(require racket/stream)

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
                (stream-rest t)) ; rest of first row
    (pairs (stream-rest s) t)))) ; next row

(define (second-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t)) ; rest of first row
    (interleave
     (stream-map (lambda (x) (list x (stream-first t)))
                 (stream-rest s)) ; rest of left column
     (second-pairs (stream-rest s) (stream-rest t)))))) ; inner matrix

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
; '((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (3 2) (1 7) (2 4) (1 8) (4 1) (1 9) (2 5) (1 10) (3 3) (1 11))
(stream->list (stream-take (second-pairs integers integers) 20))
; '((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (4 1) (1 7) (3 2) (1 8) (5 1) (1 9) (2 4) (1 10) (6 1) (1 11))
