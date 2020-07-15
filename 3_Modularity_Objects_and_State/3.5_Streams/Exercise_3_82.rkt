#lang racket/base
(require racket/stream)

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))

(define (predicate x y)
  (<= (+ (* x x) (* y y)) 1))

(define (random-position-stream low heigh)
  (stream-cons (random-in-range low heigh)
               (random-position-stream low heigh)))

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

(define (estimate-integral P x1 x2 y1 y2)
  (define bool-results (stream-map P
                                   (random-position-stream x1 x2)
                                   (random-position-stream y1 y2)))
  (define predicate-results (stream-map (lambda (x) (if x 1.0 0)) bool-results))
  (define results-sum (stream-cons (stream-first predicate-results)
                                   (add-streams (stream-rest predicate-results) results-sum)))
  (define probabilities (stream-map (lambda (x y) (/ x y))
                                    results-sum
                                    integers))
  (define square-area (abs (* (- x1 x2) (- y1 y2))))
  (stream-map (lambda (x) (* x square-area)) probabilities))

(stream->list (stream-take (estimate-integral predicate -1.0 1.0 -1.0 1.0) 10))
; '(4.0 4.0 2.6666666666666665 3.0 3.2 3.3333333333333335 3.4285714285714284 3.5 3.5555555555555554 3.6)
(stream-ref (estimate-integral predicate -1.0 1.0 -1.0 1.0) 10000)
; 3.142085791420858
