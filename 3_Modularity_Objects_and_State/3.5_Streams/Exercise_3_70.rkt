#lang racket/base
(require racket/stream)
(require racket/list)

(define (merge-weighted s1 s2 weight)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let* ([s1car (stream-first s1)]
                [s2car (stream-first s2)]
                [weight-result (- (weight s1car) (weight s2car))])
           (cond [(<= weight-result 0)
                  (stream-cons
                   s1car
                   (merge-weighted (stream-rest s1) s2 weight))] ; !!
                 [(> weight-result 0)
                  (stream-cons
                   s2car
                   (merge-weighted s1 (stream-rest s2) weight))]))]))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (weighted-pairs (stream-rest s) (stream-rest t) weight)
    weight)))

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

; a
(stream->list (stream-take (weighted-pairs
                            integers
                            integers
                            (lambda (x) (+ (first x) (second x))))
                           10))
; '((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6))

; b
(define (divisible-by? x y) (zero? (remainder x y)))
(define S (stream-filter (lambda (x) (not (or (divisible-by? x 2)
                                              (divisible-by? x 3)
                                              (divisible-by? x 5))))
                         integers))
(stream->list (stream-take (weighted-pairs
                            S
                            S
                            (lambda (x) (+ (* (first x) 2)
                                           (* (second x) 3)
                                           (* 5 (first x) (second x)))))
                           10))
; '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7))
