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

(define (cubic x) (expt x 3))

(define (weight-procedure x) (+ (cubic (first x)) (cubic (second x))))

(define S (weighted-pairs
           integers
           integers
           weight-procedure))

(define ramanujan-numbers
  (stream-filter (lambda (x) (not (zero? x)))
                 (stream-map
                  (lambda (x y)
                    (let ([x-weight (weight-procedure x)]
                          [y-weight (weight-procedure y)])
                      (if (= x-weight y-weight)
                          (begin
                            (displayln (list x-weight x y))
                            x-weight)
                          0)))
                  S (stream-rest S))))

(stream->list (stream-take ramanujan-numbers 6))
; (1729 (1 12) (9 10))
; (4104 (2 16) (9 15))
; (13832 (2 24) (18 20))
; (20683 (10 27) (19 24))
; (32832 (4 32) (18 30))
; (39312 (2 34) (15 33))
; '(1729 4104 13832 20683 32832 39312)
