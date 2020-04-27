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

(define (square x) (expt x 2))

(define (weight-procedure x) (+ (square (first x)) (square (second x))))

(define S (weighted-pairs 
           integers
           integers
           weight-procedure))

(define sum-of-square
  (stream-filter (lambda (x) (not (zero? x)))
                 (stream-map
                  (lambda (x y z)
                    (let ([x-weight (weight-procedure x)]
                          [y-weight (weight-procedure y)]
                          [z-weight (weight-procedure z)])
                      (if (= x-weight y-weight z-weight)
                          (begin
                            (displayln (list x-weight x y z))
                            x-weight)
                          0)))
                  S (stream-rest S) (stream-rest (stream-rest S)))))

(stream->list (stream-take sum-of-square 6))
; (325 (1 18) (6 17) (10 15))
; (425 (5 20) (8 19) (13 16))
; (650 (5 25) (11 23) (17 19))
; (725 (7 26) (10 25) (14 23))
; (845 (2 29) (13 26) (19 22))
; (850 (3 29) (11 27) (15 25))
; '(325 425 650 725 845 850)
