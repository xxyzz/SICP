#lang racket/base
(require racket/stream)

(define (merge s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let ([s1car (stream-first s1)]
               [s2car (stream-first s2)])
           (cond [(< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2))]
                 [(> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2)))]
                 [else (stream-cons
                        s1car
                        (merge (stream-rest s1)
                               (stream-rest s2)))]))]))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define S (stream-cons 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))
(stream->list (stream-take S 10))
; '(1 2 3 4 5 6 8 9 10 12)
