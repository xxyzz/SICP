#lang racket/base
(require racket/stream)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define ones (stream-cons 1 ones))
(define integers
  (stream-cons 1 (add-streams ones integers)))

(define (integrate-series s)
  (stream-map / s integers)) 

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2)
                                          (stream-first s1))
                            (mul-series (stream-rest s1)
                                        s2))))

(define (invert-unit-series s)
  (define X (stream-cons 1 (mul-series (scale-stream (stream-rest s) -1) 
                                       X)))
  X)

(define cosine-series (stream-cons 1 (integrate-series (stream-map - sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

(define (div-series s1 s2)
  (cond [(stream-empty? s2) (error "denominator stream can't be empty")]
        [(zero? (stream-first s2)) (error "denominator can't be 0")]
        [else (mul-series s1 (invert-unit-series s2))]))

(define tangent-series (div-series sine-series cosine-series))

; test
; tan θ = sin θ / cos θ
; sec θ = 1 / cos θ
; 1 + tan^2 θ = sec^2 θ
; https://en.wikipedia.org/wiki/List_of_trigonometric_identities#Pythagorean_identities
; https://en.wikipedia.org/wiki/Trigonometric_functions#Power_series_expansion
(stream->list (stream-take tangent-series 8))
; '(0 1 0 1/3 0 2/15 0 17/315)
(define secant-series (invert-unit-series cosine-series))
(stream->list (stream-take (add-streams (mul-series secant-series secant-series)
                                        (scale-stream (mul-series tangent-series tangent-series) -1)) 5))
; '(1 0 0 0 0)
