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

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2)
                                          (stream-first s1))
                            (mul-series (stream-rest s1)
                                        s2))))

; See the explanation at here:
; https://github.com/sarabander/p2pu-sicp/blob/master/3.5/3.60.scm

; test
(define ones (stream-cons 1 ones))
(define integers
  (stream-cons 1 (add-streams ones integers)))

(define (integrate-series s)
  (stream-map / s integers))

(define cosine-series (stream-cons 1 (integrate-series (stream-map - sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

(stream->list (stream-take (add-streams (mul-series sine-series sine-series)
                                        (mul-series cosine-series cosine-series)) 5))
; '(1 0 0 0 0)
