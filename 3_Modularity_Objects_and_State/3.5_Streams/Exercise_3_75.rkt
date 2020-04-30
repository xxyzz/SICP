#lang racket/base
(require racket/stream)

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (sign-change-detector current before)
  (cond [(and (< current 0) (> before 0)) 1]
        [(and (> current 0) (< before 0)) -1]
        [else 0]))

(define ones (stream-cons 1 ones))
(define minus-ones (stream-cons -1 minus-ones))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define sense-data (interleave ones minus-ones))

(define (make-zero-crossings input-stream last-value last-avg-value)
  (let ([avpt (/ (+ (stream-first input-stream)
                    last-value)
                 2)])
    (stream-cons
     (sign-change-detector avpt last-avg-value)
     (make-zero-crossings
      (stream-rest input-stream) (stream-first input-stream) avpt))))

(stream->list (stream-take (make-zero-crossings sense-data 0 0) 6))
; '(0 0 0 0 0 0)
