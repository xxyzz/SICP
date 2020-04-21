#lang racket/base
(require racket/stream)

(define (square x ) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ; Sn-1
        (s1 (stream-ref s 1))   ; Sn
        (s2 (stream-ref s 2)))  ; Sn+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

(define (logarithm-two-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (logarithm-two-summands (+ n 1)))))

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

(define (partial-sums s)
  (define result (stream-cons (stream-first s) (add-streams (stream-rest s) result)))
  result)

(define logarithm-two-stream (partial-sums (logarithm-two-summands 1)))

(stream->list (stream-take (accelerated-sequence euler-transform logarithm-two-stream) 15))
;'(1.0
;  0.7
;  0.6932773109243697
;  0.6931488693329254
;  0.6931471960735491
;  0.6931471806635636
;  0.6931471805604039
;  0.6931471805599445
;  0.6931471805599427
;  0.6931471805599454
;  +nan.0
;  +nan.0
;  +nan.0
;  +nan.0
;  +nan.0)

(stream->list (stream-take logarithm-two-stream 15))
;'(1.0
;  0.5
;  0.8333333333333333
;  0.5833333333333333
;  0.7833333333333332
;  0.6166666666666666
;  0.7595238095238095
;  0.6345238095238095
;  0.7456349206349207
;  0.6456349206349207
;  0.7365440115440116
;  0.6532106782106782
;  0.7301337551337552
;  0.6587051837051838
;  0.7253718503718505)

(stream->list (stream-take (euler-transform logarithm-two-stream) 15))
;'(0.7
;  0.6904761904761905
;  0.6944444444444444
;  0.6924242424242424
;  0.6935897435897436
;  0.6928571428571428
;  0.6933473389355742
;  0.6930033416875522
;  0.6932539682539683
;  0.6930657506744464
;  0.6932106782106783
;  0.6930967180967181
;  0.6931879423258734
;  0.6931137858557215
;  0.6931748806748808)
