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
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

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

(stream->list (stream-take (pairs integers integers) 18))
; '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) (4 4) (1 9) (2 6) (1 10))

; pair (1, n) appears every two elements
; ---- means interleave
; (1, 1) (1, 2)      (1, 3)      (1, 4)      (1. 5)     (1, 6)     (1, 7)     (1, 8)     (1, 9)     (1, 10)     (1, 11)     (1, 12)     (1, 13)
;        -----------------------------------------------------------------------------------------------------------------------------------------------------------------
;             (2, 2)       (2, 3)                 (2, 4)                (2, 5)                (2, 6)                  (2, 7)                  (2, 8)
;                          -------------------------------------------------------------------------------------------------------------------------------------------
;                                      (3, 3)                (3, 4)                                       (3, 5)
;                                                            -------------------------------------------------------------------------------------------------
;                                                                                  (4, 4)                                         (4, 5)

; pair (1, n) appears every two elements
; n = 1, 0 before (1, 1)
; n > 1, 1+((n-1)-1)*2 = 2n-3 before (1, n)
; 1 = 0 + 1
; 2 * 100 - 3 = 197

; pair (2, n) appears every four elements
; n = 2, 2 before (2, 2)
; n > 2, 4+((n-2)-1)*4 = 4n-8, before (2, n)
; 4 = 2 + 2

; pair (3, n) appears every eight elements
; n = 3, 4 + 2 = 6 elements before (3, 3)
; n > 3, 10 + ((n-3)-1)*2^3 before (3, n)
; 10 = 6 + 4

; n = 4, 8 + 4 + 2 = 14 elements before (4, 4)

; (m, n) has 0 elements before it when m = n = 1
; (m, n) has (1 - 2^(m-1))*2/(1-2) elements before it when m = n and m > 1
; (m, n) has (1 - 2^(m-1))*2/(1-2) + 2^(m-1) + ((n-m)-1)*2^m elements before it when m != n

(define (precede m n)
  (cond [(< m 1) (error "m should >= 1")]
        [(< n 1) (error "n should >= 1")]
        [(= m n) (- (expt 2 m) 2)]
        [else
          (+ (expt 2 m)
             -2
             (expt 2 (sub1 m))
             (* (- n m 1)
                (expt 2 m)))]))

(precede 1 100)
; 197
(precede 99 100)
; 950737950171172051122527404030
(precede 100 100)
; 1267650600228229401496703205374
