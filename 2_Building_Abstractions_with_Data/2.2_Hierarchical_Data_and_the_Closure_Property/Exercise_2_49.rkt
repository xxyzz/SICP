#lang racket/base
(require sicp-pict)
; document: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html
; source code: https://github.com/sicp-lang/sicp/blob/master/sicp-pict/main.rkt

; (painter (frame (vect 0. 0.) (vect 1. 0.) (vect 0. 1.)))
; from paint procedure, see https://github.com/sicp-lang/sicp/blob/master/sicp-pict/main.rkt#L261

(define outline
    (lambda (frame)
        (let ([vect-a (make-vect 0 0)]
              [vect-b (make-vect 0 1)]
              [vect-c (make-vect 1 1)]
              [vect-d (make-vect 1 0)])
            (let ([segment-a (make-segment vect-a vect-b)]
                  [segment-b (make-segment vect-b vect-c)]
                  [segment-c (make-segment vect-c vect-d)]
                  [segment-d (make-segment vect-d vect-a)])
                ((segments->painter (list segment-a segment-b segment-c segment-d)) frame)))))

(paint outline)

(define x-paint
    (lambda (frame)
        (let ([vect-a (make-vect 0 0)]
              [vect-b (make-vect 0 1)]
              [vect-c (make-vect 1 1)]
              [vect-d (make-vect 1 0)])
            (let ([segment-a (make-segment vect-b vect-d)]
                  [segment-b (make-segment vect-a vect-c)])
                ((segments->painter (list segment-a segment-b)) frame)))))

(paint x-paint)

(define diamond
    (lambda (frame)
        (let ([vect-a (make-vect 0 0.5)]
              [vect-b (make-vect 0.5 1)]
              [vect-c (make-vect 1 0.5)]
              [vect-d (make-vect 0.5 0)])
            (let ([segment-a (make-segment vect-a vect-b)]
                  [segment-b (make-segment vect-b vect-c)]
                  [segment-c (make-segment vect-c vect-d)]
                  [segment-d (make-segment vect-d vect-a)])
                ((segments->painter (list segment-a segment-b segment-c segment-d)) frame)))))

(paint diamond)
