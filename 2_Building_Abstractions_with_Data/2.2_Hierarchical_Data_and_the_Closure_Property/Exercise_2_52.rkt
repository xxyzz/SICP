#lang racket/base
(require sicp-pict)

; add face to the outline painter
; a: add segments to outline
(define outline
    (lambda (frame)
        (let ([vect-a (make-vect 0 0)]
              [vect-b (make-vect 0 1)]
              [vect-c (make-vect 1 1)]
              [vect-d (make-vect 1 0)]
              [vect-e (make-vect 0.2 0.7)]
              [vect-f (make-vect 0.4 0.7)]
              [vect-g (make-vect 0.6 0.7)]
              [vect-h (make-vect 0.8 0.7)]
              [vect-i (make-vect 0.2 0.3)]
              [vect-j (make-vect 0.8 0.3)])
            (let ([segment-a (make-segment vect-a vect-b)]
                  [segment-b (make-segment vect-b vect-c)]
                  [segment-c (make-segment vect-c vect-d)]
                  [segment-d (make-segment vect-d vect-a)]
                  [segment-e (make-segment vect-e vect-f)]
                  [segment-f (make-segment vect-g vect-h)]
                  [segment-g (make-segment vect-i vect-j)])
                ((segments->painter
                    (list segment-a segment-b segment-c segment-d segment-e segment-f segment-g))
                    frame)))))

(paint (square-limit outline 1))

; b: change the pattern of corner-split
(define (split op-a op-b)
    (lambda (p n)
        (if (zero? n)
            p
            (let ([smaller ((split op-a op-b) p (sub1 n))])
                (op-a p (op-b smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ([up (up-split painter (- n 1))]
              [right (right-split painter (- n 1))])
            (let ([top-left up]
                  [bottom-right right]
                  [corner (corner-split painter (- n 1))])
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(paint (corner-split einstein 4))

; c: let Einstein looks inward
(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ([top (beside (tl painter) (tr painter))]
              [bottom (beside (bl painter) (br painter))])
            (below bottom top))))

(define (identity x) x)

(define (square-limit painter n)
    (let ([combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)])
        (combine4 (corner-split (flip-horiz painter) n))))

(paint (square-limit einstein 4))
