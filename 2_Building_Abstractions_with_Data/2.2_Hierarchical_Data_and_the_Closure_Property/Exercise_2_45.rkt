#lang racket/base
(require sicp-pict)

(define right-split (split beside below))
(define up-split (split below beside))

(define (split op-a op-b)
    (lambda (p n)
        (if (zero? n)
            p
            (let ([smaller ((split op-a op-b) p (sub1 n))])
                (op-a p (op-b smaller smaller)))))

(paint (right-split einstein 2))
(paint (up-split einstein 2))
