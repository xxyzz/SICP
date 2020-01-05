#lang racket/base
(require sicp-pict)
; https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html
; (below down-pic up-pic)

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ([up (up-split painter (- n 1))]
              [right (right-split painter (- n 1))])
            (let ([top-left (beside up up)]
                  [bottom-right (below right right)]
                  [corner (corner-split painter (- n 1))])
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ([smaller (right-split painter (- n 1))])
            (beside painter (below smaller smaller)))))

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ([smaller (up-split painter (sub1 n))])
            (below painter (beside smaller smaller)))))

(paint (corner-split einstein 4))
