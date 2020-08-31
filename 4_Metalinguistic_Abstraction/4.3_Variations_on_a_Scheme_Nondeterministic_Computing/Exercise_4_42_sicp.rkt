#lang sicp

(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond [(null? items) true]
        [(null? (cdr items)) true]
        [(member (car items) (cdr items)) false]
        [else (distinct? (cdr items))]))

(define (bloody-exam-rank)
  (let* ([betty (amb 1 2 3 4 5)]
         [ethel (amb 1 2 3 4 5)]
         [joan (amb 1 2 3 4 5)]
         [kitty (amb 1 2 3 4 5)]
         [marry (amb 1 2 3 4 5)])
    (require (not (eq? (= kitty 2) (= betty 3))))
    (require (not (eq? (= ethel 1) (= joan 2))))
    (require (not (eq? (= joan 3) (= ethel 5))))
    (require (not (eq? (= kitty 2) (= marry 4))))
    (require (not (eq? (= marry 4) (= betty 1))))
    (require (distinct? (list betty ethel joan kitty marry)))
    (list (list 'betty betty) (list 'ethel ethel)
          (list 'joan joan) (list 'kitty kitty)
          (list 'marry marry))))

(bloody-exam-rank)
;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (marry 4))
