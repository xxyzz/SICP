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
         [marry (amb 1 2 3 4 5)]
         [betty-s1 (= kitty 2)]
         [betty-s2 (= betty 3)]
         [ethel-s1 (= ethel 1)]
         [ethel-s2 (= joan 2)]
         [joan-s1 (= joan 3)]
         [joan-s2 (= ethel 5)]
         [kitty-s1 (= kitty 2)]
         [kitty-s2 (= marry 4)]
         [marry-s1 (= marry 4)]
         [marry-s2 (= betty 1)])
    (require (not (eq? betty-s1 betty-s2)))
    (require (not (eq? ethel-s1 ethel-s2)))
    (require (not (eq? joan-s1 joan-s2)))
    (require (not (eq? kitty-s1 kitty-s2)))
    (require (not (eq? marry-s1 marry-s2)))
    (require (distinct? (list betty ethel joan kitty marry)))
    (list (list 'betty betty) (list 'ethel ethel)
          (list 'joan joan) (list 'kitty kitty)
          (list 'marry marry))))

(bloody-exam-rank)
;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (marry 4))
