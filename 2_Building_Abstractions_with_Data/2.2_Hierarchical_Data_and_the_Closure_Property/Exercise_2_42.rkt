#lang racket/base
(require racket/list) ;; last, drop-right

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board null)

(define (adjoin-position new-row nth-row rest-queens)
  (if (= 1 nth-row)
      (list new-row) ;; first row
      (append rest-queens (list new-row))))

;; `and` is syntax not procedure
(define (and-accumulate sequence)
  (if (null? sequence)
      #t
      (and (car sequence)
           (and-accumulate (cdr sequence)))))

(define (safe? nth-row positions)
  (if (= nth-row 1)
      #t
      (let ([new-queen (last positions)]
            [rest-queens (drop-right positions 1)])
        (and-accumulate
         (map (lambda (another-queen)
                (and (not (= new-queen another-queen)) ;; not in same column
                     (not (= (abs (- new-queen another-queen)) ;; not in same diagonal
                             (abs (- nth-row
                                     (add1 (index-of rest-queens another-queen))))))))
              rest-queens)))))

(queens 4)
;; '((2 4 1 3) (3 1 4 2))

;; 0 1 0 0
;; 0 0 0 1
;; 1 0 0 0
;; 0 0 1 0

;; 0 0 1 0
;; 1 0 0 0
;; 0 0 0 1
;; 0 1 0 0

;; Eight queens puzzle
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle
(queens 8)
;; '((1 5 8 6 3 7 2 4)
;;   (1 6 8 3 7 4 2 5)
;;   (1 7 4 6 8 2 5 3)
;;   (1 7 5 8 2 4 6 3)
;;   (2 4 6 8 3 1 7 5)
;;   (2 5 7 1 3 8 6 4)
;;   (2 5 7 4 1 8 6 3)
;;   (2 6 1 7 4 8 3 5)
;;   (2 6 8 3 1 4 7 5)
;;   (2 7 3 6 8 5 1 4)
;;   (2 7 5 8 1 4 6 3)
;;   (2 8 6 1 3 5 7 4)
;;   (3 1 7 5 8 2 4 6)
;;   (3 5 2 8 1 7 4 6)
;;   (3 5 2 8 6 4 7 1)
;;   (3 5 7 1 4 2 8 6)
;;   (3 5 8 4 1 7 2 6)
;;   (3 6 2 5 8 1 7 4)
;;   (3 6 2 7 1 4 8 5)
;;   (3 6 2 7 5 1 8 4)
;;   (3 6 4 1 8 5 7 2)
;;   (3 6 4 2 8 5 7 1)
;;   (3 6 8 1 4 7 5 2)
;;   (3 6 8 1 5 7 2 4)
;;   (3 6 8 2 4 1 7 5)
;;   (3 7 2 8 5 1 4 6) this is the one in the book
;;   (3 7 2 8 6 4 1 5)
;;   (3 8 4 7 1 6 2 5)
;;   (4 1 5 8 2 7 3 6)
;;   (4 1 5 8 6 3 7 2)
;;   (4 2 5 8 6 1 3 7)
;;   (4 2 7 3 6 8 1 5)
;;   (4 2 7 3 6 8 5 1)
;;   (4 2 7 5 1 8 6 3)
;;   (4 2 8 5 7 1 3 6)
;;   (4 2 8 6 1 3 5 7)
;;   (4 6 1 5 2 8 3 7)
;;   (4 6 8 2 7 1 3 5)
;;   (4 6 8 3 1 7 5 2)
;;   (4 7 1 8 5 2 6 3)
;;   (4 7 3 8 2 5 1 6)
;;   (4 7 5 2 6 1 3 8)
;;   (4 7 5 3 1 6 8 2)
;;   (4 8 1 3 6 2 7 5)
;;   (4 8 1 5 7 2 6 3)
;;   (4 8 5 3 1 7 2 6)
;;   (5 1 4 6 8 2 7 3)
;;   (5 1 8 4 2 7 3 6)
;;   (5 1 8 6 3 7 2 4)
;;   (5 2 4 6 8 3 1 7)
;;   (5 2 4 7 3 8 6 1)
;;   (5 2 6 1 7 4 8 3)
;;   (5 2 8 1 4 7 3 6)
;;   (5 3 1 6 8 2 4 7)
;;   (5 3 1 7 2 8 6 4)
;;   (5 3 8 4 7 1 6 2)
;;   (5 7 1 3 8 6 4 2)
;;   (5 7 1 4 2 8 6 3)
;;   (5 7 2 4 8 1 3 6)
;;   (5 7 2 6 3 1 4 8)
;;   (5 7 2 6 3 1 8 4)
;;   (5 7 4 1 3 8 6 2)
;;   (5 8 4 1 3 6 2 7)
;;   (5 8 4 1 7 2 6 3)
;;   (6 1 5 2 8 3 7 4)
;;   (6 2 7 1 3 5 8 4)
;;   (6 2 7 1 4 8 5 3)
;;   (6 3 1 7 5 8 2 4)
;;   (6 3 1 8 4 2 7 5)
;;   (6 3 1 8 5 2 4 7)
;;   (6 3 5 7 1 4 2 8)
;;   (6 3 5 8 1 4 2 7)
;;   (6 3 7 2 4 8 1 5)
;;   (6 3 7 2 8 5 1 4)
;;   (6 3 7 4 1 8 2 5)
;;   (6 4 1 5 8 2 7 3)
;;   (6 4 2 8 5 7 1 3)
;;   (6 4 7 1 3 5 2 8)
;;   (6 4 7 1 8 2 5 3)
;;   (6 8 2 4 1 7 5 3)
;;   (7 1 3 8 6 4 2 5)
;;   (7 2 4 1 8 5 3 6)
;;   (7 2 6 3 1 4 8 5)
;;   (7 3 1 6 8 5 2 4)
;;   (7 3 8 2 5 1 6 4)
;;   (7 4 2 5 8 1 3 6)
;;   (7 4 2 8 6 1 3 5)
;;   (7 5 3 1 6 8 2 4)
;;   (8 2 4 1 7 5 3 6)
;;   (8 2 5 3 1 7 4 6)
;;   (8 3 1 6 2 5 7 4)
;;   (8 4 1 3 6 2 7 5))
