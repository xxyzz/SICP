#lang racket/base
(require racket/list) ;; permutations, range

(define (filter-rank betty ethel joan kitty marry)
  (and (not (eq? (= kitty 2) (= betty 3)))
       (not (eq? (= ethel 1) (= joan 2)))
       (not (eq? (= joan 3) (= ethel 5)))
       (not (eq? (= kitty 2) (= marry 4)))
       (not (eq? (= marry 4) (= betty 1)))))

(define (retarded-exam-rank)
  (for/list ([ranks (permutations (range 1 6))]
             #:when (apply filter-rank ranks))
    (map list
         '(betty ethek joan kitty marry)
         ranks)))

(retarded-exam-rank)
;; '(((betty 3) (ethek 5) (joan 2) (kitty 1) (marry 4)))
