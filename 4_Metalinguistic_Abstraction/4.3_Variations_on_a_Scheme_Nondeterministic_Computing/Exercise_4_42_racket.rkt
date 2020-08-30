#lang racket/base
(require racket/list)

(define (filter-rank betty ethel joan kitty marry)
  (let ([betty-s1 (= kitty 2)]
        [betty-s2 (= betty 3)]
        [ethel-s1 (= ethel 1)]
        [ethel-s2 (= joan 2)]
        [joan-s1 (= joan 3)]
        [joan-s2 (= ethel 5)]
        [kitty-s1 (= kitty 2)]
        [kitty-s2 (= marry 4)]
        [marry-s1 (= marry 4)]
        [marry-s2 (= betty 1)])
    (and (not (eq? betty-s1 betty-s2))
         (not (eq? ethel-s1 ethel-s2))
         (not (eq? joan-s1 joan-s2))
         (not (eq? kitty-s1 kitty-s2))
         (not (eq? marry-s1 marry-s2)))))

(define (retarded-exam-rank)
  (for/list ([ranks (permutations (range 1 6))]
             #:when (apply filter-rank ranks))
    (map list
         '(betty ethek joan kitty marry)
         ranks)))

(retarded-exam-rank)
;; '(((betty 3) (ethek 5) (joan 2) (kitty 1) (marry 4)))
