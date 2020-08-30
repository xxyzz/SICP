#lang racket/base
(require racket/list)

(define (multiple-dwelling)
  (for*/list ([fletcher '(2 3 4)]
              [cooper '(2 3 4 5)]
              #:when (not (= fletcher cooper))
              #:when (not (= (abs (- fletcher cooper)) 1))
              [smith (in-range 1 6)]
              #:when (not (= smith fletcher))
              #:when (not (= smith cooper))
              #:when (not (= (abs (- smith fletcher)) 1))
              [miller (in-range 1 6)]
              #:when (not (= miller fletcher))
              #:when (not (= miller cooper))
              #:when (not (= miller smith))
              #:when (> miller cooper)
              [baker (in-range 1 5)]
              #:when (not (= baker fletcher))
              #:when (not (= baker cooper))
              #:when (not (= baker smith))
              #:when (not (= baker miller)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

(define (filter-combination baker cooper fletcher miller smith)
  (and (not (= baker 5))
       (not (= cooper 1))
       (not (= fletcher 5))
       (not (= fletcher 1))
       (> miller cooper)
       (not (= (abs (- smith fletcher)) 1))
       (not (= (abs (- fletcher cooper)) 1))))

(define (another-multiple-dwelling) ;; sweet
  (for/list ([floors (permutations (range 1 6))]
             #:when (apply filter-combination floors))
    (map list
         '(baker cooper fletcher miller smith)
         floors)))

(multiple-dwelling)
;; '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
(another-multiple-dwelling)
;; '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
