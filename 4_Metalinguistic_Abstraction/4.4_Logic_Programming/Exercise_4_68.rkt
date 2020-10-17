#lang racket/base

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?y) ?z)
               (and (reverse ?y ?w)
                    (append-to-form ?w (?x) ?z))))
;; change order
(assert! (rule (reverse (?x . ?y) ?z)
               (and (append-to-form ?w (?x) ?z)
                    (reverse ?y ?w))))

(reverse (1 2 3) ?x)
;;; Query results:
(reverse (1 2 3) (3 2 1))

(reverse ?x (1 2 3))
;; can't complete
;; swap and rule order:
;;; Query results:
(reverse (3 2 1) (1 2 3))
