#lang racket/base

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (reverse () ()))
(rule (reverse (?x ?y)
               (append-to-form (?first-of-x) ?rest-of-x ?x)
               (append-to-form ?rest-of-y (?first-of-x) ?y)
               (reverse (?rest-of-x) (?rest-of-y))))

(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

;; should check this(and all the other 4.4 exercises)
;; after reading next chapter(4.4.4)
