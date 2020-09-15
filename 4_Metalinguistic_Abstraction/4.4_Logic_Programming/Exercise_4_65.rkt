#lang racket/base

(supervisor (Hacker Alyssa P) (Bitdiddle Ben)) ;; 1
(supervisor (Fect Cy D) (Bitdiddle Ben))       ;; 2
(supervisor (Tweakit Lem E) (Bitdiddle Ben))   ;; 3
(supervisor (Reasoner Louis) (Hacker Alyssa P))
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(supervisor (Scrooge Eben) (Warbucks Oliver))
(supervisor (Cratchet Robert) (Scrooge Eben))  ;; 4
(supervisor (Aull DeWitt) (Warbucks Oliver))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(wheel ?who)
;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

;; database has four unique middle managers supervised by Oliver
