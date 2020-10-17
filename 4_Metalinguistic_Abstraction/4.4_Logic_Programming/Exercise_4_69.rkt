#lang racket/base

(assert! (rule (son-of ?m ?s)
               (or (son ?m ?s)
                   (and (wife ?m ?w) ;; find m's wife's son
                        (son ?w ?s)))))

(assert! (rule (grandson ?g ?s)
               (and (son-of ?f ?s)
                    (son-of ?g ?f))))

(assert! (rule (last-pair (?x) (?x))))
(assert! (rule (last-pair (?u . ?v) (?x))
               (last-pair ?v (?x))))

(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))

(assert! (rule ((great . ?rel) ?x ?y)
               (and (son-of ?x ?s)
                    (?rel ?s ?y)
                    (last-pair ?rel (grandson)))))

((great grandson) ?g ?ggs)
;;; Query results:
((great grandson) Mehujael Jubal)
((great grandson) Irad Lamech)
((great grandson) Mehujael Jabal)
((great grandson) Enoch Methushael)
((great grandson) Cain Mehujael)
((great grandson) Adam Irad)

(?relationship Adam Irad)
;;; Query results:
((great grandson) Adam Irad)
