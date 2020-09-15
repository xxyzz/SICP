#lang racket/base

(rule (grandson ?g ?s)
      (and (is-son-of ?f ?s)
           (is-son-of ?g ?f)))

(rule (is-son-of ?m ?s)
      (or (son ?m ?s)
          (and (wife ?m ?w) ;; stepson
               (son ?w ?s))))

(rule (end-with-grandson (grandson)))

(rule (end-with-grandson (?x . ?y))
      (end-with-grandson ?y))

(rule ((grandson) ?x ?y)
      (grandson ?x ?y))

(rule ((great . ?rel) ?x ?y)
      (and (end-with-grandson ?rel)
           (son ?x ?s)
           (?rel ?s ?y)))
