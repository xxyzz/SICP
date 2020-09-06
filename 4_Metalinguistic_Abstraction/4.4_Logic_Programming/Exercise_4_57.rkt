#lang racket/base

(rule (replace ?person-1 ?person-2)
      (and (job ?person-1 ?person-1-job)
           (job ?person-2 ?person-2-job)
       (or (same ?person-1-job ?person-2-job)
           (can-do-job ?person-1-job ?person-2-job))
       (not (same ?person-1 ?person-2))))

;; a:
(replace ?x (Fect Cy D))

;; b:
(and (replace ?a ?b)
     (salary ?a ?a-salary)
     (salary ?b ?b-salary)
     (lisp-value < ?a-salary ?b-salary))
