#lang racket/base

(rule (same ?x ?x))

(rule (replace ?person-1 ?person-2)
      (and (job ?person-1 ?person-1-job)
           (job ?person-2 ?person-2-job)
           (or (same ?person-1-job ?person-2-job)
               (can-do-job ?person-1-job ?person-2-job))
           (not (same ?person-1 ?person-2))))

;; a:
(replace ?x (Fect Cy D))
;; (replace (Bitdiddle Ben) (Fect Cy D))
;; (replace (Hacker Alyssa P) (Fect Cy D))

;; b:
(and (replace ?a ?b)
     (salary ?a ?a-salary)
     (salary ?b ?b-salary)
     (lisp-value < ?a-salary ?b-salary))
;; (and (replace (Aull DeWitt) (Warbucks Oliver)) (salary (Aull DeWitt) 25000) (salary (Warbucks Oliver) 150000) (lisp-value < 25000 150000))
;; (and (replace (Fect Cy D) (Hacker Alyssa P)) (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (lisp-value < 35000 40000))
