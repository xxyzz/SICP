#lang racket/base

;; 1: all people supervised by Ben Bitdiddle
(supervisor ?name (Bitdiddle Ben))
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;; (supervisor (Fect Cy D) (Bitdiddle Ben))
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; 2: the names and jobs of all people in the accounting division
(job ?name (accounting . ?position))
;; (job (Cratchet Robert) (accounting scrivener))
;; (job (Scrooge Eben) (accounting chief accountant))

;; 3: the names and addresses of all people who live in Slumerville
(address ?name (Slumerville ?street ?number))
;; (address (Aull DeWitt) (Slumerville (Onion Square) 5))
;; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
;; (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))

;; add data before (query-driver-loop)
(add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-assertion! '(job (Bitdiddle Ben) (computer wizard)))
(add-assertion! '(salary (Bitdiddle Ben) 60000))

(add-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-assertion! '(salary (Hacker Alyssa P) 40000))
(add-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(add-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(add-assertion! '(job (Fect Cy D) (computer programmer)))
(add-assertion! '(salary (Fect Cy D) 35000))
(add-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(add-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-assertion! '(job (Tweakit Lem E) (computer technician)))
(add-assertion! '(salary (Tweakit Lem E) 25000))
(add-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(add-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
(add-assertion! '(salary (Reasoner Louis) 30000))
(add-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))

(add-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(add-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-assertion! '(job (Warbucks Oliver) (administration big wheel)))
(add-assertion! '(salary (Warbucks Oliver) 150000))

(add-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(add-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
(add-assertion! '(salary (Scrooge Eben) 75000))
(add-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(add-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-assertion! '(job (Cratchet Robert) (accounting scrivener)))
(add-assertion! '(salary (Cratchet Robert) 18000))
(add-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))

(add-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-assertion! '(job (Aull DeWitt) (administration secretary)))
(add-assertion! '(salary (Aull DeWitt) 25000))
(add-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(add-assertion! '(can-do-job (computer wizard) (computer programmer)))
(add-assertion! '(can-do-job (computer wizard) (computer technician)))
(add-assertion! '(can-do-job (computer programmer) (computer programmer trainee)))
(add-assertion! '(can-do-job (administration secretary) (administration big wheel)))
