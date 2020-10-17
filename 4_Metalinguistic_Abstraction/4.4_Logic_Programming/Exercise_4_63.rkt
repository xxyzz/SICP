#lang racket/base

(rule (son-of ?m ?s)
      (or (son ?m ?s)
          (and (wife ?m ?w) ;; find m's wife's son
               (son ?w ?s))))

(rule (grandson ?g ?s)
      (and (son-of ?f ?s)
           (son-of ?g ?f)))

(add-assertion! '(son Adam Cain))
(add-assertion! '(son Cain Enoch))
(add-assertion! '(son Enoch Irad))
(add-assertion! '(son Irad Mehujael))
(add-assertion! '(son Mehujael Methushael))
(add-assertion! '(son Methushael Lamech))
(add-assertion! '(wife Lamech Ada))
(add-assertion! '(son Ada Jabal))
(add-assertion! '(son Ada Jubal))

(grandson Cain ?son)
;;; Query results:
(grandson Cain Irad)

(son-of Lamech ?son)
;;; Query results:
(son-of Lamech Jubal)
(son-of Lamech Jabal)

(grandson Methushael ?son)
;;; Query results:
(grandson Methushael Jubal)
(grandson Methushael Jabal)

;; https://www.biblegateway.com/passage/?search=+Genesis+4&version=NIV
