#lang sicp

(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond [(null? items) true]
        [(null? (cdr items)) true]
        [(member (car items) (cdr items)) false]
        [else (distinct? (cdr items))]))

(define (verify daughter-yacht parker)
  (if (eq? (car daughter-yacht) "Gabrielle")
      (require (eq? (cdr daughter-yacht) (car parker)))))

(define (weirdos)
  (let ([moore (cons "Mary Ann" "Lorna")]
        [barnacle (cons "Melissa" "Gabrielle")]
        [parker (cons (amb "Lorna" "Rosalind") "Mary Ann")]
        [colonel (cons (amb "Lorna" "Rosalind" "Gabrielle") "Melissa")])
    (require (not (eq? (car parker) (car colonel))))
    (verify colonel parker)
    (let ([hall (cons (amb "Lorna" "Gabrielle") "Rosalind")])
      (require (distinct? (list (car parker) (car colonel) (car hall))))
      (verify hall parker)
      (list (list "Mr. Moore" (car moore))
            (list "Colonel Downing" (car colonel))
            (list "Mr. Hall" (car hall))
            (list "Sir Barnacle Hood" (car barnacle))
            (list "Dr. Parker" (car parker))))))

(weirdos)
;; (("Mr. Moore" "Mary Ann") ("Colonel Downing" "Lorna") ("Mr. Hall" "Gabrielle") ("Sir Barnacle Hood" "Melissa") ("Dr. Parker" "Rosalind"))

;; Mr. Moore's daughter is unknown:
(define (weird-dads)
  (let ([barnacle (cons "Melissa" "Gabrielle")]
        [parker (cons (amb "Lorna" "Rosalind") "Mary Ann")]
        [moore (cons (amb "Rosalind" "Gabrielle" "Mary Ann") "Lorna")])
    (require (not (eq? (car parker) (car moore))))
    (verify moore parker)
    (let ([colonel (cons (amb "Lorna" "Rosalind" "Gabrielle" "Mary Ann") "Melissa")])
      (require (distinct? (list (car parker) (car moore) (car colonel))))
      (verify colonel parker)
      (let ([hall (cons (amb "Lorna" "Gabrielle" "Mary Ann") "Rosalind")])
        (require (distinct? (list (car parker) (car moore) (car colonel) (car hall))))
        (verify hall parker)
        (list (list "Mr. Moore" (car moore))
              (list "Colonel Downing" (car colonel))
              (list "Mr. Hall" (car hall))
              (list "Sir Barnacle Hood" (car barnacle))
              (list "Dr. Parker" (car parker)))))))

(weird-dads)
;; (("Mr. Moore" "Gabrielle") ("Colonel Downing" "Rosalind") ("Mr. Hall" "Mary Ann") ("Sir Barnacle Hood" "Melissa") ("Dr. Parker" "Lorna"))
;; (("Mr. Moore" "Mary Ann") ("Colonel Downing" "Lorna") ("Mr. Hall" "Gabrielle") ("Sir Barnacle Hood" "Melissa") ("Dr. Parker" "Rosalind"))
