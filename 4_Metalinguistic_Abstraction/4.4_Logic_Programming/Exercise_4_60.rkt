#lang racket/base
(require racket/string) ;; string-append*

;; different order means different data

(define (name->string person)
  (string-append* (map symbol->string person)))

(define (name>? person-1 person-2)
  (string>? (name->string person-1)
            (name->string person-2)))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (lisp-value name>? ?person-1 ?person-2)))

;;; Query results:
(lives-near (Reasoner Louis) (Aull DeWitt))
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Bitdiddle Ben) (Aull DeWitt))
