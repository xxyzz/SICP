#lang racket/base

;; different order means different data

(define (name-string person)
  (string-append (map symbol->string person)))

(define (compare-name person-1 person-2)
  (string>? (name-string person-1)
            (name-string person-2)))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (lisp-value compare-name ?person-1 ?person-2)))
