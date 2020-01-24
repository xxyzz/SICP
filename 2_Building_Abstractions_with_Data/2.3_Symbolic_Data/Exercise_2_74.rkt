#lang racket/base
(require racket/list)

(define division1-file
  '(division1
    ("Winnie" (address "174 Chang'an Avenue") (salary 1))
    ("Pootie-Poot" (address "Kremlin Senate") (salary 2))
    ("Rocket Man" (address "Ryongsong Residence") (salary 3))))

(define division2-file
  '(division2
    ((name "Dotard") (wage 1) (place "White House"))))

(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref table (list op type)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (get-record-division1 name file)
  (let ([record (filter (lambda (employee) (eq? name (first employee)))
                        (contents file))])
    (if (null? record)
        null
        (first record))))

(put 'get-record 'division1 get-record-division1)

(define (get-record-division2 name file)
  (let ([record (filter (lambda (employee) (eq? name (cadar employee)))
                        (contents file))])
    (if (null? record)
        null
        (first record))))

(put 'get-record 'division2 get-record-division2)

(define (get-record name file)
  ((get 'get-record (type-tag file)) name file))

; test get-record
(get-record "Rocket Man" division1-file)
; '("Rocket Man" (address "Ryongsong Residence") (salary 3))
(get-record "Dotard" division2-file)
; '((name "Dotard") (wage 1) (place "White House"))

(define (get-salary-division1 name file)
  (let ([record (get-record name file)])
    (if (null? record)
        null
        (cadr (third record)))))

(put 'get-salary 'division1 get-salary-division1)

(define (get-salary-division2 name file)
  (let ([record (get-record name file)])
    (if (null? record)
        null
        (cadr (second record)))))

(put 'get-salary 'division2 get-salary-division2)

(define (get-salary name file)
  ((get 'get-salary (type-tag file)) name file))

; test get-salary
(get-salary "Rocket Man" division1-file)
; 3
(get-salary "Dotard" division2-file)
; 1

(define (find-employee-record name files)
  (let ([record (filter (lambda (x) (not (null? x)))
                        (map (lambda (file)
                               (get-record name file))
                             files))])
    (if (null? record)
        null
        (first record))))

; test find-employee-record
(find-employee-record "Dotard" (list division1-file division2-file))
; '((name "Dotard") (wage 1) (place "White House"))
