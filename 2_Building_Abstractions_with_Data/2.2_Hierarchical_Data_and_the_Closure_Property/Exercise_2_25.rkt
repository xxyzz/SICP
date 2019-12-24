#lang racket/base

(define a (list 1 3 (list 5 7) 9))
; '(1 3 (5 7) 9)
(define b (list (list 7)))
; '((7))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; '(1 (2 (3 (4 (5 (6 7))))))

(car (cdaddr a))
(car (cdr (car (cdr (cdr a)))))
; 7

(caar b)
(car (car b))
; 7

(cadadr (cadadr (cadadr c)))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
; 7

; -----------------------------------------------------------------------
; ----------------------------- explation -------------------------------
; -----------------------------------------------------------------------
(list 1 (list 2 3))
(cons 1 (cons (cons 2 (cons 3 null)) null))
; '(1 (2 3))
; 🛑 is null
; 1️⃣ ⬛
;     ↘️
;      ⬛ 🛑
;       ↘️
;      2️⃣ ⬛
;          ↘️
;         3️⃣ 🛑
(cdr (list 1 (list 2 3)))
; '((2 3))
(cadr (list 1 (list 2 3)))
; '(2 3)
; this is the reason that we need cdr then car to loop the list c

(list 1 2 3)
(cons 1 (cons 2 (cons 3 null)))
; '(1 2 3)
;  1️⃣ ⬛
;      ↘️
;     2️⃣ ⬛
;         ↘️
;          3️⃣ 🛑
; just a normal list, not a nested list

(list (list 1 2) 3)
(cons (cons 1 (cons 2 null)) (cons 3 null))
; '((1 2) 3)
;       ⬛ ⬛
;      ↙️   ↘️
;   1️⃣ ⬛    3️⃣ 🛑
;       ↘️
;        2️⃣ 🛑
(cons (cons 1 (cons 2 null)) 3)
; '((1 2) . 3)
;       ⬛ 3️⃣
;      ↙️
;   1️⃣ ⬛
;       ↘️
;        2️⃣ 🛑
; (1 2) and 3 are in a pair not a list

(list 1 (list 2 3) 4)
(cons 1 (cons (cons 2 (cons 3 null)) (cons 4 null)))
; '(1 (2 3) 4)
;   1️⃣ ⬛
;       ↘️
;     ⬛  ⬛
;   ↙️     ↘️
;  2️⃣ ⬛    4️⃣ 🛑
;      ↘️
;       3️⃣ 🛑
(cdr (list 1 (list 2 3) 4))
; '((2 3) 4)
(cadr (list 1 (list 2 3) 4))
; '(2 3)
(cddr (list 1 (list 2 3) 4))
; '(4)
