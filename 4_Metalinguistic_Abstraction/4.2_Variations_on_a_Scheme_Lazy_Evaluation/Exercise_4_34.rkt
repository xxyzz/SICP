#lang racket/base

(define (cons x y) (lazy-pair (m) (m x y))) ;; input

(define (lazy-pair? exp) (tagged-list? exp 'lazy-pair))
(define (make-lazy-pair parameters body env)
  (list 'lazy-pair parameters body env))

(define (compound-procedure? p)
  (or (tagged-list? p 'procedure)
      (lazy-pair? p)))

(define (eval exp env)
  (cond [(lazy-pair? exp) ;; before application?
         (make-lazy-pair (lambda-parameters exp)
                         (lambda-body exp)
                         env)]))

(define (user-print object)
  (cond [(lazy-pair? object) (display-lazy-pair object 0)]
        [(compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))]
        [else (display object)]))

(define (display-lazy-pair object len)
  (display "(")
  (let* ([env (cadddr object)]
         [first (actual-value 'x env)]
         [second (actual-value 'y env)])
    (if (lazy-pair? first)
        (display-lazy-pair first len) ;; tree
        (display first))
    (cond [(lazy-pair? second) ;; list
           (if (< len 5)
               (begin
                 (display " ")
                 (display-lazy-pair second (add1 len)))
               (display " ..."))]
          [(not (null? second)) ;; pair
           (display " . ")
           (display second)]))
  (display ")"))

(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (define-variable! 'null null initial-env)
    initial-env))

;; remove cons, car, cdr and list from primitive-procedures
;; inputs:
;; (define (cons x y) (lazy-pair (m) (m x y)))
;; (define (car z) (z (lambda (p q) p)))
;; (define (cdr z) (z (lambda (p q) q)))
;; (cons 1 2)
;; (cons 1 (cons 2 null))
;;
;; (define (add-lists list1 list2)
;;   (cond ((null? list1) list2)
;;         ((null? list2) list1)
;;         (else (cons (+ (car list1) (car list2))
;;                     (add-lists (cdr list1) (cdr list2))))))
;; (define ones (cons 1 ones))
;; (define integers (cons 1 (add-lists ones integers)))
;; this naive approach prints extra parentheses
