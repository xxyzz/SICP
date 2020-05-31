#lang racket/base

;; move tag to the end only need to modify tagged-list?

(define (last-exp? seq) (null? (cdr seq)))

(define (last-ele seq)
  (if (last-exp? seq)
      (car seq)
      (last-ele (cdr seq))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (last-ele exp) tag)
      #f))
