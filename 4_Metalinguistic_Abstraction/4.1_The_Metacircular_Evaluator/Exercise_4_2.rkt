#lang racket/base

; a:
; eval will try to evaluate x before it is defined

; b:
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))