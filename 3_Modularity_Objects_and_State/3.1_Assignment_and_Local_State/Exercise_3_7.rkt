#lang racket/base

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password m)
    (cond [(not (eq? password input-password)) (lambda (x) "Incorrect password")]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       m)]))
  dispatch)

(define (make-joint account old-pw new-pw)
  (define (withdraw amount)
    ((account old-pw 'withdraw) amount))
  (define (deposit amount)
    ((account old-pw 'deposit) amount))
  (define (dispatch input-password m)
    (cond [(not (eq? new-pw input-password)) (lambda (x) "Incorrect password")]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       m)]))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 10)
; 90
((paul-acc 'roosebud 'withdraw) 10)
; "Incorrect password"
((peter-acc 'open-sesame 'deposit) 20)
; 110
