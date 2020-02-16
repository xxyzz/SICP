#lang racket/base

(define (make-account balance password)
  (define incorrect-pw-count 0)
  (define incorrect-pw-limit 2)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) "Suck my balls!")
  (define (dispatch input-password m)
    (cond [(not (eq? password input-password))
           (lambda (x)
             (set! incorrect-pw-count (add1 incorrect-pw-count))
             (if (< incorrect-pw-count incorrect-pw-limit)
                 "Incorrect password"
                 (call-the-cops)))]
          [(>= incorrect-pw-count incorrect-pw-limit)
           (lambda (x) (call-the-cops))]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       m)]))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
; 60
((acc 'some-other-password 'deposit) 50)
; "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; "Suck my balls!"
((acc 'secret-password 'withdraw) 40)
; "Suck my balls!"
