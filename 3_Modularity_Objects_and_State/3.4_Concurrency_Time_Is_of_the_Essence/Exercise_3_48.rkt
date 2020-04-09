#lang racket/base

(define mutex (make-mutex))
(define account-count 0)

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ([balance-serializer (make-serializer)]
        [account-number 0])
    (mutex 'acquire)
    (set! account-number account-count)
    (set! account-count (add1 account-count))
    (mutex 'release)
    (define (dispatch m)
      (cond [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'balance) balance]
            [(eq? m 'serializer) balance-serializer]
            [(eq? m 'account-number) account-number]
            [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ([serializer1 (account1 'serializer)]
        [serializer2 (account2 'serializer)]
        [account1-number (account1 'account-number)]
        [account2-number (account2 'account-number)])
    (if (account1-number < account2-number)
      ((serializer1 (serializer2 exchange))
      account1
      account2)
      ((serializer2 (serializer1 exchange))
      account1
      account2))))
