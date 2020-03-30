#lang racket/base

; a. in terms of mutexes
(define (make-semaphore n)
  (let ([mutex (make-mutex)])
    (define (the-semaphore m)
      (cond [(eq? m 'wait)
             (mutex 'acquire)
             (if (> n 0)
                 (begin (set! n (sub1 n))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'wait)))]
            [(eq? m 'post)
             (mutex 'acquire)
             (set! n (add1 n))
             (mutex 'release)]))
    the-semaphore))

; b. interms of atomic `test-and-set!` operations.
(define (make-semaphore n)
  (let ([cell (mcons #f null)])
    (define (the-semaphore m)
      (cond [(eq? m 'wait)
             (if (test-and-set! cell)
                 (the-semaphore 'wait)
                 (if (> n 0)
                     (begin (set! n (sub1 n))
                            (clear! cell))
                     (begin (clear! cell)
                            (the-semaphore 'wait))))]
            [(eq? m 'post)
             (if (test-and-set! cell)
                 (the-semaphore 'post)
                 (begin (set! n (add1 n))
                        (clear! cell)))]))
    the-semaphore))
