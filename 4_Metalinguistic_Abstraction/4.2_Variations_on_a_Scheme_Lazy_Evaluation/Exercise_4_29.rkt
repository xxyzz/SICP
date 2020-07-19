#lang racket/base

;; the ultimate version of Fibonacci procedure(exercise 1.19)
;; uses several times of it's parameters, it will benefit
;; from this memorization
;; see exercise 3.27 for another memoization approach

(define (fib n)
  (define (fib-iter a b p q count)
    (cond [(= count 0) b]
          [(even? count)
           (fib-iter a
                     b
                     (+ (* q q) (* p p))   ;; compute p′
                     (+ (* 2 p q) (* q q)) ;; compute q′
                     (/ count 2))]
          [else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))]))
  (fib-iter 1 0 0 1 n))

(define (fib2 n)
  (define (iter a b k)
    (if (= k 0)
        b
        (iter (+ a b) a (- k 1))))
  (iter 1 0 n))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; add even? to primitive-procedures

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([start (current-inexact-milliseconds)]
          [output
           (actual-value
            input the-global-environment)])
      (define end (current-inexact-milliseconds))
      (announce-output output-prompt)
      (user-print output)
      (displayln (format "ms: ~a" (- end start)))))
  (driver-loop))

;; no memorize
;; take fib2 for example
(fib2 4) ;; in fib2-env, n is (thunk 4 gl-env)
(iter 1 0 n)
;; a is (thunk 1 fib2-env), b is (thunk 0 fib2-env)
;; k is (thunk n fib2-env)
(iter (+ a b) a (- k 1))
;; a is (thunk (+ a b) iter-env), b is (thunk a iter-env)
;; k is (thunk (- k 1) iter-env)
;; without memoization, evaluated each parameter will search
;; all the way back to the beginning environment
;; it turns a linear factorial code into quadratic growth
;; and turn linear fib2 back to tree recursion

(fib 10)
;; ms: 0.555908203125
(fib 100)
;; ms: 33.19189453125
(fib 10000)
;; wait for it if you have nothing to do
(fib2 10)
;; ms: 0.43603515625
(fib2 20)
;; ms: 25.77001953125
(fib2 40)
;; eh
(factorial 10)
;; ms: 0.2750244140625
(factorial 20)
;; ms: 0.84697265625
(factorial 40)
;; ms: 3.0660888671875 about four times slower

;; memorize
(fib 10)
;; ms: 0.172119140625
(fib 100)
;; ms: 0.2919921875
(fib 10000)
;; ms: 0.51708984375
(fib2 10)
;; ms: 0.1328125
(fib2 20)
;; ms: 0.23193359375
(fib2 40)
;; ms: 0.427978515625
(factorial 10)
;; ms: 0.10791015625
(factorial 20)
;; ms: 0.192138671875
(factorial 40)
;; ms: 0.363037109375

(define (square x) (* x x))
;; square runs faster with memoization enabled
;; x is evaluated only once

;; no memorize
(square (id 10))
;; 100
count
;; 2

;; memorize
(square (id 10))
;; 100
count
;; 1

