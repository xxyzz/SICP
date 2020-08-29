#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (a-pythagorean-triple-between low high)
  (let ([i (an-integer-between low high)]
        [hsq (* high high)])
    (let ([j (an-integer-between i high)])
      (let ([ksq (+ (* i i) (* j j))])
        (require (>= hsq ksq))
        (let ([k (sqrt ksq)])
          (require (integer? k))
          (list i j k))))))

(define start (runtime))
(a-pythagorean-triple-between 1 100)
(display (- (runtime) start))
(display " microseconds\n")
;; (3 4 5)
;; 114834 microseconds

(define (old-a-pythagorean-triple-between low high)
  (let ([i (an-integer-between low high)])
    (let ([j (an-integer-between i high)])
      (let ([k (an-integer-between j high)])
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define old-start (runtime))
(old-a-pythagorean-triple-between 1 100)
(display (- (runtime) old-start))
(display " microseconds\n")
;; (3 4 5)
;; 2844622 microseconds

;; It now find two numbers instead of three. If the old procedure
;; needs to check more numbers, the new one indeed faster.

(a-pythagorean-triple-between 1 5)
;; 1812 microseconds
(old-a-pythagorean-triple-between 1 5)
;; 700 microseconds hey!
