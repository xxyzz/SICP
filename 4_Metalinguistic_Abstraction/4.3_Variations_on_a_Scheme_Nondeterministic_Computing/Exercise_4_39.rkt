#lang sicp

(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond [(null? items) true]
        [(null? (cdr items)) true]
        [(member (car items) (cdr items)) false]
        [else (distinct? (cdr items))]))

(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

;; check right after each one is selected
(define (faster-multiple-dwelling)
  (let ([fletcher (amb 2 3 4)]
        [cooper (amb 2 3 4 5)])
    (require (not (= cooper fletcher)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (let ([smith (amb 1 2 3 4 5)])
      (require (distinct? (list fletcher cooper smith)))
      (require (not (= (abs (- smith fletcher)) 1)))
      (let ([miller (amb 1 2 3 4 5)])
        (require (distinct? (list fletcher cooper smith miller)))
        (require (> miller cooper))
        (let ([baker (amb 1 2 3 4)])
          (require (distinct? (list fletcher cooper smith miller baker)))
          (list (list 'baker baker) (list 'cooper cooper)
                (list 'fletcher fletcher) (list 'miller miller)
                (list 'smith smith)))))))

(define start (runtime))
(multiple-dwelling)
(display (- (runtime) start))
(display " microseconds\n")
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; 19362 microseconds

(define faster-start (runtime))
(faster-multiple-dwelling)
(display (- (runtime) faster-start))
(display " microseconds\n")
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; 2025 microseconds

;; order doesn't affect result but does affect speed
