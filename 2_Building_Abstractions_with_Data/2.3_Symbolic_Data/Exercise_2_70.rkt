#lang racket/base

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
    
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)      ; symbol
                               (cadr pair))    ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-set)
  (if (= 1 (length ordered-set))
      (car ordered-set)
      (let ([first (car ordered-set)]
            [second (cadr ordered-set)]
            [rest (cddr ordered-set)])
        (successive-merge (adjoin-set (make-code-tree first second)
                                      rest)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (encode-symbol x tree)
  (define (encode-symbol-1 result current-branch)
    (cond [(not (element-of-set? x (symbols current-branch))) (error "no such letter" x)]
          [(leaf? current-branch) result]
          [(element-of-set? x (symbols (left-branch current-branch)))
           (encode-symbol-1 (append result (list 0)) (left-branch current-branch))]
          [else (encode-symbol-1 (append result (list 1)) (right-branch current-branch))]))
  (encode-symbol-1 null tree))

(define tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))
; '((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf BOOM 1) (leaf WAH 1) (BOOM WAH) 2) (A BOOM WAH) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A BOOM WAH SHA JOB GET) 11) (YIP A BOOM WAH SHA JOB GET) 20) (NA YIP A BOOM WAH SHA JOB GET) 36)

(encode '(GET A JOB) tree)
; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0)  14 bits
; fixed-length code: 3 * 3 = 9 bits

(encode '(SHA NA NA NA NA NA NA NA NA) tree)
; '(1 1 1 0 0 0 0 0 0 0 0 0)  12 bits
; fixed-length code: 3 * 9 = 27 bits

(encode '(GET A JOB) tree)
; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0)  14 bits
; fixed-length code: 3 * 3 = 9 bits

(encode '(SHA NA NA NA NA NA NA NA NA) tree)
; '(1 1 1 0 0 0 0 0 0 0 0 0)  12 bits
; fixed-length code: 3 * 9 = 27 bits

(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) tree)
; '(1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)  23 bits
; fixed-length code: 3 * 10 = 30 bits

(encode '(SHA BOOM) tree)
; '(1 1 1 0 1 1 0 1 0) 9 bits
; fixed-length code: 3 * 2 = 6 bits

; total bits: 84
; fixed-length code total bits: 108
