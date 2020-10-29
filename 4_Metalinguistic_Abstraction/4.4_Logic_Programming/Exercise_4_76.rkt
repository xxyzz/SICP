#lang racket/base

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-streams (qeval (first-conjunct conjuncts) frame-stream)
                           (conjoin (rest-conjuncts conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (merge-frame-streams s1 s2)
  (stream-flatmap
   (lambda (frame1)
     (stream-flatmap
      (lambda (frame2)
        (let ([frame (merge-frames frame1 frame2)])
          (if (null? frame)
              empty-stream
              (singleton-stream frame))))
      s2))
   s1))

(define (merge-frames f1 f2)
  (cond [(null? f1) f2]
        [(null? f2) f1]
        [else
         (let* ([binding1 (car f1)]
                [var (binding-variable binding1)]
                [binding2 (binding-in-frame var f2)])
           (if binding2
               (let ([val1 (find-final-value var f1)]
                     [var2 (find-final-value var f2)])
                 (if (equal? val1 var2)
                     (merge-frames (cdr f1) (cons binding1 f2))
                     null))
               (merge-frames (cdr f1) (cons binding1 f2))))]))

(define (find-final-value var frame)
  (let ([binding (binding-in-frame var frame)])
    (if binding
        (let ([val (binding-value binding)])
          (if (var? val)
              (find-final-value (binding-value val) frame)
              val))
        null)))

;; tests
(and (address ?p ?l)
     (job ?p (computer . ?j)))
;;; Query results:
(and (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)) (job (Reasoner Louis) (computer programmer trainee)))
(and (address (Tweakit Lem E) (Boston (Bay State Road) 22)) (job (Tweakit Lem E) (computer technician)))
(and (address (Fect Cy D) (Cambridge (Ames Street) 3)) (job (Fect Cy D) (computer programmer)))
(and (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)) (job (Hacker Alyssa P) (computer programmer)))
(and (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)) (job (Bitdiddle Ben) (computer wizard)))
