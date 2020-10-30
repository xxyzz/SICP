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
  (cond [(null? f2) f1]
        [(eq? f2 'failed) null]
        [(null? f1) f2]
        [else
         (let* ([binding (car f1)]
                [var (binding-variable binding)]
                [val (binding-value binding)]
                [frame (extend-if-possible var val f2)])
           (merge-frames (cdr f1) frame))]))

;; tests
(and (address ?p ?l)
     (job ?p (computer . ?j)))
;;; Query results:
(and (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)) (job (Reasoner Louis) (computer programmer trainee)))
(and (address (Tweakit Lem E) (Boston (Bay State Road) 22)) (job (Tweakit Lem E) (computer technician)))
(and (address (Fect Cy D) (Cambridge (Ames Street) 3)) (job (Fect Cy D) (computer programmer)))
(and (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)) (job (Hacker Alyssa P) (computer programmer)))
(and (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)) (job (Bitdiddle Ben) (computer wizard)))

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))
(and (append-to-form (1 2) (3 4) ?x)
     (append-to-form (1) ?y ?x))
;;; Query results:
(and (append-to-form (1 2) (3 4) (1 2 3 4)) (append-to-form (1) (2 3 4) (1 2 3 4)))
