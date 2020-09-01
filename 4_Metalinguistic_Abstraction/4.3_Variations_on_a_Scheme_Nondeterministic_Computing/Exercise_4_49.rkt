#lang sicp

(define (require p) (if (not p) (amb)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a an))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (find-word words)
  (if (null? words)
      (amb)
      (amb (car words) (find-word (cdr words)))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  ;; (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (find-word (cdr word-list))])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(parse '(the professor lectures to the student in the class with the cat))
;; (sentence
;;  (simple-noun-phrase (article the) (noun student))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;;     (verb studies)
;;     (prep-phrase
;;      (prep for)
;;      (simple-noun-phrase (article the) (noun student))))
;;    (prep-phrase
;;     (prep for)
;;     (simple-noun-phrase (article the) (noun student))))
;;   (prep-phrase
;;    (prep for)
;;    (simple-noun-phrase (article the) (noun student)))))
