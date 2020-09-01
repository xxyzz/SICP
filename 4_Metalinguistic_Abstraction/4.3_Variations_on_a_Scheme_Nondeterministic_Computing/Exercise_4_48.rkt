#lang sicp

(define (require p) (if (not p) (amb)))

(define nouns '(noun student professor cat class))
(define verbs '(verb farts studies lectures eats sleeps))
(define articles '(article the a an))
(define prepositions '(prep for to in by with))
(define adjectives '(adjective drunk))
(define adverbs '(adverb loudly))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-simple-verb-phrase)
  (let ([verb (parse-word verbs)])
    (amb verb
         (list 'simple-verb-phrase
               verb
               (parse-word adverbs)))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb-phrase)))

(define (parse-simple-noun-phrase)
  (let ([article (parse-word articles)])
    (amb
     (list 'simple-noun-phrase
           article
           (parse-word nouns))
     (list 'simple-noun-phrase
           article
           (parse-word adjectives)
           (parse-word nouns)))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (car *unparsed*)])
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

(parse '(the professor farts loudly to the drunk student))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (simple-verb-phrase (verb farts) (adverb loudly))
;;   (prep-phrase
;;    (prep to)
;;    (simple-noun-phrase (article the) (adjective drunk) (noun student)))))
