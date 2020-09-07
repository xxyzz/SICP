#lang racket/base

(rule (grandson ?g ?s)
      (and (is-son-of ?f ?s)
           (is-son-of ?g ?f)))

(rule (is-son-of ?m ?s)
      (or (son ?m ?s)
          (and (wife ?m ?w) ;; stepson
               (son ?w ?s))))

;; https://www.biblegateway.com/passage/?search=+Genesis+4&version=NIV
