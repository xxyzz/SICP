#lang racket/base

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?position))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not (job ?boss (?division . ?boss-position)))
                    (not (big-shot? ?boss ?division))))))
