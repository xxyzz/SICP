#lang racket/base

;; a:
(meeting ?division (Friday ?time))

;; b:
(rule (meeting-time ?person ?day-and-time)
      (or (metting whole-company ?day-and-time)
          (and (job ?pseron (?division . ?type))
               (metting ?division ?day-and-time))))

;; c:
(metting-time? (Hacker Alyssa P) (Wednesday ?time))
