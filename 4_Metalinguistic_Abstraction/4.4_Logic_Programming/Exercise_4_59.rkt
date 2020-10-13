#lang racket/base

;; a:
(meeting ?division (Friday ?time))
;; (meeting administration (Friday 1pm))

;; b:
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?type))
               (meeting ?division ?day-and-time))))
(meeting-time (Hacker Alyssa P) ?time)
;; (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
;; (meeting-time (Hacker Alyssa P) (Wednesday 3pm))

;; c:
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
;; (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
;; (meeting-time (Hacker Alyssa P) (Wednesday 3pm))

;; add data
(add-assertion! '(meeting accounting (Monday 9am)))
(add-assertion! '(meeting administration (Monday 10am)))
(add-assertion! '(meeting computer (Wednesday 3pm)))
(add-assertion! '(meeting administration (Friday 1pm)))
(add-assertion! '(meeting whole-company (Wednesday 4pm)))
