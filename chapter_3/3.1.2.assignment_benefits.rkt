#lang sicp

; Since random-update is not available
; we will use the random function
(define rand
  (let ((x 453783))
    (lambda ()
      ; we need to add a number to this because
      ; for lower values it is possible that we get 0 as a random number
      ; in which case the function fails
      (set! x (+ (random x) 636733))
      x)))
     

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

