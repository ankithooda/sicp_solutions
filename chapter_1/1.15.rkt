#lang racket
(define (cube x)
  (* x x x))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (sine-meta x)
  (- (* 3 (sine (/ x 3)))
     (* 4 (cube (sine (/ x 3))))))

(define (sine angle)
  (writeln angle)
  (if (< (abs angle) 0.1)
      angle
      (sine-meta angle)))

(define (p x)
  (- (* 3 x)
     (* 4 (cube x))))
  
(define (new-sine angle)
  (writeln angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))