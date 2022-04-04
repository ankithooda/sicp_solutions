#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
  
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (good-enough? guess x)
  (define tolerance 0.000000000000000000000000000001)
  (< (abs (- (square guess) x))
     tolerance))

(define (improve guess x)
  (average guess (/ x guess)))

