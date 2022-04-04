#lang racket

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))


(define (cube x)
  (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x))
     0.00001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

