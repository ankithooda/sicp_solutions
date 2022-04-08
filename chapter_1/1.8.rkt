#lang racket

(define (cube x)
  (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

(define (cube-root x)
  (cube-iter 1.0 x))