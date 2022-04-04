#lang racket
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x)
                 x)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.0000000001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))