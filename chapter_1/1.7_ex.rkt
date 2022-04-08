#lang racket

; Helper functions

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

; If guess is good-enough?
; return guess
; otherwise improve guess
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
  
(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (improve guess x)
  (average guess (/ x guess)))


(define (improve-until-equal guess x)
  (let ((new-guess (average guess (/ x guess))))
    (if (< (abs (- new-guess guess)) 1)
        (begin
          (display new-guess)
          (newline)
          (display guess))
        (improve-until-equal new-guess x))))
