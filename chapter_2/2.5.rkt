#lang racket

(define (even? x)
  (= (remainder x 2) 0))

(define (n-cons num1 num2)
  (* (expt 2 num1)
     (expt 3 num2)))

(define (n-car cons-num)
  (if (= (remainder cons-num 2) 0)
      (+ (n-car (/ cons-num 2)) 1)
      0))

(define (n-cdr cons-num)
  (if (= (remainder cons-num 3) 0)
      (+ (n-cdr (/ cons-num 3)) 1)
      0))