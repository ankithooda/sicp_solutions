#lang racket
(define (double x)
  (* x x))

(define (halve x)
  (/ x 2))

(define (larger-num x y)
  (if (x > y)
      x
      y))

(define (m-multiply x counter product)
  (cond ((= counter 0) product)
        ((= counter 1) (+ x product))
        (else 