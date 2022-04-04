#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (same-sign? x y)
  (< 0 (/ x y)))
 

(define (make-rat n d)
  (let ((divisor (gcd n d)))
    (cons (/ n divisor) (/ d divisor))))

(define (numer rat-number)
  (car rat-number))

(define (denom rat-number)
  (cdr rat-number))
  
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (eq-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
