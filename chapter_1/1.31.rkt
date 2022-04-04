#lang racket
(define (inc x) (+ x 1))

(define (identity x) x)

(define (product term next a b)
  (if (> a b)
      1
      (* (term a)
         (product term next (next a) b))))

(define (product-iter term next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (even? x)
  (= (remainder x 2) 0))

(define (factorial n)
  (product-iter identity inc 1 n))

(define (pi-approx limit)
  (define (term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (define (inc x) (+ x 1))
  (* (product-iter term inc 1 limit) 4))