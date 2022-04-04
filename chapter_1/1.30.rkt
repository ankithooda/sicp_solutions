#lang racket
(define (sum term next a b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-integers a b)
  (sum identity inc a b))

(define (sum-iter term next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (inc a) (+ (term a) result))))
  (iter a 0))