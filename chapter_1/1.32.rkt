#lang racket
(define (accumulator combiner null-value term next a b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulator combiner null-value term next (next a) b))))

(define (accumulator-iter combiner null-value term next a b)
  (define (iter result a)
    (if (> a b)
        result
        (iter (combiner result (term a)) (next a))))
  (iter null-value a))
      

(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum a b)
  (accumulator-iter + 0 identity inc a b))

(define (prod a b)
  (accumulator-iter * 1 identity inc a b))