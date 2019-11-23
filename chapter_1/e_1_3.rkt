#lang racket
(define (sum-of-sq a b)
  (+ (* a a) (* b b))
  )

(define (get-greater a b)
  (if (> a b)
      a b)
  )

(define (solution a b c)
  (if (> a b)
      (sum-of-sq a (get-greater b c))
      (sum-of-sq b (get-greater a c))
      )
  )

