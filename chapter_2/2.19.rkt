#lang racket

(define us-coins (list 50 25 10 5 1))

(define us-coins-dis (list 10 1 5 50 25))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-coin coins)
  (car coins))

(define (except-first coins)
  (cdr coins))

(define (no-more coins)
  (null? coins))

(define (count-change amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more coins)) 0)
        (else (+ (count-change (- amount (first-coin coins)) coins)
                 (count-change amount (except-first coins))))))
  