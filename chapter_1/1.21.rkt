#lang racket
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides a b)
  (= (remainder a b) 0))

(define (square n)
  (* n n))

(define (find-divisor n candidate)
  (cond ((> (square candidate) n) n)
        ((divides n candidate) candidate)
        (else (find-divisor n (+ candidate 1)))))


  