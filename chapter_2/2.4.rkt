#lang racket
(define (p-cons x y)
  (define (dispatch flag)
    (cond ((= flag 0) x)
          ((= flag 1) y)
          (else (error "Incorrect call"))))
  dispatch)

(define (p-car p-consed-pair)
  (p-consed-pair 0))

(define (p-cdr p-consed-pair)
  (p-consed-pair 1))

(define (l-cons x y)
  (lambda (m) (m x y)))

(define (l-car z)
  (z (lambda (p q) p)))

(define (l-cdr z)
  (z (lambda (p q) q)))
