#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define nil `())

(define (same-parity first . rest)
  (define parity (remainder first 2))
  (define (iter items parity)
    (cond ((null? items) nil)
          ((= (remainder (car items) 2) parity)
           (cons (car items) (iter (cdr items) parity)))
          (else (iter (cdr items) parity))))
  (cons first (iter rest parity)))
        
        
        
    