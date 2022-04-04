#lang racket

(define nil `())

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (f-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (f-map proc (cdr items)))))

(define (new-square-list items factor)
  (f-map (lambda (x) (* x factor)) items))

(define (square x) (* x x))