#lang racket
(define (f-for-each proc items)
  
  (define nil `())

  (define (apply-call-next proc item-list)
    (proc (car item-list))
    (f-for-each proc (cdr item-list)))
  
  (if (null? items)
      true
      (apply-call-next proc items)))
      