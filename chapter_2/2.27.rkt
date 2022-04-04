#lang racket


(define (deep-reverse items)
  (define (iter reversed-items items)
    (cond ((null? items) reversed-items)
          ((list? (car items)) (iter (cons (deep-reverse (car items)) reversed-items) (cdr items)))
          (else (iter (cons (car items) reversed-items) (cdr items)))))
  (iter `() items))
    
  