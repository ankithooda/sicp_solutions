#lang racket
(define (dec n)
  (- n 1)
  )

(define (double func n)
  (func (func n))
  )

(define (fibo-rec n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fibo-rec (dec n))
                 (fibo-rec (double dec n))
                 )
              )
        )
  )
