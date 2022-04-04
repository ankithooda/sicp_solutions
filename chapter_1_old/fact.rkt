#lang sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
      )
  )

(define (i_factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product) (+ counter 1))
        )
    )
  (fact-iter 1 1)
  )

(define (fibo n)
  
  )