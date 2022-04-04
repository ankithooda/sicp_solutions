#lang sicp

;;;;; Utils
(define (square n)
  (* n n)
  )

(define (even? n)
  (= 0 (remainder n 2))
  )

;;;;;;;;;;
(define (slow-exp-rec b n)
  (if (= n 0)
      1
      (* b (slow-exp-rec b (- n 1)))
      )
  )

(define (slow-exp-iter b n)
  (define (iter product count)
    (if (= n count)
        product
        (iter (* b product)
              (+ count 1)
              )
        )
    )
  (iter 1 0)
  )

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2)))
              )
        (else (* b (fast-exp b (- n 1)))
              )
       
        )
  )

(define (fast-exp-iter b n)
  (define (iter product count)
    (cond ((= n 0) product)
          ((even? count) (iter (* product (square b)) (/ count 2)))
          (else (iter (* product b) (- count 1)))
          )
    )
  (iter 1 n)
  )