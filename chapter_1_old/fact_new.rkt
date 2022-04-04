#lang racket
(define (fact-rec n)
  (if (= n 1)
      1
      (* n (fact-rec (- n 1)))
      )
  )

(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))
        )
    )
  (iter 1 1)
  )

;;;;;;


(define (sum1 a b)
  (if (= a 0)
      b
      (inc (sum1 (dec a) b))
      )
  )

(define (sum2 a b)
  (if (= a 0)
      b
      (sum2 (dec a) (inc b))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* y 2))
        ((= y 1) 2)
        (else (A
               (- x 1)
               (A x (- y 1))
               ))
        )
  )

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(define (conc-f n)
  (* 2 n)
  )
(define (test conc-def def)
  (define (iter success failure i n)
    (cond ((> i n) (begin (display success)
                          (display failure)
                          ))
          ((= (conc-def i) (def i)) (iter (inc success)
                                          failure
                                          (inc i)
                                          n
                                          )
                                    )
          (else (iter success
                      (inc failure)
                      (inc i)
                      n
                      )
                )
          )
    )
  (iter 0 0 0 1000)
  )
