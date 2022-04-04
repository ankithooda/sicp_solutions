#lang racket
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))



(define (meta-f x y z count n)
  (if (= count n)
      z
      (meta-f y z (+ (* 3 x)
                     (* 2 y)
                     (* 1 z)
                     )
              (+ 1 count)
              n
  )))


(define (f-iter n)
  (if (< n 3)
      n
      (meta-f 0 1 2 2 n)))