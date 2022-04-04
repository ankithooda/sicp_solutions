#lang racket
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (m-expt-iter b counter product)
  (if (= counter 0)
      product
      (m-expt-iter b (- counter 1) (* b product))))
  
(define (expt-iter b n)
  (m-expt-iter b n 1))

(define (square x) (* x x))

(define (even? x)
  (= (remainder x 2) 0))
  
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (m-fast-expt-iter a b n)
  (writeln a)
  (writeln b)
  (writeln n)
  (writeln "-----------")
  (cond ((= n 0) 1)
        ((= n 1) a)
        ((even? n) (m-fast-expt-iter (* a (square b)) b (/ n 2)))
        (else (m-fast-expt-iter (* a b) b (- n 1)))))
  
      
(define (fast-expt-iter b n)
      (m-fast-expt-iter 1 b n))