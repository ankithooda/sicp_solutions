#lang racket
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define tree-1 (list 1 2 3 4))

(define tree-2 (list 1 2 (cons 3 4)))


(define x (list 1 2 3))

(define y (list 4 5 6))

(define lxy (list x y))

(define cxy (cons x y))



  