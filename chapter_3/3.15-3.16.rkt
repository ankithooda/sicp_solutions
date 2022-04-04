#lang sicp


(define x (list `a `b))
(define z1 (cons x x))
(define z2 (cons (list `a `b) (list `a `b)))

(define (set-to-wow! x)
  (set-car! (car x) `wow)
  x)

(define z3 (list 1 2 3 4))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


; count-pairs returns 3, 4, 7, Inf with 3 pairs


(define pair-3 (list 1 2 3))

(define x1 (cons 1 1))

(define pair-4 (list x1 x1))

(define x2 '(foo)) 
(define y2 (cons x2 x2)) 
(define pair-7 (cons y2 y2)) 

(define x3 (list 1 2))
(define pair-inf x3)
(set-cdr! (cdr pair-inf) pair-inf)
 