#lang sicp

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;(define x (list (list `a `b) `c `d))
;(define y (list `e `f))

(define (append! x y)
  (set-cdr! (last-pair x) y))

(define x (list `a `b))
(define y (list `c `d))

(define z (append x y))

(define w (append! x y))

(cdr x)
; (b c d)