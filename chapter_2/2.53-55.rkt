#lang sicp

(list `a `b `c)
; (a b c)

(list (list `george))

;((george))

(cdr `((x1 x2) (y1 y2)))

; ((y1 y2))

(cadr `((x1 x2) (y1 y2)))

; (y1 y2)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)))
        (else (memq item (cdr x)))))

(memq `red `((red shoes) (bluce socks)))

; false

(memq `red `(red blue shoes socks))

;true

(define (list-equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
        ((and (null? l1) (not (null? l2)) false))
        ((and (not (null? l1)) (null? l2) false))
        ((eq? (car l1) (car l2)) (list-equal? (cdr l1) (cdr l2)))
        (else false)))
      
      