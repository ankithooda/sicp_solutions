#lang sicp

(define empty-set `())

(define (make-set . args) args)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (union set1 set2)
  (append set1 set2))

(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) empty-set)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))