#lang sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define empty-set `())

(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union (cdr set1) set2))
        (else (union (cdr set1) (cons (car set1) set2)))))

(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) empty-set)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

(define (make-set . args)
  (define (construct-set items partial-set)
    (if (null? items)
        partial-set
        (construct-set (cdr items) (adjoin-set (car items) partial-set))))
  (construct-set args empty-set))
  
