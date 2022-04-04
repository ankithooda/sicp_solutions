#lang racket
(define (list-ref items n)
  (cond ((null? items) (error "Index out of bounds"))
        ((= n 0) (car items))
        (else (list-ref (cdr items) (- n 1)))))

(define (length items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ count 1))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define a (list 1 2 3 4))

(define b (list 5 6 7 8))

(define (last-pair items)
  (define (iter last a)
    (if (null? a)
        last
        (iter (car a) (cdr a))))
  (iter (car a) (cdr a)))

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))