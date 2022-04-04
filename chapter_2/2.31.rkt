#lang scheme

(define (square x) (* x x))

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map proc tree)
  (define nil `())
  (define (leaf? x) (not (pair? x)))
  
  (cond ((null? tree) nil)
        ((leaf? tree) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))