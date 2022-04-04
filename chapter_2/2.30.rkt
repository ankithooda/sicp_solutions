#lang scheme
(define (scale-tree tree scale-factor)
  (define nil `())
  (define (leaf? x) (not (pair? x)))
  
  (cond ((null? tree) nil)
        ((leaf? tree) (* tree scale-factor))
        (else (cons (scale-tree (car tree) scale-factor)
                    (scale-tree (cdr tree) scale-factor)))))


(define (scale-tree-map tree scale-factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree scale-factor)
             (* scale-factor sub-tree)))
       tree))


(define (square-tree tree)
  (define nil `())
  (define (leaf? x) (not (pair? x)))

  (cond ((null? tree) nil)
        ((leaf? tree) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)

  (define (leaf? x) (not (pair? x)))
  
  (map (lambda (sub-tree)
         (if (leaf? sub-tree)
             (* sub-tree sub-tree)
             (square-tree sub-tree)))
       tree))


  
  

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))


