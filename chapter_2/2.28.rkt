#lang scheme
(define (fringe tree)
  (define nil `())
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (car (cdr tree)))))))
      
 (define (fringe-s tree) 
   (define nil '()) 
   (if (null? tree)  
       nil 
       (let ((first (car tree))) 
         (if (not (pair? first)) 
             (cons first (fringe-s (cdr tree))) 
             (append (fringe-s first) (fringe-s (cdr tree))))))) 

(define x (list (list 1 2) (list 3 4)))

(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8)))) 

(define test-tree (list 10 (list 20 (list 30 40) 50) (list 60 70)))
          