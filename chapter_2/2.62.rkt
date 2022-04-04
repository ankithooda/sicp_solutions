#lang sicp

(define (union set-1 set-2)
  (define (merge set1 set2 merged-set)
    (cond ((null? set1) (append (reverse merged-set) set2))
          ((null? set2) (append (reverse merged-set) set1))
          ((= (car set1) (car set2))
           (merge (cdr set1) (cdr set2) (cons (car set1) merged-set)))
          ((< (car set1) (car set2))
           (merge (cdr set1) set2 (cons (car set1) merged-set)))
          (else (merge set1 (cdr set2) (cons (car set2) merged-set)))))
  (merge set-1 set-2 `()))

(define (union-set set1 set2) 
   (cond  ((null? set1) set2) 
          ((null? set2) set1) 
          ((= (car set1) (car set2))  
           (cons (car set1) (union-set (cdr set1) (cdr set2)))) 
          ((< (car set1) (car set2))   
           (cons (car set1) (union-set (cdr set1) set2))) 
          (else  
           (cons (car set2) (union-set set1 (cdr set2)))))) 