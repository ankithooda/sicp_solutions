#lang scheme

(define m1 (list (list 1 2 3 4)
                 (list 4 5 6 6)
                 (list 6 7 8 9)))

(define v1 (list 1 2 3 4))

(define v2 (list 4 5 6 6))

(define (accumulate proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (accumulate proc value (cdr sequence)))))

(define (accumulate-n proc init seqs)
  (if (null? (car seqs))
             `()
             (cons (accumulate proc init (map car seqs))
                   (accumulate-n proc init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (matrix-row)
         (dot-product matrix-row v))
       m))

(define (transpose mat)
  (accumulate-n cons `() mat))

 (define (matrix-*-matrix m n) 
   (let ((cols (transpose n))) 
     (map (lambda (v) (matrix-*-vector cols v)) m))) 
  