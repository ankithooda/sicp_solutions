#lang scheme

(define nil `())

(define (subsets set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets (cdr set))))
        (append rest (map
                      (lambda (x) (cons (car set) x))
                      rest)))))
      

(define s (list 1 2 3))

    
  