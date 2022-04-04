#lang scheme

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


(define test (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)
                   (list 10 11 12)))