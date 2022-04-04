#lang sicp

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))


(define (fold-right proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (fold-right proc value (cdr sequence)))))


(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq)
                                     (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (unique-sum-triplets n s)
  (filter (lambda (triplet)
            (and (= (+ (car triplet)
                       (cadr triplet)
                       (caddr triplet))
                    s)
                 (not (and (= (car triplet)
                         (cadr triplet))
                      (= (car triplet)
                         (caddr triplet))))))
                         
          (flatmap (lambda (pair)
                     (map (lambda (term-3)
                            (append (list term-3) pair))
                          (enumerate 1 n)))
                   (flatmap (lambda (term-2)
                              (map (lambda (term-1)
                                     (list term-1 term-2))
                                   (enumerate 1 n)))
                            (enumerate 1 n)))))
         
                       
(unique-sum-triplets 5 8)