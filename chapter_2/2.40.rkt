#lang racket

(require math/number-theory)

(define nil `())

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial seq))

(define (fold-right proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (fold-right proc value (cdr sequence)))))


(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))
    
  
(define (enumerate-n n) (enumerate 1 n))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq)
                                     (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))


  
(define (enumerate-pairs n)
  (fold-right append nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate 1 (- i 1))))
                   (enumerate 1 n))))

(define (prime-pair? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (remove sequence item)
  (filter (lambda (x) (not(= item x)))
          sequence))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove s x))))
               s)))

(define (unique-pairs n)
  (filter (lambda (pair) (not (= (car pair) (cadr pair))))
          (flatmap (lambda (term)
                     (map (lambda (sub-term)
                            (list sub-term term))
                          (enumerate 1 term)))
                   (enumerate 1 n))))

(define (prime-sum-pairs n)
  (map (lambda (p)
         (list (car p) (cadr p) (+ (car p) (cadr p))))
       (filter (lambda (pair)
            (prime? (+ (car pair) (cadr pair))))
          (unique-pairs n))))
         