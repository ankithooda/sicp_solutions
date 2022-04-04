#lang scheme
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

(define (reverse-left seq)
  (fold-left (lambda (x y)
                (cons y x))
              `() seq))

(define (reverse-right seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              `() seq))

(define test1 (list 1 2 3 4))