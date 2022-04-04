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


