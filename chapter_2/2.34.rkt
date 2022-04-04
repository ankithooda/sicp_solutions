#lang scheme

(define (accumulate proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (accumulate proc value (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define l1 (list 1 3 0 5 0 1))