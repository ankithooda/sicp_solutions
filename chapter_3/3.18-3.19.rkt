#lang sicp

(define (has-cycle-slow? list-struct)
  (define temp-struct `())

  (define (traverse list-struct)
    (cond ((null? list-struct) false)
          ((memq (car list-struct) temp-struct) true)
          (else (begin
                  (set! temp-struct (cons (car list-struct) temp-struct))
                  (traverse (cdr list-struct))))))

  (traverse list-struct))


(define x3 (list 1 2 3 4 5 6 7 7 8 9))
(define pair-inf x3)
(set-cdr! (cdr pair-inf) pair-inf)


(define (has-cycle? list-struct)

  (define (jump-2 current)
    (if (and (not (null? current))
             (not (null? (cdr current))))
        (cdr (cdr current))
        nil))
      
  (define (traverse slow-pointer fast-pointer)
    (cond ((or (null? fast-pointer)
              (null? slow-pointer)) false)
          ((eq? slow-pointer fast-pointer) true)
          (else (traverse
                 (cdr slow-pointer)
                 (jump-2 fast-pointer)))))

  (traverse list-struct
            (jump-2 list-struct)))

      
      
 