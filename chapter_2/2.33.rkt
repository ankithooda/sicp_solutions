#lang scheme
(define (filter predicate sequence)
  (define nil `())
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; This seems to be an incorrect implementation of accumulate
; although it works.
; Tha main problem seem to be that it accumulate call is first and
; then proc'ed value of car sequence is sent in the recursive call.
; This makes it impossible to implement an append function.
; Also in map implementation a call to list is made
(define (accumulate proc value sequence)
  (cond ((null? sequence) value)
        (else (accumulate proc
                           (proc value (car sequence))
                           (cdr sequence)))))

(define (enumerate-interval low high)
  (define nil `())
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
  


(define (even? x)
  (= (remainder x 2) 0))

(define (square x) (* x x))

(define a (list 1 2 3 4 5 6 7))


(define (acc-map proc sequence)
  (accumulate (lambda (x y) (append x (list (proc y))))
              `()
              sequence))

(define (acc-append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (acc-length seq)
  (accumulate (lambda (x y)
                (if (null? y)
                    x
                    (+ x 1)))
              0
              seq))


; New Acc

(define (new-acc proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (new-acc proc value (cdr sequence)))))

(define (new-acc-map proc sequence)
  (new-acc (lambda (x y) (cons (proc x) y))
           `()
           sequence))

(define (new-acc-length seq)
  (new-acc (lambda (x y)
             (if (null? x)
                 0
                 (+ y 1)))
           0
           seq))

(define (new-acc-append seq1 seq2)
  (new-acc cons seq2 seq1))
  

(define b (list 1 2 3))
(define c (list 4 5 6))
