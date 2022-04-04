#lang sicp

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define (stream-for-each proc s)
  (if (stream-null? s)
      the-empty-stream
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each show s))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1)
                                              high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter predicate s)
  (cond ((stream-null? s) the-empty-stream)
        ((predicate (stream-car s))
                    (cons-stream
                     (stream-car s)
                     (stream-filter predicate (stream-cdr s))))
        (else (stream-filter predicate (stream-cdr s)))))
            

         