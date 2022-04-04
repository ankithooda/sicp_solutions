#lang sicp


(define (stream-car s) (car s))

(define (stream-cdr s) (delay (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      the-empty-stream
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-map-gen proc argstream)
  (if (stream-null? (car argstream))
      the-empty-stream
      (cons-stream
       (apply proc (map (stream-car argstream)))
       (apply stream-map
              (cons proc (map stream-cdr argstream))))))
   