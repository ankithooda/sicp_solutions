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

(define (stream-filter predicate s)
  (cond ((stream-null? s) the-empty-stream)
        ((predicate (stream-car s))
                    (cons-stream
                     (stream-car s)
                     (stream-filter predicate (stream-cdr s))))
        (else (stream-filter predicate (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (integers-from n)
  (cons-stream n
               (integers-from (+ n 1))))

(define integers (integers-from 1))

(define no-sevens
  (stream-filter (lambda (x)
                   (not (= (remainder x 7)
                           0)))
                 integers))


(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (divisible? a b)
  (= (remainder a b) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-from 2)))

(define ones (cons-stream 1 ones))

(define (stream-map proc . stream-list)
  (if (stream-null? (car stream-list))
      the-empty-stream                  
      (cons-stream
       (apply proc (map stream-car stream-list))
       (apply stream-map
              (cons proc (map stream-cdr stream-list))))))

(define (add-streams . args)
  (apply stream-map (cons + args)))

(define ints (cons-stream 1
                          (add-streams ones integers)))


(define crazy-fib
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr crazy-fib)
                                         crazy-fib))))

;;;;;;;;;;;;;;;;;; 3.53 ;;;;;;;;;;;;;;;;;;;;;;

(define s (cons-stream 1
                       (add-streams s s)))

;;;;;;;;;;;;; Alt Primes ;;;;;;;;;;;;;;;;;;;;;

(define alt-primes
  (cons-stream
   2
   (stream-filter prime? (integers-from 3))))

(define (square x) (* x x))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter alt-primes))


                                                     
         

            
         
