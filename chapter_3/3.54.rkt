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

(define (display-stream-until s n)
  (if (= n 0)
      (display-line (stream-car s))
      (begin
        (display-line (stream-car s))
        (display-stream-until (stream-cdr s) (- n 1)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (stream-map proc . stream-list)
  (if (stream-null? (car stream-list))
      the-empty-stream                  
      (cons-stream
       (apply proc (map stream-car stream-list))
       (apply stream-map
              (cons proc (map stream-cdr stream-list))))))

(define (add-streams . args)
  (apply stream-map (cons + args)))

;;;;;;;;;;;;;;;;; 3.54 ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mul-streams . args)
  (apply stream-map (cons * args)))


(define ones (cons-stream 1 ones))

(define crazy-ints
  (cons-stream 1
               (add-streams crazy-ints
                            ones)))

(define factorials
  (cons-stream 1
               (mul-streams crazy-ints
                            factorials)))


;;;;;;;;;;;;; 3.55 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (partial-sums s)
 (add-streams s
              (cons-stream 0
                           (partial-sums s))))


;;;;;;;;;;;;;;; 3.56 ;;;;;;;;;;;;;;;;;;;;;;


(define (scale-stream stream factor)
  (stream-map (lambda (x)
                (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      s2)))
                 ((> s1car s2car)
                  (cons-stream s2car
                               (merge s1
                                      (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1
                       (merge (scale-stream S 2)
                              (merge (scale-stream S 3)
                                     (scale-stream S 5)))))



;;;;;;;;;;;;;;;;; 3.58 ;;;;;;;;;;;;;;;;;;;;;;;

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;;;;;;;;;;;;;;;;;;;;; 3.59 ;;;;;;;;;;;;;;;;;;;

(define (div-streams . args)
  (apply stream-map (cons / args)))


(define (integrate-series s)
  (cons-stream 1
               (div-streams s crazy-ints)))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))






      



