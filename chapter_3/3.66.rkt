#lang sicp

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)


(define (display-stream s)
  (stream-for-each show s))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (square x) (* x x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      the-empty-stream
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc . stream-list)
  (if (stream-null? (car stream-list))
      the-empty-stream                  
      (cons-stream
       (apply proc (map stream-car stream-list))
       (apply stream-map
              (cons proc (map stream-cdr stream-list))))))

(define (stream-till s n)
  (display-line (stream-car s))
  (if (> n 0)
      (stream-till (stream-cdr s) (- n 1))
      `done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integers-from n)
  (cons-stream n
               (integers-from (+ n 1))))


(define integers (integers-from 1))

(define (pairs S T)
  (cons-stream
   (list (stream-car S) (stream-car T))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car S) x))
                (stream-cdr T))
    (pairs (stream-cdr S)
           (stream-cdr T)))))

(define (stream-append s t)
  (if (stream-null? s)
      t
      (cons-stream (stream-car s)
                   (stream-append (stream-cdr s)
                                  t))))

(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (cons-stream
       (stream-car s1)
       (interleave s2
                   (stream-cdr s1)))))
                                  
(define (stream-filter predicate s)
  (cond ((stream-null? s) the-empty-stream)
        ((predicate (stream-car s))
                    (cons-stream
                     (stream-car s)
                     (stream-filter predicate (stream-cdr s))))
        (else (stream-filter predicate (stream-cdr s)))))  

;;;;;;;;;;;;;;;;;;;;;; 3.67 ;;;;;;;;;;;;;;;;;;;;;;


(define (all-pairs S T)
  (cons-stream
   (list (stream-car S) (stream-car T))
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car S) x))
                 (stream-cdr T))
     (stream-map (lambda (x)
                   (list x (stream-car T)))
                 (stream-cdr S)))
    (all-pairs (stream-cdr S) (stream-cdr T)))))
    
                
;;;;;;;;;;;;;;;;;;;;; 3.68 ;;;;;;;;;;;;;;;;;;;;;;

(define (louis-pairs S T)
  (interleave
   (stream-map (lambda (x)
                 (list (stream-car S) x))
               T)
   (louis-pairs (stream-cdr S) (stream-cdr T))))


;;;;;;;;;;;;;;;;;;; 3.69 ;;;;;;;;;;;;;;;;;;;;;;;;

(define (constant-stream x)
  (cons-stream x (constant-stream x)))

(define (triples S T U)
  (cons-stream
   (list (stream-car S)
         (stream-car T)
         (stream-car U))
   (interleave (stream-map (lambda (x)
                             (cons (stream-car U) x))
                           (stream-map (lambda (y)
                                         (list (stream-car S)
                                               y))
                                       (stream-cdr T)))
               (triples (stream-cdr S)
                        (stream-cdr T)
                        (stream-cdr U)))))
                                       
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (p-triplets t) (stream-filter (lambda (trip)
                                    (= (+ (square (car trip))
                                          (square (cadr trip)))
                                       (square (caddr trip))))
                                  t))       
    
                   






        
       
      
 