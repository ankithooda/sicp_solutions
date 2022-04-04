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

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
  
(define (sqrt-val x)
  (define (test-tolerance s tolerance)
    (let ((current-guess (stream-car s)))  
      (if (< (abs (- x (square current-guess)))
             0.00001)
          current-guess
          (test-tolerance (stream-cdr s)
                          tolerance))))
  
  (test-tolerance (sqrt-stream x) 0.0001))




 
;;;;;;;;;;;;;;;;;; 3.64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (louis-sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             
                             (sqrt-improve guess x))
                           (louis-sqrt-stream x))))



;;;;;;;;;;;;;;;;;;; 3.64 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-limit s tolerance)
  (let ((first-term (stream-car s))
        (second-term (stream-car (stream-cdr s))))
    (if (< (abs (- first-term second-term))
           tolerance)
        second-term
        (stream-limit (stream-cdr s) tolerance))))


;;;;;;;;;;;;;;;;; 3.65 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integers-from n)
  (cons-stream n
               (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (summands (+ n 1)))))

(define (partial-sums s)
 (add-streams s
              (cons-stream 0
                           (partial-sums s))))

(define (add-streams . args)
  (apply stream-map (cons + args)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define ln2 (accelerated-sequence euler-transform
                                  (partial-sums (summands 1))))


