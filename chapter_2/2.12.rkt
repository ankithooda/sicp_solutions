#lang racket

; Util Function
(define (get-value base percent)
  (/ (* base percent) 100))

(define (spans-zero? interval)
  (and (> 0 (lower-bound interval))
       (< 0 (upper-bound interval))))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display " , ")
  (display (upper-bound x))
  (display "]"))

; make-interval constructor
(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

; center constructor

(define (make-center-interval center tolerance)
  (make-interval (- center (get-value center tolerance))
                 (+ center (get-value center tolerance))))

(define (center interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2))

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))
        
(define (percent interval)
  (/ (* (width interval) 100) (center interval)))


; Interval Arithmetic
   
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
 
(define (div-interval x y)
  (if (spans-zero? y)
      (error "Division not defined for interval that span zero.")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


  