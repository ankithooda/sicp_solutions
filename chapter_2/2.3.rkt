#lang racket

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (length segment)
  (sqrt (+ (square (abs (- (x-point (start-segment segment))
                           (x-point (end-segment segment)))))
           (square (abs (- (y-point (start-segment segment))
                           (y-point (end-segment segment))))))))
  

(define (make-rectangle p1 p2 p3)
  (cons (make-segment p1 p2)
        (make-segment p2 p3)))

(define (rectangle-base rectangle)
  (car rectangle))

(define (rectangle-height rectangle)
  (cdr rectangle))

(define (area rectangle)
  (* (length (rectangle-base rectangle))
     (length (rectangle-height rectangle))))

(define (parameter rectangle)
  (+ (* 2 (length (rectangle-base rectangle)))
     (* 2 (length (rectangle-height rectangle)))))
  
  