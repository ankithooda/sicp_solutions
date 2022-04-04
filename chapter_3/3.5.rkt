#lang sicp

(define (square x) (* x x))

; Point Implementation
(define (make-point x y)
  (cons x y))

(define (x-coord p) (car p))
(define (y-coord p) (cdr p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Circle Implementation
(define (make-circle center radius)
  (cons center radius))

(define (center circle) (car circle))
(define (radius circle) (cdr circle))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Rectangle Implementation
(define (make-rectangle diag-start diag-end)
  (cons diag-start diag-end))

(define (diag-start rectangle) (car rectangle))
(define (diag-end rectangle) (cdr rectangle))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define circle (make-circle (make-point 0.0 0.0)
                            1.0))


  
  
(define (get-encapsulating-rectangle circle)
  (make-rectangle (make-point (- (x-coord (center circle))
                                 (radius circle))
                              (- (y-coord (center circle))
                                 (radius circle)))
                  (make-point (+ (x-coord (center circle))
                                 (radius circle))
                              (+ (y-coord (center circle))
                                 (radius circle)))))

                  
(define encap-rectangle (get-encapsulating-rectangle circle))                                 


                                          

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (get-random-point-rect rectangle)
  (make-point (random-in-range (x-coord (diag-start rectangle))
                               (x-coord (diag-end rectangle)))
              (random-in-range (y-coord (diag-start rectangle))
                               (y-coord (diag-end rectangle)))))
  
(define (within-circle? circle point)
  (and (<= (+ (square (- (x-coord point) (x-coord (center circle))))
              (square (- (y-coord point) (y-coord (center circle)))))
           (square (radius circle)))))

(define (estimate-pi trials)
  (* (monte-carlo trials check-circle) 4.0))

(define (check-circle)
  (within-circle? circle (get-random-point-rect encap-rectangle)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))