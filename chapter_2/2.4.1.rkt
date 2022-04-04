#lang sicp

(define (square x) (* x x))


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; Generic operations

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular z))
        ((polar? z) (real-part-polar z))
        (else (error "UNKOWN-TYPE -- REAL-TYPE" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular z))
        ((polar? z) (imag-part-polar z))
        (else (error "UNKOWN-TYPE -- IMAG-TYPE" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular z))
        ((polar? z) (magnitude-polar z))
        (else (error "UNKOWN-TYPE -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular z))
        ((polar? z) (angle-polar z))
        (else (error "UNKOWN-TYPE -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang mag ang)
  (make-from-mag-ang-polar mag ang))

; Ben's Representation

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z) (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-type "RECTANGULAR"
               (car x y)))

(define (make-from-mag-ang-rectangular mag ang)
  (attach-type "RECTANGULAR"
               (cons (* mag (cos ang)) (* mag (sin ang)))))

; Alyssa's Representation

(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-type "POLAR"
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))

(define (make-from-mag-ang-polar mag ang)
  (attach-type "POLAR"
               (cons mag ang)))

; Implementing tags

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad Tagged datum -- TYPE_TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad Tagged datum -- CONTENTS" datum)))


(define (rectangular? datum)
  (eq? (type-tag datum) "RECTANGULAR"))

(define (polar? datum)
  (eq? (type-tag datum) "POLAR"))





