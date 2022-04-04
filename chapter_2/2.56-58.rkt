#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (symbol? x)
       (symbol? y)
       (eq? x y)))

(define (=number? x num)
  (and (number? x) (= x num)))

(define (make-sum s1 s2)
  (cond ((=number? s1 0) s2)
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (+ s1 s2))
        (else (list `+ s1 s2))))

(define (make-product s1 s2)
  (cond ((or (=number? s1 0) (=number? s2 0)) 0)
        ((=number? s1 1) s2)
        ((=number? s2 1) s1)
        ((and (number? s1) (number? s2)) (* s1 s2))
        (else (list `* s1 s2))))

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list `** base exponent))))

(define (sum? s)
  (and (pair? s)
       (eq? (car s) `+)))

(define (product? p)
  (and (pair? p)
       (eq? (car p) `*)))

(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) `**)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
       
(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))


; deriv function
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else error "Unknwon Expression" exp)))



