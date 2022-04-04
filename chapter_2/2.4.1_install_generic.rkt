#lang sicp

(define (square x) (* x x))

; PUT AND GET STARTS;
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; PUT AND GET END ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)

  ;private data/code
  
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang mag ang)
    (cons (* mag (cos ang)) (* mag (sin ang))))

  ; interface
  (define (tag x) (attach-tag `rectangular x))

  (put `real-part `rectangular real-part)
  (put `imag-part `rectangular imag-part)
  (put `magnitude `rectangular magnitude)
  (put `angle `rectangular angle)
  (put `make-from-real-imag `rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put `make-from-mag-ang `rectangular
       (lambda (mag ang) (tag (make-from-mag-ang mag ang))))
  `done)

(define (install-polar-package)

  ;private data/code

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  
  (define (make-from-mag-ang mag ang) (cons mag ang))
  
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
           
  ; interface
  (define (tag x) (attach-tag `polar x))

  (put `real-part `polar real-part)
  (put `imag-part `polar imag-part)
  (put `magnitude `polar magnitude)
  (put `angle `polar angle)
  (put `make-from-real-imag `polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put `make-from-mag-ang `polar
       (lambda (mag ang) (tag (make-from-mag-ang mag ang))))
  `done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op (car type-tags))))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))


; Generic Functions

(define (real-part z) (apply-generic `real-part z))
(define (imag-part z) (apply-generic `imag-part z))
(define (magnitude z) (apply-generic `magnitude z))
(define (angle z) (apply-generic `angle z))

(define (make-from-real-imag x y)
  ((get `make-from-real-imag `rectangular) x y))

(define (make-from-mag-ang mag ang)
  ((get `make-from-mag-ang `polar) mag ang))

; Complex Arithmetic

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

(install-rectangular-package)
(install-polar-package)