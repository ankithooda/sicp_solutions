#lang sicp

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  
  (define (dispatch m)
    (cond ((eq? m `car) x)
          ((eq? m `cdr) y)
          ((eq? m `set-car!) set-x!)
          ((eq? m `set-cdr!) set-y!)
          (else (error "Undefined Operation -- CONS" m))))
  dispatch)

(define (car cell)
  (cell `car))

(define (cdr cell)
  (cell `cdr))

(define (set-car! cell value)
  ((cell `set-car!) value)
  cell)

(define (Set-cdr! cell value)
  ((cell `set-cdr!) value)
  cell)