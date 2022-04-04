#lang sicp

(define (make-account orig-password balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch method)
    (cond ((eq? method `withdraw) withdraw)
          ((eq? method `deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" method))))
  
  (define (auth password method)
    (if (eq? password orig-password)
        (dispatch method)
        (error "Incorrect password")))
  
  auth)