#lang sicp
(define (make-account orig-password balance)
  (let ((incorrect-password-count 0))
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

    (define (show-auth-failure-message)
      (if (> incorrect-password-count 7)
          (error "Incorrect password,too many auth failures, this will be reported.")
          (error "Incorrect password")))
    
    (define (auth password method)
      (if (eq? password orig-password)
          (begin (set! incorrect-password-count 0)
                 (dispatch method))
          (begin (set! incorrect-password-count
                       (+ incorrect-password-count 1))
                 (show-auth-failure-message))))
    auth))
