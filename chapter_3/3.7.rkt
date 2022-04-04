#lang sicp
(define (make-account orig-password balance)
  (let ((incorrect-password-count 0)
        (user-passwords (list orig-password)))
    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)))
  
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (add-user password)
      (set! user-passwords (cons password user-passwords)))
      

    (define (dispatch method)
      (cond ((eq? method `withdraw) withdraw)
            ((eq? method `deposit) deposit)
            ((eq? method `add-user) add-user)
            (else (error "Unknown request -- MAKE-ACCOUNT" method))))

    (define (show-auth-failure-message)
      (if (> incorrect-password-count 7)
          (error "Incorrect password,too many auth failures, this will be reported.")
          (error "Incorrect password")))

    (define (password-correct? supplied-password)
      
      (define (iter password-list)
        (cond ((null? password-list) false)
              ((eq? supplied-password (car password-list)) true)
              (else (iter (cdr password-list)))))

      (iter user-passwords))
        
    (define (auth supplied-password method)
      (if (password-correct? supplied-password)
          (begin (set! incorrect-password-count 0)
                 (dispatch method))
          (begin (set! incorrect-password-count
                       (+ incorrect-password-count 1))
                 (show-auth-failure-message))))
    auth))

(define (make-joint-acc account first-holder-password second-holder-password)
  ((account first-holder-password `add-user) second-holder-password)
  account)
  