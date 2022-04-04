#lang sicp


;;;;;;;;;;;;;;;;;;; 3.39 ;;;;;;;;;;;;;;;;;;;;;

; The results will be 100, 101 or 121.

;;;;;;;;;;;;;;;;;;; 3.40 ;;;;;;;;;;;;;;;;;;;;;

; P1 -> P2 : 1000000
; P2 -> P1 : 1000000
; P1r1 -> P2 -> P1r2 -> P1w : 10000
; P1r1 -> P1r2 -> P2 -> P1w : 100
; P2r1 -> P1 -> (rest of P2) : 100000
; P2r1 -> P2r2 -> P1 -> (rest of P2) : 10000
; P2r1 -> P2r2 -> P2r3 -> P1 -> P2w : 1000

; After serialization only 1000000,
; since P2P1 and P1P2 are same (commutative!)


;;;;;;;;;;;;;;;;;;;;;; 3.41 ;;;;;;;;;;;;;;;;;;;;;;;;;

; This is not a problem since reading the balance
; fetches whatever is tha latest value of
; the balance in memory irrespective of
; how many process have read that and are going to
; change it.


;;;;;;;;;;;;;;;;;;;;;; 3.42 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m `withdraw (protected withdraw)))
            ((eq? m `deposit (protected deposit)))
            ((eq? m `balance) balance)
            (else (error "Unknown Operation -- MAKE-ACCOUNT" m))))
    dispatch))



(define (make-account balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected-deposit)))
      (define (dispatch m)
        (cond ((eq? m `withdraw) protected-withdraw)
              ((eq? m `deposit) protected-deposit)
              ((eq? m `balance) balance)
              (else (error "Unknown Operation -- MAKE-ACCOUNT" m))))
      dispatch))

  ; In first implementation, the serialized set will have
  ; a member added whenever there is a call to withdraw
  ; or deposit. Thus if there were 5 deposit and 5 withdraw calls
  ; There are 10 members of the serialized set and none of them
  ; can run at the same time.

  ; In the second implementation, the serialized set will have
  ; only two members, one for deposit and one for withdraw
  ; Therefore the same protected procedure can run with two
  ; sperate processes just like a non protected procedure
  ; 
  ; It only prevents deposit and withdraw running at the same time,
  ; but two deposits and two withdraws can run at the same time
  ; interfering with each other.

  ; In both cases each account has its' serialized set.


;;;;;;;;;;;;;;;;;;;;;;;;; 3.43 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; In case of proper serialization across two accounts
  ; Amounts are always some order of 10, 20, 30. Since all
  ; exchanges are sequential that means that
  ; deposits from a and withdrawl into b always run one after
  ; another without any writes from any other exchange
  ; This way amounts will always be consistent.


  ; In case of serialization across only one account, sums will be
  ; be preserved

  ; Consider the following amounts for a, b , c
  ; 10, 20, 30

  ; We have two concurrent exchanges, a-b and a-c

  ; During execution of a-b, process reads value of a as 10
  ; and value of b as 20.

  ; But before it can update the amounts
  ; a-c executes completeley, so the new amount becomes
  ; 30, 20, 10

  ; After which a-b continues
  ; Procedure will add 10 to a and subtract 10 from b
  ; 40, 10, 10

  ; The sum is still 60, this happends because during writes we
  ; we always add and subtract the same amount, there can
  ; no intermediate values in a single account's deposit and withdraw
  ; as they are serialized.
  ; Even though the difference calculated is incorrect, the sum remains
  ; the same as the "incorrect difference" is atomically added and
  ; subtracted

  ; If even single account serialization is not available then even the
  ; sum wont conserver


;;;;;;;;;;;;;;;;;;;;;;; 3.44 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; This should not be a problem as each transactions operate
  ; only one one account at a time, so if an account's deposit
  ; and withdrawl are serialized then we can implement transfer
  ; this way without implementing any sophisticated mechanism.


;;;;;;;;;;;;;;;;;;;;;;; 3.45 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; serialized-exchange calls the deposit and withdraw, however
  ; in Louis's implementation now calls to deposit and withdraw
  ; returns their serialized versions, therefore deposit and withdraw
  ; can not be serialized again with same serializer as it will
  ; leads to forver waiting.

  




