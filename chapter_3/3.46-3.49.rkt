#lang sicp
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex `acquire)
        (let ((value (apply p args)))
          (mutex `release)
          value))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m `acquire)
             (if (test-and-set! cell)
                 (the-mutex `acquire)))
            ((eq? m `release)
             (set-car! cell false))))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;;;;;;;;;;;;;;;; 3.47 ;;;;;;;;;;;;;;;;;;;;

(define (semaphore n)
  (let ((lock-count 0)
        (max n))
    (define (the-semaphore m)
      (cond ((eq? m `acquire)
             ; If test-and-increment is not successful
             ; try again
             (if (not (test-and-increment! lockcount max))
                 (the-semaphore `acquire)))
            
            ((eq? m `release)
             (decrement! lockcount))))
    the-semaphore))

(define (test-and-increment! count)
  (if (< count max)
      (begin
       (set! count (inc count))
       true)
      false))

(define (decrement! count)
  (set! count (dec count)))



;;;;;;;;;;;;;;;;;;;;;;; 3.48 ;;;;;;;;;;;;;;;;;;;;;;;;;;

; A simple deadlock situation
; Process A needs two resources a, b
; Process B also needs to resources a, b

; A acquires lock for a, B acquires lock for b.
; Now both will deadlock as they will wait for the other
; to release the lock.

; If each resource is given a number then
; can occur when a process has higher resource number
; and is waiting for a lower resource number.

; If each process takes locks increaing order
; then deadlock can not occur.

; Only applicable when all required resources are known before
; hand.

;;;;;;;;;;;;;;;;;;;;;; 3.49 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  
            
             
      