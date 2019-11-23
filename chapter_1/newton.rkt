#lang sicp
(define (sqrt x)

  ; square
  (define (square x)
    (* x x)
    )
  
  ; Core logic
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))
        )
    )

  ; Impl of good enough measure
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001)
    )

  ; Improve the guess
  (define (improve guess)
    (average guess (/ x guess))
    )

  (define (average x y)
    (/ (+ x y) 2)
    )

  ; Call to sqrt-iter with initial guess value of 1.0
  (sqrt-iter 1.0)
  )


