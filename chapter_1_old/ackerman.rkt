#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; inc and dec
(define (inc x)
  (+ x 1)
  )

(define (dec x)
  (- x 1)
  )

; Ackerman Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* y 2))
        ((= y 1) 2)
        (else (A
               (- x 1)
               (A x (- y 1))
               ))
        )
  )

; Wrappers over ackerman
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

; Concise versions of wrappers over ackerman pertaining to 1.10
(define (conc-f n)
  (* 2 n)
  )

(define (conc-g n)
  (if (= n 0)
      0
       (expt 2 n)
      )
  )


; Test util to compare outputs of concise and not so concise versions of wrappers over ackerman
(define (test conc-def def limit)
  (define (iter success failure i n)
    (cond ((> i n) (begin (display success)
                          (newline)
                          (display failure)
                          (newline)
                          success))
          ((= (conc-def i) (def i)) (iter (inc success)
                                          failure
                                          (inc i)
                                          n
                                          )
                                    )
          (else (iter success
                      (inc failure)
                      (inc i)
                      n
                      )
                )
          )
    )
  (if (= (iter 0 0 0 limit) (+ limit 1))
      (display "All test case pass")
      (display "Failure")
      )
  )