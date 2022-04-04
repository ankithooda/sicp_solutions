#lang sicp

(define balance 100)
(define (peter) (set! balance (+ balance 10)))
(define (paul) (set! balance (+ balance 20)))
(define (mary) (set! balance (+ balance (/ balance 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;3.38.a ;;;;;;;;;;;;;;;;;;;;;;;;;;;

; There can be 6 possible answers as these 3 procedures can
; run in 6 ways

; peter, paul, mary - 45
; peter, mary paul  - 35
; paul, peter, mary - 45
; paul, mary, peter - 50
; mary, peter, paul - 40
; mary, paul, peter - 40

;;;;;;;;;;;;;;;;;;;;;;;;;3.38.b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Each function consists of two instructions
; 1 Reading the balance
; 2 Setting the balance

; Pe1 Pe2
; Pa1 Pa2
; Ma1 Ma2

;;;;;;;;;;;;;;;;; Possible Orders ;;;;;;;;;;;;;;;;;

; Pe1 Pe2 Pa1 Pa2 Ma1 Ma2 - 45
; Pe1 Pe2 Pa1 Ma1 Pa2 Ma2 - 55
; Pe1 Pe2 Pa1 Ma1 Ma2 Pa2 - 90

; Pe1 Pe2 Ma1 Ma2 Pa1 Pa2 - 35
; Pe1 Pe2 Ma1 Pa1 Ma2 Pa2 - 90
; Pe1 Pe2 Ma1 Pa1 Pa2 Ma2 - 55















