#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Util Functions
(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq)
                                     (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (fold-right proc value sequence)
  (if (null? sequence)
      value
      (proc (car sequence)
            (fold-right proc value (cdr sequence)))))

(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

(define nil `())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (col pos) (car pos))
(define (row pos) (row pos))

(define (enumerate-all-positions k)
  (flatmap (lambda (term1)
             (flatmap (lambda (term2)
                    (if (= term1 term2)
                        (list (list term1 term2))
                        (list (list term1 term2) (list term2 term1))))
                  (enumerate 1 term1)))
           (enumerate 1 k)))

; Assuming k-1 queens has been placed
; This automatically eliminates occupied rows and columns for the
; already placed k-1 queens
(define (candidate-positions k)
  (filter (lambda (pos)
            (or (= (car pos) k)
                (= (cadr pos) k)))
          (enumerate-all-positions k)))

; This function returns true if pos1 and pos2 are on different
; diagonals on a board of size 
(define (generate-diagonal-positions pos s)

  (define dec (lambda (x) (- x 1)))
  (define inc (lambda (x) (+ x 1)))
  
  (define (iter diagonal-positions col-op row-op pos s)
    (let ((new-pos (list (col-op (col pos))
                         (row-op (row pos)))))
      (if (or (< (col new-pos) 1)
              (< (row new-pos) 1)
              (> (col new-pos) s)
              (> (col new-pos) s))
          diagonal-positions
          (iter (cons new-pos diagonal-positions)
                col-op
                row-op
                new-pos
                s))))
  (flatmap (lambda (ops)
             (iter `() (car ops) (cadr ops) pos s))
           (list (list dec inc)
                 (list inc dec)
                 (list inc inc)
                 (list dec dec))))
  
  
;
;(define (safe? k positions)
;  (define (row-col-safe? k possible-positions
  


(define empty-board nil)


