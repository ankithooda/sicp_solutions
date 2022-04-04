#lang sicp

(define (count-pairs x)
  (define already-seen `())

  (define (count list-struct)
    (if (or (not (pair? list-struct))
            (memq list-struct already-seen))
        0
        (begin
          (set! already-seen (cons list-struct already-seen))
          (+ (count (car list-struct))
             (count (cdr list-struct))
             ))))

  (count x))


(define x3 (list 1 2))
(define pair-inf x3)
(set-cdr! (cdr pair-inf) pair-inf)

(define x1 (cons 1 1))

(define pair-4 (list x1 x1))

(define x2 '(foo)) 
(define y2 (cons x2 x2)) 
(define pair-7 (cons y2 y2)) 