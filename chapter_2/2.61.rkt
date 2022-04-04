#lang sicp

(define empty-set `())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((> x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection set1 set2)
  (if (or (null? set1) (null? set2))
      empty-set
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection (cdr set1) set2))
              ((> x1 x2)
               (intersection set1 (cdr set2)))))))

(define (adjoin-set x set)
  (define (construct-set lower-set upper-set)
    (cond ((null? upper-set) (append (reverse lower-set)
                                     (cons x upper-set)))
          ((> x (car upper-set)) (construct-set (cons (car upper-set) lower-set)
                                                (cdr upper-set)))
                       
          (else (append (reverse lower-set)
                        (cons x upper-set)))))
  (construct-set empty-set set))

(define (adjoin-set-rec x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set-rec x (cdr set))))))
      
              
             
        
        