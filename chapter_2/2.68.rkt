#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-leaf symbol weight)
  (list `leaf symbol weight))

(define (leaf? object)
  (eq? `leaf (car object)))

(define (symbol-leaf o)
  (cadr o))

(define (weight-leaf o)
  (caddr o))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define sample-tree
  (make-code-tree (make-leaf `A 4)
                  (make-code-tree
                   (make-leaf `B 2)
                   (make-code-tree (make-leaf `D 1)
                                   (make-leaf `C 1)))))

(define sample-message (list 0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message)
                             (encode (cdr message) tree)))))




      
  