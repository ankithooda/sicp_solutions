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


(define (decode-old bits tree)
  (define (select-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- select-branch" bit))))
  
  (define (partial-decode remaining-bits current-branch)
    (cond ((null? remaining-bits) nil)
          ((leaf? tree)
           (cons (symbol-leaf current-branch)
                 (partial-decode (cdr remaining-bits) tree)))
          (else (partial-decode (cdr remaining-bits)
                                (select-branch (car remaining-bits) current-branch)))))
  (partial-decode bits tree))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- select-branch" bit))))
  
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

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







