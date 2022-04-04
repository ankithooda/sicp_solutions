#lang sicp

(define nil `())

(define (make-tree tree left right) (list tree left right))

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (root set)) true)
        ((< x (root set)) (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (root set)) set)
        ((< x (root set)) (make-tree
                           (root set)
                            (adjoin-set x (left-branch set))
                            (right-branch set)))
        ((> x (root set)) (make-tree
                           (root set)
                           (left-branch set)
                           (adjoin-set x (right-branch set))))))


(define (tree-list1 tree)
  (if (null? tree)
      nil
      (append (tree-list1 (left-branch tree))
              (cons (root tree)
                    (tree-list1 (right-branch tree))))))

(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) 