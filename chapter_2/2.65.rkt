#lang sicp

(define nil `())

(define (make-tree root left right)
  (list root left right))

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

  
(define (right-branch tree) (caddr tree))

(define tree1
  (make-tree 4
             (make-tree 2
                        (make-tree 1 nil nil)
                        (make-tree 3 nil nil))
             (make-tree 6
                        (make-tree 5 nil nil)
                        (make-tree 8 nil nil))))

(define tree2
  (make-tree 5
             (make-tree 3
                        nil
                        (make-tree 4 nil nil))
             (make-tree 8
                        nil
                        nil)))


  
(define (intersection set1 set2)
  