#lang sicp

(define (make-tree root left right)
  (list root left right))

(define (root tree) (car tree))

(define (left-branch tree) (cadr tree))

  
(define (right-branch tree) (caddr tree))

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons `() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree-list1 tree)
  (if (null? tree)
      nil
      (append (tree-list1 (left-branch tree))
              (cons (root tree)
                    (tree-list1 (right-branch tree))))))

(define a (list 1 3 5 7 9 11))