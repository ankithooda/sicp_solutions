#lang sicp
(#%require sicp-pict)
;(paint einstein)

(define ein einstein)

(define ein2 (beside ein (flip-vert ein)))

(define ein4 (below ein2 ein2))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (right-trail painter n)
  (if (= n 0)
      painter
      (beside painter (right-trail painter (- n 1)))))

(define (all-corner-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (all-corner-split painter (- n 1))))
        (below (beside smaller smaller)
               (beside smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))