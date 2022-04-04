#lang sicp
(#%require sicp-pict)
;(paint einstein)

(define ein einstein)
(define zorro mark-of-zorro)

(define (identity x) x)

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; split ;

(define (r-split painter n)
  (if (= n 1)
      painter
      (let ((smaller (r-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (u-split painter n)
  (if (= n 1)
      painter
      (let ((smaller (u-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (split orig-placer split-placer)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (orig-placer painter (split-placer smaller smaller)))))
  rec)

(define uu-split (split below beside))

(define rr-split (split beside below))
         
