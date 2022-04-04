#lang sicp

; Define dcell

(define (make-dcell value)
  (cons value (cons `() `())))

(define (get-value dcell)
  (car dcell))

(define (pointer-pair dcell)
  (cdr dcell))

(define (get-next dcell)
  (car (pointer-pair dcell)))

(define (get-prev dcell)
  (cdr (pointer-pair dcell)))

(define (set-next! dcell item)
  (set-car! (pointer-pair dcell) item))

(define (set-prev! dcell item)
  (set-cdr! (pointer-pair dcell) item))


; make-dqueue and selectors

(define (make-dqueue)
  (cons `() `()))

(define (empty? dqueue)
  (or (null? (front-ptr dqueue))
      (null? (rear-ptr dqueue))))

(define (front-ptr dqueue)
  (car dqueue))

(define (rear-ptr dqueue)
  (cdr dqueue))

(define (set-front-ptr! dqueue item)
  (set-car! dqueue item))

(define (set-rear-ptr! dqueue item)
  (set-cdr! dqueue item))

(define (insert-in-empty! dqueue item)
  (let ((new-dcell (make-dcell item)))
    (begin
      (set-front-ptr! dqueue new-dcell)
      (set-rear-ptr! dqueue new-dcell))))

(define (front-insert-dqueue! dqueue item)
  (if (empty? dqueue)
      (insert-in-empty! dqueue item)
      (let ((new-dcell (make-dcell item)))
         ; assign next and prev
         (set-next! new-dcell (front-ptr dqueue))
         (set-prev! (front-ptr dqueue) new-dcell)

         ; update front ptr
         (set-front-ptr! dqueue new-dcell))))

(define (rear-insert-dqueue! dqueue item)
  (if (empty? dqueue)
      (insert-in-empty! dqueue item)
      (let ((new-dcell (make-dcell item)))
         ; assign next and prev
         (set-prev! new-dcell (rear-ptr dqueue))
         (set-next! (rear-ptr dqueue) new-dcell)

         ; update front ptr
         (set-rear-ptr! dqueue new-dcell))))

(define (front-delete-dqueue! dqueue)
  (if (empty? dqueue)
      (error "Delete called on empty queue")
      (let ((new-front (get-next (front-ptr dqueue))))
        (set-prev! new-front nil)
        (set-front-ptr! dqueue new-front))))
          
(define (rear-delete-dqueue! dqueue)
  (if (empty? dqueue)
      (error "Delete called on empty queue")
      (let ((new-rear (get-prev (rear-ptr dqueue))))
        (set-next! new-rear nil)
        (set-rear-ptr! dqueue new-rear))))

(define (print-queue dqueue)
  (define (traverse dcell)
    (if (null? dcell)
        (display "nil")
        (begin
          (display (get-value dcell))
          (display " -> ")
          (traverse (get-next dcell)))))
  (traverse (front-ptr dqueue)))
                              
      

  




























