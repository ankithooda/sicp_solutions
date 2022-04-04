#lang sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons `() `()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called on an empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair))
        (begin
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair
                         )))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "Delete Item called on empty queue")
      (begin
        (set-front-ptr! queue (cdr (front-ptr queue))))))

(define (print-list-struct list-struct)
  (if (null? list-struct)
      (display "nil")
      (begin
        (display (car list-struct))
        (display " -> ")
        (print-list-struct (cdr list-struct)))))

(define (print-queue queue)
  (print-list-struct (front-ptr queue)))
  