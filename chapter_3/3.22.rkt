#lang sicp

(define (make-queue)
  (let ((front-ptr `())
        (rear-ptr `()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (set-front-ptr! item) (set! front-ptr item))

    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called on an empty queue")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item nil)))
        (if (empty-queue?)
            (begin
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair))
            (begin
              (set-cdr! rear-ptr new-pair)
              (set-rear-ptr! new-pair)))))
                             

    (define (delete-queue!)
      (if (empty-queue?)
          (error "Delete Item called on empty queue")
          (begin
            (set-front-ptr! (cdr front-ptr )))))

    (define (print-list-struct list-struct)
      (if (null? list-struct)
          (display "nil")
          (begin
            (display (car list-struct))
            (display " -> ")
            (print-list-struct (cdr list-struct)))))

    (define (print-queue)
      (print-list-struct front-ptr))

    (define (dispatch m)
      (cond ((eq? m `insert-queue!) insert-queue!)
            ((eq? m `delete-queue!) delete-queue!)
            ((eq? m `front-queue) front-queue)
            ((eq? m `print-queue) print-queue)
            (else (error "Unkown Operation - MAKE-QUEUE" m))))
    dispatch))


(define (insert-queue! queue item)
  ((queue `insert-queue!) item))


(define (delete-queue! queue)
  ((queue `delete-queue!)))

(define (front-queue queue)
  ((queue `front-queue)))

(define (print-queue queue)
  ((queue `print-queue)))