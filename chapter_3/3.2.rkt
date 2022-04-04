#lang sicp

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (x)
      (if (eq? x `how-many-calls?)
          call-count
          (begin (set! call-count (+ call-count 1))
                 (f x))))))
      
  