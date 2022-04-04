#lang sicp


(define (make-f)
  (let ((call-count 0))
    (lambda (x)
      (begin
        (set! call-count (+ call-count 1))
        (cond ((= x 0) call-count)
              ((= x 1) -1)
              (else (error "Args can be 0 or 1 only.")))))))
                        
               
             
             
          

(define f (make-f))