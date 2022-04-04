#lang sicp

;;;;;;;;;;;;;;;;;;;;;; Table Impl ;;;;;;;;;;;;;;;;;;;

(define (make-table equality-check?)
  (let ((local-table (cons `**table** nil)))
    (define (resolve key-list records)
      (define (assoc-recur key-list records last-subtable)
        (cond ((null? records) (list `partial key-list last-subtable))
              ((null? key-list) (list `complete key-list last-subtable))
              ((equality-check? (caar records) (car key-list))
               (assoc-recur (cdr key-list) (cdr (car records)) (car records)))
              (else (assoc-recur key-list (cdr records) last-subtable))))
  
      (assoc-recur (append key-list (list `raw)) records local-table))

    (define (construct-subtable keys item)
      (cond ((null? keys) nil)
            ((eq? (car keys) `raw) (cons `raw item))
            (else (list (car keys) (construct-subtable (cdr keys) item)))))

    (define (lookup key-list)
      (let ((resolved-record (resolve key-list (cdr local-table))))
        (let ((resolution-type (car resolved-record))
              (target-record (caddr resolved-record)))
          (if (eq? resolution-type `complete)
              (cdr target-record)
              false))))
             
    (define (insert! key-list item)
      (let ((resolved-record (resolve key-list (cdr local-table))))
        (let ((resolution-type (car resolved-record))
              (pending-keys (cadr resolved-record))
              (target-record (caddr resolved-record)))
          (cond ((eq? resolution-type `complete)
                 (set-cdr! target-record item))
                ((eq? resolution-type `partial)
                 (set-cdr! target-record
                           (cons (construct-subtable pending-keys item)
                                 (cdr target-record))))))))
    (define (print)
      (display local-table))
    
    (define (dispatch m)
      (cond ((eq? m `insert!) insert!)
            ((eq? m `lookup) lookup)
            ((eq? m `print) print)
            (else "Method not available on table" m)))

    dispatch))


(define (put! table key-list item)
  ((table `insert!) key-list item)
  `done)

(define (get table key-list)
  ((table `lookup) key-list))

;;;;;;;;;;;;;;;;;;;;;;;;;; Table Impl ;;;;;;;;;;;;;;;;;;;;;;


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table eq?)))
    (lambda (x)
      (let ((prev-result (get table (list x))))
        (if prev-result
            prev-result
            (let ((result (f x)))
              (put! table (list x) result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
                   

