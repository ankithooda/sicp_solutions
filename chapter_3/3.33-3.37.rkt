#lang sicp

;;;;;;;;;;;;;;;;;;;;; Connectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints `()))


    (define (set-my-value! new-value requesting-informant)
      (cond ((not (has-value? me))
             (set! value new-value)
             (set! informant requesting-informant)
             (for-each-except requesting-informant
                              inform-about-value
                              constraints))

            ((not (= value new-value))
             (error "Contradiction" (list value new-value)))
            (else `ignored)))
      
    (define (forget-my-value! retractor)
      (if (eq? informant retractor)
          (begin (set! informant false)
                 (set! value false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          `ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))

      (if (has-value? me)
          (inform-about-value new-constraint))
      `done)
      

    (define (me request)
      (cond ((eq? request `has-value?)
             (if informant true false))
            ((eq? request `value) value)
            ((eq? request `set-value!) set-my-value!)
            ((eq? request `forget) forget-my-value!)
            ((eq? request `connect) connect)
            (else (error "Unknown method -- CONNECTOR" request))))
    me))

(define (has-value? connector)
  (connector `has-value?))

(define (get-value connector)
  (connector `value))

(define (set-value! connector new-value requesting-informant)
  ((connector `set-value!) new-value requesting-informant))

(define (forget-value! connector retractor)
  ((connector `forget) retractor))
          
(define (connect connector new-constraint)
  ((connector `connect) new-constraint))

(define (for-each-except exception procedure item-list)
  (define (loop items)
    (cond ((null? items) `done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop item-list))
    
;;;;;;;;;;;;;;;;;; Constraints ;;;;;;;;;;;;;;;;;;;;

(define (adder a1 a2 sum)
  
  (define (process-new-value)
    (cond ((and (has-value? a1)
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1)
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2)
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request `I-have-a-value)
           (process-new-value))
          ((eq? request `I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
    
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

(define (multiplier m1 m2 product)
  
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request `I-have-a-value)
           (process-new-value))
          ((eq? request `I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
    
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)

(define (equalizer a b)
  
  (define (process-new-value)
    (cond ((and (has-value? a)
                (not (has-value? b)))
           (set-value! b (get-value a) me))
          ((and (has-value? b)
                (not (has-value? a)))
           (set-value! a (get-value b) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request `I-have-a-value)
           (process-new-value))
          ((eq? request `I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
    
    (connect a me)
    (connect b me)
    me)

(define (inform-about-value constraint)
  (constraint `I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint `I-lost-my-value))

(define (constant value connector)
  (define (me request)
    (error "Unknow request -- REQUEST" request))

  (connect connector me)
  (set-value! connector value me))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9.0 w)
    (constant 5.0 x)
    (constant 32.0 y)
    `ok))

(celsius-fahrenheit-converter C F)
        
;;;;;;;;;;;;;;;;;;; 3.33 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (averager a b average)
  (let ((s (make-connector))
        (half (make-connector)))
    (adder a b s)
    (constant 0.5 half)
    (multiplier s half average)
    "averager constraint network initialized"))

(define input1 (make-connector))
(define input2 (make-connector))
(define average (make-connector))

;(averager input1 input2 average)

;;;;;;;;;;;;;;;;;;; 3.34 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (squarer a b)
  (multiplier a a b)
  "Squarer network initialized")

(define sq-input (make-connector))
(define sq-output (make-connector))


;(squarer sq-input sq-output)

; For the multiplier constraint atleast two values should be available
; In squarer however, when the output is available none of the two inputs
; can be computed as they are both unknown.


;;;;;;;;;;;;;;;;;; 3.35 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (correct-squarer a b)

  (define (process-new-value)
    (cond ((and (has-value? b)
                (not (has-value? a)))
           (if (< (get-value b) 0)
               (error "square has value less than 0 -- CORRECT-SQUARER")
               (set-value! a
                           (sqrt (get-value b))
                           me)))
          ((and (has-value? a)
                (not (has-value? b)))
           (set-value! b
                       (* (get-value a) (get-value a))
                       me))))

  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request `I-have-a-value)
           (process-new-value))
          ((eq? request `I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
    
  (connect a me)
  (connect b me)
  me)


;;;;;;;;;;;;;;;;;;;;;;;;;;; 3.37 ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c* a b)
  (let ((product (make-connector)))
    (multiplier a b product)
    product))

(define (cv a)
  (let ((c (make-connector)))
    (constant a c)
    c))

 (define (c- x y) 
   (let ((z (make-connector))) 
     (adder z y x) 
     z))

 (define (c/ x y) 
   (let ((z (make-connector))) 
     (multiplier z y x) 
     z))

(define (c+ a b)
  (let ((sum (make-connector)))
    (adder a b sum)
    sum))
  
(define (celsius-fahrenheit-converter-exp x) 
  (c+ (c* (c/ (cv 9) (cv 5)) 
          x) 
      (cv 32))) 

(define C-in (make-connector))
(define F-out (celsius-fahrenheit-converter-exp C-in))


;;;;;;;;;;;;;;;;;; Additional ;;;;;;;;;;;;;;;;;;;

(define (c= a b)
  (equalizer a b))

(define (create-cf-network C F)
  (c= (c* (cv 5) C)
      (c* (cv 5)
          (c- F (cv 32)))))
          
  





