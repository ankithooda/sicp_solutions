#lang sicp

;;;;;;;;;;;;;;;;;; Queue Implementation ;;;;;;;;;;;;;;;;

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
            ((eq? m `empty-queue?) empty-queue?)
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

(define (empty-queue? queue)
  ((queue `empty-queue?)))

;;;;;;;;;;;;;;;;;;;;;;; Queue Implementation End ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; Logical Function Implementations ;;;;;;;;;;;;;;;;;;
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid Signal" s))))

(define (logical-and s1 s2)
  (cond ((not (and (number? s1)
                   (number? s2))) (error "Invalid Signals" s1 s2))
        ((and (= s1 1)
              (= s2 1)) 1)
        (else 0)))

(define (logical-or s1 s2)
  (cond ((not (and (number? s1)
                   (number? s2))) (error "Invalid Signals" s1 s2))
        ((or (= s1 1)
             (= s2 1)) 1)
        (else 0)))


(define (inverter input output)
  (define inverter-delay 2)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  `ok)

(define (and-gate a1 a2 output)
  (define and-gate-delay 3)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  `ok)

(define (or-gate a1 a2 output)
  (define or-gate-delay 5)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  `ok)

;;;;;;;;;;;;;;;;;;;;;;;;; Implementation End ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;; Wire Implementation Start ;;;;;;;;;;;;;;;;;;;

(define (make-wire)

  ; A wire is implemented as a procedure state with two
  ; state objects
  ; signal-value
  ; action-procedures : A list of procedures to call whenever the
  ; signal value for this particular wire changes.
  (let ((signal-value 0)
        (action-procedures `()))

    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          `done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
      ;(proc))

    (define (dispatch m)
      (cond ((eq? m `get-signal) signal-value)
            ((eq? m `set-signal!) set-my-signal!)
            ((eq? m `add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))

    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      `done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire `get-signal))

(define (set-signal! wire new-value)
  ((wire `set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire `add-action!) action-procedure))


;;;;;;;;;;;;;;;;;;;;;;; Wire Implementation End ;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;; Agenda Implementation Start ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Segment ;;;;;;

; A segment is cons of two objects
; car represents time within simulation
; cdr represents the list of actions that should be called.
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

;;;;;;;;;;;; Segment End ;;;;;;


;;;;;;;;;;;; Agenda Start ;;;;;;

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)

  ; Get a list of segments
  ; Checks if the time 
  (define (belongs-before? segment-list)
    (or (null? segment-list)
        (< time (segment-time (car segment-list)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
      

;;;;;;;;;;;;;;;;;;;;;;; Simulation Implementation Start ;;;;;;;;;;;;;;;;;

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      `done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;;;;;;;;;;;;;;;;;;;; Simulation Implementation End ;;;;;;;;;;;;;;;;;;;;;

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))

;(define input-1 (make-wire))
;(define input-2 (make-wire))
;(define sum (make-wire))
;(define carry (make-wire))

;(probe `sum sum)
;(probe `carry carry)

;(define (half-adder a b s c)
;  (let ((d (make-wire))
;        (e (make-wire)))
;    (or-gate a b d)
;    (and-gate a b c)
;    (inverter c e)
;    (and-gate d e s)
;    `ok))


;(half-adder input-1 input-2 sum carry)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define input1 (make-wire))
(define input2 (make-wire))
(define out (make-wire))

;(probe `input1 input1)
;(probe `input2 input2)
;(probe `output and-out)

;(and-gate input1 input2 and-out)

(or-gate input1 input2 out)




