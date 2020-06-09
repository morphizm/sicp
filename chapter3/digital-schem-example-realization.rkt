#lang racket/base

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(or-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda ()
                    (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ([= s 0] 1)
        ([= s 1] 0)
        (else (error "Wrong signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1) (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal a1) (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)



; get-signal <wire>
; set-signal! <wire> new-value
; add-action! <wire>


; half-adder
; a b ------ s c
; 0 0 ------ 0 0
; 1 0 ------ 1 0
; 0 1 ------ 1 0
; 1 1 ------ 0 1

(define (make-wire)
  (let ([signal-value 0] [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not [= signal-value new-value])
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ([eq? m 'get-signal] signal-value)
            ([eq? m 'set-signal] set-my-signal!)
            ([eq? m 'add-action!] accept-action-procedure!)
            (else [error "Unknown procedure -- WIRE" m])))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ([car procedures])
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire)
  (wire 'set-signal!))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))





; make-agenda
; empty-agenda <action-plan>
; first-agenda-item! <action-plan>
; remove-first-agenda-item! <action-plan>
; add-to-agenda! <time> <action> <action-plan>
; current-time <action-plan>

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                  (newline)
                  (display name)
                  (display " ")
                  (display (current-time the-agenda))
                  (display "  New-value = ")
                  (display (get-signal-wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal input-2 1)
(propagate)

(define (make-time-first-seg time quque)
  (cons time quque))
(define (segment-time s) (car s))
(define (segment-quque s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-quque)])
      (insert-quque! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-quque! (segment-quque (car segments)) action)
        (let ([rest (cdr segments)])
          (if (belongs-before? rest)
              (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
                (add-to-segments! rest)))))))
  (let ([segments (segments agenda)])
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
          (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let [(q (segment-quque (first-segment agenda)))]
    (delete-quque! q)
    (if (empty-quque? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Plan is empty -- FIRST-AGENDA-ITEM")
      (let [(first-seg (first-segment agenda))]
        (set-current-time! agenda (segment-time first-seg))
        (front-quque (segment-quque first-seg)))))














