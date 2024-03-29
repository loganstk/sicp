#lang sicp

; 3.1 Assignment and Local State

; Ex. 3.1
(define (make-accumulator acc)
  (lambda (x)
      (set! acc (+ acc x))
      acc))

; Ex. 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (cond ((eqv? m 'how-many-calls?)
             count)
            ((eqv? m 'reset-count)
             (set! count 0))
            (else
              (set! count (+ count 1))
              (f m))))))

; Ex. 3.3-3.4
(define (call-the-cops) 
  (error "Whoop whoop! That's a sound of da police!"))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m))))
  (let ((attempts 0))
    ; Kind of over-abstracting...
    (define (authorize pwd max-attempts)
      (cond ((eq? pwd password)
             (set! attempts 0)
             (lambda (m) (dispatch m)))
            ((< attempts max-attempts)
             (set! attempts (+ attempts 1))
             (error "Incorrect password!"))
            (else (call-the-cops))))
      (lambda (pwd action) ((authorize pwd 7) action))))

; (define (estimate-pi trials)
;   (sqrt (/ 6 (monte-carlo trials cesaro-test))))

; (define (cesaro-test)
;   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

; Ex. 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random 1.0) range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((total-area (* (- x2 x1) (- y2 y1))))
    (exact->inexact
     (* (monte-carlo
          trials
          (lambda () (P (random-in-range x1 x2)
                        (random-in-range y1 y2))))
        total-area))))

(define (estimate-pi trials)
  (estimate-integral
   (lambda (x y) (<= (+ (* x x) (* y y)) 1.0))
   -1.0
   1.0
   -1.0
   1.0
   trials))

; Ex. 3.8
(define f
  (let ((prev 0))
    (lambda (x)
      (set! prev (- x prev))
      (- x prev))))
