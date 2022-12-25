#lang sicp

; Ex. 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; TODO: import from chapter1
(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ((xa (x-point (start-segment s)))
        (ya (y-point (start-segment s)))
        (xb (x-point (end-segment s)))
        (yb (y-point (end-segment s))))
    (make-point (average xa xb) (average ya yb))))
  
(define point-a (make-point -1 0))
(define point-b (make-point 3 -4))

(define segment (make-segment point-a point-b))
; (print-point (midpoint-segment segment))

; Ex. 2.3
; Defined in terms of two points
(define (make-rectangle top-left bottom-right)
  (cond ((>= (x-point top-left) (x-point bottom-right))
         (error "Top-left X coordinate must be less then bottom-right X coordinate!"))
        ((<= (y-point top-left) (y-point bottom-right))
         (error "Top left Y coordinate must be greater then botom right Y coordinate!"))
        (else (cons top-left bottom-right))))

(define top-left car)
(define bottom-right cdr)

(define (width rec)
  (abs (- (x-point (bottom-right rec))
          (x-point (top-left rec)))))
(define (height rec)
  (abs (- (y-point (top-left rec))
          (y-point (bottom-right rec)))))

(define (perimeter rec)
  (* 2 (+ (width rec)
          (height rec))))
(define (area rec)
  (* (width rec)
     (height rec)))

(define rectangle (make-rectangle point-a point-b))
; (perimeter rectangle)
; (area rectangle)

; Ex. 2.5
(define (new-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (new-car z)
  (define (car a z)
    (if (even? z)
        (car (+ a 1) (/ z 2))
        a))
  (car 0 z))

(define (new-cdr z)
  (define (cdr b z)
    (if (zero? (remainder z 3))
        (cdr (+ b 1) (/ z 3))
        b))
  (cdr 0 z))
