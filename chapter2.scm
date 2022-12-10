#lang sicp

; Chapter 2. Building Abstractions with Data

; Arithmetic Operations for Rational Numbers

; TODO: import from chapter1
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))) ; constructor

(define (numer x) (car x)) ; selector

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
; (print-rat one-half)

(define one-third (make-rat 1 3))
; (print-rat (add-rat one-half one-third))
; (print-rat (mul-rat one-half one-third))
; (print-rat (add-rat one-third one-third))

; Ex. 2.1
(define (make-rat-signed n d)
  (let ((g (gcd n d)))
    (if (negative? d)
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

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
; TODO: import from chapter 1
(define (pow x n)
  (define (sqr x) (* x x))
  (cond ((zero? n) 1)
        ((even? n) (sqr (pow x (/ n 2))))
        (else (* x (pow x (- n 1))))))

(define (new-cons a b)
  (* (pow 2 a) (pow 3 b)))

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

; Ex. 2.7
(define (make-interval a b)
  (if (> a b)
      (error "The upper-bound should be greater then the lower-bound")
      (cons a b)))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define a (make-interval 1 3))
(define b (make-interval 3 4))
(define c (make-interval -2 -2))

; Ex. 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Ex. 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

(= (width-interval (add-interval a b))
   (+ (width-interval a)
      (width-interval b)))

(= (width-interval (sub-interval a b))
   (+ (width-interval a)
      (width-interval b)))

; Ex. 2.10
(define (div-interval x y)
  (if (zero? (width-interval y))
      (error "Can't divide by interval that spans zero width!")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
