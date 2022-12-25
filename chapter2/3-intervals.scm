#lang sicp

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
(define (width-interval i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(= (width-interval (add-interval a b))
   (+ (width-interval a)
      (width-interval b)))

(= (width-interval (sub-interval a b))
   (+ (width-interval a)
      (width-interval b)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Ex. 2.10
(define (div-interval x y)
  (if (zero? (width-interval y))
      (error "Can't divide by interval that spans zero width!")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

; Ex. 2.11
; Ha-ha! Really?!

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

; Ex. 2.12
(define (make-center-percent c p)
  (let ((width (* (abs c) (/ p 100))))
    (make-center-width c width)))

(define (percent i)
  (* (/ 1 (abs (center i)))
     (width-interval i)
     100))

; Ex. 2.13
(define multiplicand (make-center-percent 5 5))
(define multiplier (make-center-percent 4 7))
(define product (mul-interval multiplicand multiplier))

(define (within-tolerance? a b tolerance)
  (< (abs (- a b)) tolerance))

; Tolerance of a sum is within 1% of sum of tolerances of terms
(within-tolerance? (percent product)
                  (+ (percent multiplicand)
                     (percent multiplier))
                  1)
