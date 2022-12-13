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

(define (within-tolerance a b tolerance)
  (< (abs (- a b)) tolerance))

; The percentage tolerance of a product is within +-1 difference of sum of percentage tolerances of operands
(within-tolerance (percent product)
                  (+ (percent multiplicand)
                     (percent multiplier))
                  1)

; Hierarchical Data and Closure Property
(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Ex. 2.17
(define (last-pair list)
  (if (= (length list) 1)
      (car list)
      (last-pair (cdr list))))

; Ex. 2.18
(define (reverse list)
  (define (reverse result list)
    (if (null? list)
        result
        (reverse (cons (car list) result) (cdr list))))
  (reverse nil list))

; Ex. 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

; (cc 100 us-coins)
; (cc 100 uk-coins)

; Ex. 2.20
(define (same-parity first . rest)
  (let ((first-even (even? first)))
    (define (helper result list)
      (if (null? list)
          result
          (if (equal? first-even (even? (car list)))
              (helper (cons result (car list)) (cdr list))
              (helper result (cdr list)))))
    (helper first rest)))

; Ex. 2.21
; TODO: import from chapter 1
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

; Ex. 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter (reverse items) nil))

; Ex. 2.23
(define (for-each proc items)
   (define (iter first rest)
     (proc first)
     (for-each proc rest))
  (if (null? items)
      nil
      (iter (car items) (cdr items))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Ex. 2.25
(define list1 (list 1 3 (list 5 7) 9))
(cadr (cadr (cdr list1))) ; (car (cdr (car (cdr (cdr list1)))))

(define list2 (list (list 7)))
(caar list2) ; (car (car list2))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr list3)))))) ; ...

; Ex. 2.26
; (define x (list 1 2 3))
; (define y (list 4 5 6))

; > (append x y)
; (1 2 3 4 5 6)

; > (cons x y)
; ((1 2 3) 4 5 6)

; > (list x y)
; ((1 2 3) (4 5 6))

; Ex. 2.27
(define (deep-reverse items)
  (define (deep-reverse result items)
    (cond ((not (list? items)) items)
          ((null? items) result)
          (else (deep-reverse (cons (deep-reverse nil (car items)) result) (cdr items)))))
  (deep-reverse nil items))

; Ex. 2.28
(define (fringe tree)
  (define leaf? number?)
  (cond ((null? tree) nil)
        ((leaf? tree) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))
