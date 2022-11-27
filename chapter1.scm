#lang sicp
(#%require srfi/1)

; Hello SICP

; 42

(define radius 5)

(define pi 3.14159)

(define circumference (* 2 pi radius))

(define (square x)
  (* x x))

; (square 2)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; (sum-of-squares 3 4)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (another-abs x)
  (if (< x 0)
      (- x)
      x))

; Recursion
(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

; (factorial 5)

(define (factorial-tailrec x acc)
  (if (= x 1)
      acc
      (factorial-tailrec (- x 1) (* x acc))))

; (factorial-tailrec 5 1)

; Ex. 1.3
(define (max a b)
  (if (> a b) a b))

(define (min a b)
  (if (< a b) a b))

(define (sum-of-squares-larger a b c)
  (sum-of-squares (max a b) (max (min a b) c)))

; Ex. 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Ex. 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Ex. 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else (else-clause))))

; Ex. 1.7
; Square root calculation
(define tolerance 0.001)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? prev-guess guess)
    (< (/ (abs (- guess prev-guess)) guess) tolerance))

  (define (square-root-iter guess)
    (if (good-enough? guess (improve guess))
      guess
      (square-root-iter (improve guess))))
  
  (square-root-iter 1.0))

; Ex. 1.8
(define (cube-root x)
  (define abs-x (abs x))
  
  (define (improve guess)
    (/ (+ (/ abs-x (square guess)) (* 2 guess)) 3))

  (define (good-enough? prev-guess guess)
    (< (/ (abs (- guess prev-guess)) guess) tolerance))
  
  (define (cube-root-iter guess)
    (if (good-enough? guess (improve guess))
      guess
      (cube-root-iter (improve guess))))
  
  ((if (> x 0) + -) (cube-root-iter 1.0)))

; Ex. 1.9
; Recursive
; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))

; Iterative
; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))

; Ex. 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; Fibonacci numbers
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-tailrec n)
  (define (fib-iter a b n)
    (if (= n 0)
      a
      (fib-iter b (+ a b) (- n 1))))
  
  (fib-iter 0 1 n))

; Ex. 1.11
; Recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

; Iterative
(define (f-iter n)
  (define (f-helper a b c n)
    (if (< n 3) c
        (f-helper b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  
  (f-helper 0 1 2 n))

; Counting change
(define coins `(1 5 10 25 50))

(define (count-change amount coins)
  (cond ((zero? amount) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (count-change amount (cdr coins))
                 (count-change (- amount (car coins)) coins)))))

; Ex. 1.12
(define (pascal row col)
  (if (or (zero? col) (= row col))
       1
       (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))

; Exponentiation
(define (exp x n)
  (if (zero? n)
      1
      (* x (exp x (- n 1)))))

(define (fast-exp x n)
  (cond ((zero? n) 1)
        ((even? n) (square (fast-exp x (/ n 2))))
        (else (* x (fast-exp x (- n 1))))))

(define (expt x n)
  (if (> n 0)
      (fast-exp x n)
      (/ 1 (fast-exp x (- n)))))
