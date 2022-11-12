#lang sicp

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

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? prev-guess guess)
  (< (/ (abs (- guess prev-guess)) guess) tolerance))

(define (square-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (square-root-iter (improve guess x)
                        x)))

(define (sqrt x)
  (square-root-iter 1.0 x))

; Ex. 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve-cube guess x))
      guess
      (cube-root-iter (improve-cube guess x)
                       x)))

(define (cube-root x)
  ((if (> x 0) + -) (cube-root-iter 1.0 (abs x))))

; Ex. 1.9
; Recursive
; (define (_+ a b)
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

(define (fib-iter a b n)
  (if (= n 0)
      a
      (fib-iter b (+ a b) (- n 1))))

(define (fib-tailrec n)
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
(define (f-helper a b c n)
  (if (< n 3) c
        (f-helper b c (+ c (* 2 b) (* 3 a)) (- n 1))))

(define (f-iter n)
  (f-helper 0 1 2 n))
        


