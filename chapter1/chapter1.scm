#lang sicp

; Hello SICP

; 42

(define radius 5)

(define pi 3.14159)

(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (cube x) (* x x x))

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
; Won't work, because both then-clause and else-clause
; are evaluated when before new-if is applied.
;
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else (else-clause))))
;
; But we could achieve the desired behavior if then-clause
; and else-clause were λ-expressions:
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate (then-clause))
;         (else (else-clause))))
;
; (new-if (zero? 5) 
;         (lambda () (display "true")) 
;         (lambda () (display "false")))

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
(define (expt x n)
  (if (zero? n)
      1
      (* x (exp x (- n 1)))))

(define (fast-expt x n)
  (cond ((zero? n) 1)
        ((even? n) (square (fast-expt x (/ n 2))))
        (else (* x (fast-expt x (- n 1))))))

(define (exp x n)
  (if (> n 0)
      (fast-expt x n)
      (/ 1 (fast-expt x (- n)))))

; Ex. 1.16
(define (fast-expt-iter acc x n)
  (cond ((zero? n) acc)
        ((even? n) (fast-expt-iter acc (square x) (/ n 2)))
        (else (fast-expt-iter (* acc x) x (- n 1)))))

; Ex. 1.17
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (mul a b)
  (cond ((= a 1) b)
        ((even? a) (mul (halve a) (double b)))
        (else (+ b (mul (- a 1) b)))))

; Ex. 1.18
(define (mul-iter acc a b)
  (cond ((zero? a) acc)
        ((even? a) (mul-iter acc (halve a) (double b)))
        (else (mul-iter(+ acc b) (- a 1) b))))

; Ex. 1.19
(define (fib-log n)
  (define (fib-iter a b p q count)
    (cond ((zero? count) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square q) (square p))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

; Greatest common divisors
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

; Testing for primality
(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; The Fermat test
(define (expmod base exp m)
  (cond ((zero? exp) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((zero? times) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Ex. 1.21
; (smallest-divisor 199)
; 199
; (smallest-divisor 1999)
; 1999
; (smallest-divisor 19999)
; 7

; Ex. 1.22
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes count n)
  (cond ((zero? count)
         (newline)
         (display "done"))
        ((even? n) 
         (search-for-primes count (+ n 1)))
        ((timed-prime-test n)
         (search-for-primes (- count 1) (+ n 2)))
        (else (search-for-primes count (+ n 2)))))

; Ex. 1.23
(define (next n)
  (if (= n 2) 
      3 
      (+ n 2)))

; Ex. 1.24
(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      false))

; Ex. 1.25
; In the previous expmod implementation we are relying on the
; property demonstrated in Footnote 1.46. Thus we don't have to 
; deal with extra large numbers.
;
; (define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

; Ex. 1.26
; Louis' expmod procedure calls itself twice every time 
; thus doubling the amount of work it has to perform, so its 
; complexity becomes O(log (2^n)) == O(n).
;
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (* (expmod base (/ exp 2) m)
;                        (expmod base (/ exp 2) m))
;           m))
;         (else
;          (remainder (* base
;                        (expmod base (- exp 1) m))
;           m))))
;
; He could fix this by storing the result of the first call to expmod.
;
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;           (let (partial (expmod base (/ exp 2) m))
;             (remainder (* partial partial)
;                     m)))
;         (else
;          (remainder (* base
;                        (expmod base (- exp 1) m))
;           m))))

; Ex. 1.27
(define (verify-carmichael-num n)
  (define (try-it a)
    (cond ((= a n) true)
          ((= (expmod a n n) a)
           (try-it (+ a 1)))
          (else false)))
  (try-it 1))

; Ex. 1.28
; 561, 1105, 1729, 2465, 2821, and 6601
; The book has a typo: a 'non-trivial-root' x
; must be congruent 1 mod n, not equal to 1 mod n.
(define (miller-rabin-test n)
  (define (non-trivial-root? x)
    (and (not (= x 1))
         (not (= x (- n 1)))
         (= (remainder 1 n) 
            (remainder (square x) n)))) 

  (define (expmod base exp m)
    (cond ((zero? exp) 1)
          ((even? exp)
           (let ((x (expmod base (/ exp 2) m)))
             (if (non-trivial-root? x)
                 0
                 (remainder (square x) m))))
          (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (do-miller-rabin n times)
  (cond ((zero? times) true)
        ((miller-rabin-test n) (do-miller-rabin n (- times 1)))
        (else false)))
