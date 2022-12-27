#lang sicp

; Symbolic Data

(define (memq item xs)
  (cond ((null? xs) false)
        ((eq? item (car xs)) xs)
        (else (memq item (cdr xs)))))

; Ex. 2.53
; (list 'a 'b 'c) => (a b c)
; (list (list 'george)) => ((george))
; (cdr '((x1 x2) (y1 y2))) => ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) => (y1 y2)
; (pair? (car '(a short list))) => false
; (memq 'red '((red shoes) (blue socks))) => false
; (memq 'red '(red shoes blue socks)) => (red shoes blue socks)

; Ex. 2.54
(define (my-equal? xs ys)
  (cond ((and (symbol? xs) (symbol? ys)) (eq? xs ys))
        ((and (list? xs) (list? ys))
         (and (my-equal? (car xs) (car ys))
              (my-equal? (cdr xs) (cdr ys))))
        (else false)))

; Ex. 2.55
; (car ''abracadabra) is equivalent to (car (quote (quote (abracadbara)))).

; Symbolic Differentiation
(define (variable? v) (symbol? v))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

; Ex. 2.56
; (** x n) <=> x^n
(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent) (expt base exponent)))
        (else (list '** base exponent))))

;(define (sum? s) (and (pair? s) (eq? (car s) '+)))
;(define (addend s) (cadr s))
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2))
;         (+ a1 a2))
;        (else (list '+ a1 a2))))

;(define (product? p) (and (pair? p) (eq? (car p) '*)))
;(define (multiplier p) (cadr p))
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2) (* m1 m2)))
;        (else (list '* m1 m2))))

; Ex. 2.57
;(define (augend s)
;  (let ((rest (cddr s)))
;    (if (= (length rest) 1)
;        (car rest)
;        (cons '+ rest))))

;(define (multiplicand p)
;  (let ((rest (cddr p)))
;    (if (= (length rest) 1)
;        (car rest)
;        (cons '* rest))))

; Ex. 2.58a
(define (sum? s) (and (pair? s) (eq? (cadr s) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? p) (and (pair? p) (eq? (cadr p) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (- (exponent exp) 1))))
        (else
         (error "unknown expression type: DERIV" exp))))
