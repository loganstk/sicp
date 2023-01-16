#lang sicp

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
     (if (pair? datum)
         (car datum)
         (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (square x) (* x x))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z) (eq? (type-tag z) 'polar))

; Rectangular
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Polar
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
(put 'make-from-real-imag 'polar
     (lambda (x y) (tag (make-from-real-imag x y))))
(put 'make-from-mag-ang 'polar
     (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

; Generic procedures
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; Ex. 2.73
(define (variable? v) (symbol? v))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-arithmetic-deriv-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2) (* m1 m2)))
          (else (list '* m1 m2))))
  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))

  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent) (expt base exponent)))
          (else (list '** base exponent))))
  (define (deriv-exponentiation exp var)
    (make-product (exponent exp)
                  (make-exponentiation (base exp)
                                       (- (exponent exp) 1))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

; Ex. 2.74
(define (get-record name file)
  (let ((record ((get 'get-record (type-tag file))
                 name
                 (contents file))))
    (if record
        (attach-tag (type-tag file record))
        false)))
        
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

(define (find-employee-record name files)
  (if (null? files)
      false
      (let ((record (get-record name (car files))))
        (if record
            record
            (find-employee-record name (cdr files))))))

; Ex. 2.75
; (define (make-from-mag-ang m a)
;   (define (dispatch op)
;     (cond ((eq? op 'real-part) (* m (cos a)))
;           ((eq? op 'imag-part) (* m (sin a)))
;           ((eq? op 'magnitude) m)
;           ((eq? op 'angle) a)
;           (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
;   dispatch)
;
; (define (apply-generic op arg) (arg op))

; Ex. 2.76
; For the explicit dispatch approach, adding either a new type or a new operation
; requires changes to existing generic procedures. One has to make sure they cover all
; type/operation combos and don't miss anything. Also, name conflicts are
; inevitable.
;
; In a data-directed table-driven approach adding a new type requires no changes to
; the generic operations code. A new operation is added as a generic procedure and it's
; a responsibility of a package developer to provide an implemenation and install it
; into the dispatch table. Name conflicts aren't a problem.
;
; In a message-passing style data types have the strongest encapsulation. When adding
; a new type, one should only provide a procedure that will dispatch to an apropriate
; operation based on the arguments. Adding a new operation requires changes to all the types
; at the same time.
;
; Adding new types using either data-directed or message-passing style is relatively easy.
; To add new operations, data-directed approach seems to be less painful. I guess...

