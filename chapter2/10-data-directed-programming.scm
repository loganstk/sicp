#lang sicp

(define operation-and-type-table '())
(define coercion-table '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put-helper k item array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k item (cdr array))))))
(define (get-helper k array)
    (cond ((null? array) false)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))

; Operation and type table ops
(define (put op type item)
  (set! operation-and-type-table (put-helper (list op type) item operation-and-type-table)))
(define (get op type)
  (get-helper (list op type) operation-and-type-table))

; Coercion table ops
(define (put-coercion from-type to-type proc)
  (set! coercion-table (put-helper (list from-type to-type) proc coercion-table)))
(define (get-coercion from-type to-type)
  (get-helper (list from-type to-type) coercion-table))

; Tagging utils
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; Ex. 2.81
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (not (eq? type1 type2))
;                    (let ((t1->t2 (get-coercion type1 type2))
;                          (t2->t1 (get-coercion type2 type1)))
;                      (cond (t1->t2
;                             (apply-generic op (t1->t2 a1) a2))
;                            (t2->t1
;                             (apply-generic op a1 (t2->t1 a2)))
;                            (else (error "No method for these types"
;                                         (list op type-tags))))))
;              (error "No method for these types"
;                     (list op type-tags))))))))

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (square x) (* x x))

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
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put '=zero? '(rectangular) =zero?)
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
  (define (=zero? z) (= (magnitude z) 0))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero?)
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

; 2.5.1 Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; Handling integers
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer (lambda (x) (tag x)))
;  (put 'raise 'integer (lambda (x) (make-rational x 1)))
  'done)
(define (make-integer n)
  ((get 'make 'integer) n))

; Handling rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x) (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational)
       (lambda (x) (=zero-rat? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
;  (put 'raise 'rational
;       (lambda (x) (make-real (/ (numer x) (denom x)))))
  'done)
; Rational public proc
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer x)
  ((get 'numer 'rational) x))
(define (denom x)
  ((get 'denom 'rational) x))

; Handling real numbrers
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '('real 'real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '('real 'real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '('real 'real)
       (lambda (x y) (tag (* x y))))
  (put 'div '('real 'real)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '('real 'real)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '('real 'real)
       (lambda (x y) (= x y)))
  (put '=zero? '('real)
       (lambda (x) (= x 0)))
  (put 'make 'real (lambda (x) (tag x)))
;  (put 'raise 'real
;       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)
(define (make-real n)
  ((get 'make 'real) n))

; Handling complex numbers
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
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
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex)
       (lambda (z) (=zero? z))) ; delegating to polar and rectangular packages for optimization
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'raise 'real
       (lambda (x) (tag (make-from-real-imag x 0))))
  ; (put 'raise 'complex (lambda (x) (tag x)))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Install packages
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Ex. 2.77
; (define z (make-complex-from-real-imag 3 4))
; (magnitude z)

; 'apply-generic is called twice: first to locate the generic 'magnitude procedure
; for a complex number, and second time to find the representation-specific
; version installed as part of polar or rectangular packages.

; Ex. 2.78
; (define (attach-tag type-tag contents)
;   (if (number? contents)
;       contents
;       (cons type-tag contents)))
;
; (define (type-tag datum)
;      (cond ((pair? datum) (car datum))
;            ((number? datum) 'scheme-number)
;           (else (error "Bad tagged datum: TYPE-TAG" datum))))
;
; (define (contents datum)
;   (cond ((pair? datum) (cdr datum))
;         ((number? datum) datum)
;         (else (error "Bad tagged datum: CONTENTS" datum))))

; Ex. 2.79
(define (equ? x y)
  (if (eq? (type-tag x) (type-tag y))
      (apply-generic 'equ? x y)
      false))

; Ex. 2.80
(define (=zero? x) (apply-generic '=zero? x))

; Coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

; Ex. 2.83
; We have 2 options here:
;   (a) embed the "type casting" into the correspoing number packages,
;       so that each type is responsible for raising itself to the upper level
;       in the hierarchy. When adding a new type one should simply define the
;       procedure to raise it to the next known supertype, so that generic operations
;       on two numbers can be performed after a few iterations of raising.
; (define (raise obj)
;  ((get 'raise (type-tag obj)) (contents obj)))
; Or
;   (b) define the type system outside the number packages, making it an
;       independent concern. Authors of the book seem to be giving a bold hint
;       toward this approach. In fact, this allows us to use any number package
;       in isolation without knowing about any of its super/subtypes. Another
;       advantage is that all the type hierarchy information and casting operations
;       are  grouped within the single component and no changes to original
;       number packages are required if we want to add a new cast rule.

(define (integer->rational x) (make-rational x 1))
(define (rational->real x) (make-real (/ (numer x) (denom x))))
(define (real->complex x) (make-complex-from-real-imag x 0))

(put-coercion 'integer 'rational integer->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real->complex)

(define type-hierarchy '(integer rational real complex))

(define (raise x)
  (let* ((from-type (type-tag x))
         (to-type (cadr
                   (memq from-type type-hierarchy))))
    (if (null? to-type)
        x
        (let ((coercion-proc (get-coercion from-type to-type)))
          (if coercion-proc
              (coercion-proc (contents x))
              (error "No coercion for these types -- RAISE"
               (list from-type to-type)))))))

; Ex. 2.84 WIP
(define (raise-to type x)
  (let ((source-type (type-tag x)))
    (if (eq? source-type type)
        x
        (raise-to type (raise x)))))

; Checks whether type 'x is a subtype of 'type
(define (subtype? x type)
  (let ((rest (memq x type-hierarchy)))
    (if (list? rest)
        (memq type rest)
        (error "Type is not part of hierarchy -- SUBTYPE" x))))
