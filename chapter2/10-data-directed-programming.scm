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

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

; Rectangular
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (arctan (div (imag-part z) (real-part z))))
  (define (=zero-rect? z)
    (and (=zero? (real-part z))
         (=zero? (imag-part z))))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put '=zero? '(rectangular) =zero-rect?)
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
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define (=zero-polar? z) (=zero? (magnitude z)))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan (div y x))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero-polar?)
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

; (define (make-from-real-imag x y)
;   ((get 'make-from-real-imag 'rectangular) x y))
; (define (make-from-mag-ang r a)
;   ((get 'make-from-mag-ang 'polar) r a))

; (define (add-complex z1 z2)
;   (make-from-real-imag (+ (real-part z1) (real-part z2))
;                        (+ (imag-part z1) (imag-part z2))))
; (define (sub-complex z1 z2)
;   (make-from-real-imag (- (real-part z1) (real-part z2))
;                        (- (imag-part z1) (imag-part z2))))
; (define (mul-complex z1 z2)
;   (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                      (+ (angle z1) (angle z2))))
; (define (div-complex z1 z2)
;   (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                      (- (angle z1) (angle z2))))

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
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer)
       (lambda (x) (make-real (atan x))))
  (put 'square '(integer)
       (lambda (x) (make-integer (* x x))))
  (put 'square-root '(integer)
       (lambda (x) (make-real (sqrt x))))
  (put 'make 'integer (lambda (x) (tag x)))
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
  (put 'sine '(rational)
       (lambda (x) (make-real (sin (/ (numer x) (denom x))))))
  (put 'cosine '(rational)
       (lambda (x) (make-real (cos (/ (numer x) (denom x))))))
  (put 'arctan '(rational)
       (lambda (x) (make-real (atan (/ (numer x) (denom x))))))
  (put 'square '(rational)
       (lambda (x) (tag (mul-rat x x))))
  (put 'square-root '(rational)
       (lambda (x) (make-real (sqrt (/ (numer x) (denom x))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
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
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '('real)
       (lambda (x) (= x 0)))
  (put 'sine '(real)
       (lambda (x) (make-real (sin x))))
  (put 'cosine '(real)
       (lambda (x) (make-real (cos x))))
  (put 'arctan '(real)
       (lambda (x) (make-real (atan x))))
  (put 'square '(real)
       (lambda (x) (make-real (* x x))))
  (put 'square-root '(real)
       (lambda (x) (make-real (sqrt x))))
  (put 'make 'real (lambda (x) (tag x)))
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
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
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
  (and (eq? (type-tag x) (type-tag y))
       (apply-generic 'equ? x y)))

; Ex. 2.80
(define (=zero? x) (apply-generic '=zero? x))

; Coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

; Ex. 2.81
; See Ex. 2.84

; Ex. 2.82
; Maybe later

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
;       are  grouped within the single component and no changes to the original
;       number packages are required if we want to add a new cast rule.

(define (integer->rational x) (make-rational x 1))
(define (rational->real x) (make-real (/ (numer x) (denom x))))
(define (real->complex x) (make-complex-from-real-imag (make-real x) (make-real 0)))

(put-coercion 'integer 'rational integer->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real->complex)

(define type-hierarchy '(integer rational real complex))

(define (raise obj)
  (let* ((src-type (type-tag obj))
         (target-type (direct-supertype src-type))
         (src->target (get-coercion src-type target-type)))
    (cond ((eq? src-type target-type) obj)
          (src->target (src->target (contents obj)))
          (else 
            (error "No coercion method for these types"
              (list src-type target-type))))))

(define (raise-to obj type)
  (define (iter x)
    (if (eq? (type-tag x) type)
        x
        (iter (raise x))))
  (iter obj))

(define (direct-supertype type)
  (let ((upper-types (memq type type-hierarchy)))
    (if upper-types
        (if (null? (cdr upper-types))
            type
            (cadr upper-types))
        (error "Type is not part of a hierarchy" type))))

(define (direct-subtype type)
  (let ((lower-types (memq type (reverse type-hierarchy))))
    (if lower-types
        (if (null? (cdr lower-types))
            type
            (cadr lower-types))
        (error "Type is not part of a hierarchy"))))

; Ex. 2.84
(define (subtype? type1 type2)
  (let ((upper-types (memq type1 type-hierarchy)))
    (if upper-types
        (memq type2 (cdr upper-types))
        false)))

(define (apply-generic op . args)
 (define (do-apply)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((subtype? type1 type2)
                        (apply-generic op (raise-to a1 type2) a2))
                      ((subtype? type2 type1)
                        (apply-generic op a1 (raise-to a2 type1)))
                      (else (error "Cannot cast types" type-tags))))
              (error "No method for these types"
                      (list op type-tags)))))))
  (let ((result (do-apply)))
    (if (pair? result)
        (drop result)
        result)))

; Ex. 2.85
(define (complex->real x) (real-part x))

(define (real->rational x)
  (let ((rat (rationalize x 0.1)))
    (make-rational
      (inexact->exact (numerator rat))
      (inexact->exact (denominator rat )))))

(define (rational->integer x)
  (make-integer 
    (round (/ (numer x) (denom x)))))

(put-coercion 'complex 'real complex->real)
(put-coercion 'real 'rational real->rational)
(put-coercion 'rational 'integer rational->integer)

(define (project obj)
  (let* ((src-type (type-tag obj))
         (target-type (direct-subtype src-type))
         (src->target (get-coercion src-type target-type)))
    (cond ((eq? src-type target-type) obj)
          (src->target (src->target (contents obj)))
          (else
            (error "No coercion method for these types"
              (list src-type target-type))))))

(define (drop obj)
  (define (iter projection prev)
    (cond ((equ? projection prev) projection)
          ((equ? (raise-to projection (type-tag obj)) obj)
           (iter (project projection) projection))
          (else prev)))
  (iter (project obj) obj))

; Ex. 2.86
; Generic functions
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))
(define (square x) (apply-generic 'square x)) ; could be simply (mul x x)
(define (square-root x) (apply-generic 'square-root x))

; (define a (make-complex-from-real-imag
;            (make-integer 1)
;            (make-integer 1)))
;
; (define b (make-complex-from-real-imag
;            (make-rational 3 4)
;            (make-real 2.7)))
;
; > (add a b)
; (mcons
;  'complex
;  (mcons 'rectangular (mcons (mcons 'rational (mcons 7 4)) (mcons 'real 3.7))))

; > (add a (make-rational 3 4))
; (mcons
;  'complex
;  (mcons 'rectangular (mcons (mcons 'real 7/4) (mcons 'integer 1))))
