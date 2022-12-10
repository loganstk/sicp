; Combinations
; (operator operand1 operand2 ...operandN)
(+ 1 2 3)		; 6
(* 25 4 12)		; 1200

; Special forms are not combinations and have their own evaluation rules
(define a 42)
(define b 24)

; Procedure definitions
; (define (<name> <formal parameters>) <body>)
(define (square x) (* x x))

; Applicative order evaluation - evaluate arguments and then apply (call-by-value)
; Normal order evaluation - fully expand then reduce (call-by-name)

; Conditional expressions
;
; (cond (<p1> <e1>)
;       (<p2> <e2>) - clauses
;       ...
;       (else <e>))


; If special form
; (if <predicate> <consequent> <alternative>)
; #t - TRUE, #f - FALSE 
(if (> a 0) 1 (- 1))

; (and <e1> ... <en>)
(and (> a 0) (< a 10)) ; #f

; (or <e1> ... <en>)
(or (= a 42) (= b 0))  ; #t 

; (not <e>)
(not #t)               ; #f

; Creating anonymous procedures - labmda special form
; (lambda (<formal-parameters>) (<body>)

(lambda (x) (+ x 4)) ; the procedure that returns its input incremented by 4
(lambda (x) (/ 1.0 (* x (+ x 2)))) ; the procedure that returns the reciprocal of its input times its input plus 2

; let special form - creating local variables
; (let ((<var1> <exp1>)
;       (<var2> <exp2>)
;       ...
;       (<varn> <expn>))
;     <body>)
; TODO: add example

; Pairs
; (cons (x y)) - construct a pair
; (car z) - get first element of the pair
; (cdr z) - get second element of the pair
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z)) 		; 1
(car (cdr z))		; 3

; Sequences
; (list <a1> <a2> ... <an>)
; is equivalent to
; (cons <a1> (cons <a2> (cons ... (cons <an> nil) ...)))
(list 1 2 3 4)