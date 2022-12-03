; Combinations
; (operator operand1 operand2 ...operandN)
(+ 1 2 3)
(* 25 4 12)

; Special forms are not combinations and have their own evaluation rules
(define a 42)
(define b 24)

; Procedure definitions
; (define (<name> <formal parameters>) <body>)
(define (square x) (* x x))

; Applicative order evaluation - evaluate arguments and then apply (call-by-value)
; Normal order evaluation - fully expand then reduce (call-by-name)

; Conditional expressions
; (cond (<p1> <e1>)
;       (<p2> <e2>) - clauses
;       ...
;       (else <e>))
;

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
