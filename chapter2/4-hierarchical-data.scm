#lang sicp

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
          (else (deep-reverse (cons (deep-reverse nil (car items)) result)
                              (cdr items)))))
  (deep-reverse nil items))

; Ex. 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((number? tree) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

; Ex. 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (number? mobile)
      true
      (let* ((left (left-branch mobile))
             (right (right-branch mobile))
             (left-str (branch-structure left))
             (right-str (branch-structure right)))
        (and (= (torque left) (torque right))
             (balanced? left-str)
             (balanced? right-str)))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))

; Ex. 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Ex. 2.31
(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

; (define (square-tree tree) (tree-map square tree))
; (define (scale-tree factor tree)
;   (tree-map (lambda (x) (* x factor)) tree))

; Ex. 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
