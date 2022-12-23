#lang sicp

; Sequences as Conventional Interfaces

; TODO: import from chapter1
(define (square x) (* x x))

(define (fib n)
  (define (fib-iter a b count)
    (if (zero? count)
        a
        (fib-iter b (+ a b) (- count 1))))
  (fib-iter 0 1 n))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; (filter even? (list 1 2 3 4 5))
; (2 4)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; (accumulate + 0 (list 1 2 3 4 5))
; 15

; (accumulate * 1 (list 1 2 3 4 5))
; 120

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; (enumerate-interval 2 7)
; (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(define (sum-odd-squares2 tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs2 n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; Ex. 2.33
; (define (map p sequence)
;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

; (define (append seq1 seq2)
;   (accumulate cons seq2 seq1))

; (define (length sequence)
;   (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; Ex. 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

; (horner-eval 2 (list 1 3 0 5 0 1) <=> 1 + 3x + 5x^3 + x^5
; 79

; Ex. 2.35
(define (count-leaves t)
 (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

; Ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex. 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector n r)) m)))

; Ex. 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Ex. 2.39
(define (reverse-right sequence)
  (fold-right (lambda (elem acc) (append acc (list elem))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (acc elem) (cons elem acc)) nil sequence))

; Nested Mappings

; somehting something prime-sum-pairs...

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))   

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

; Ex. 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
                  (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Ex. 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (adds-to? triple sum)
  (= (accumulate + 0 triple) sum))

(define (three-sum s n)
  (filter (lambda (x) (adds-to? x s))
          (unique-triples n)))

; Ex. 2.42
(define (adjoin-position row col rest-of-queens)
  (cons (list row col) rest-of-queens)) ; Inserting a new position at column K
                                        ; at the front of the positions list
  
(define empty-board nil)

; Checks whether the queen 'a' is attacked by the queen 'b'.
(define (under-attack? a b)
  (let ((row-a (car a))
        (col-a (cadr a))
        (row-b (car b))
        (col-b (cadr b)))
    (or (= row-a row-b)
        (= col-a col-b)
        (= (abs (- row-a row-b))
           (abs (- col-a col-b))))))

(define (forall predicate sequence)
  (accumulate (lambda (x y) (and x y)) true
              (map predicate sequence)))
  
(define (safe? positions)
  (let ((k (car positions))             ; Last inserted queen was at column K
        (other-queens (cdr positions)))
    (forall (lambda (q)
              (not (under-attack? k q)))
            other-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (placed-queens)
                 (map (lambda (new-row)
                        (adjoin-position
                         new-row k placed-queens))
                      (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
