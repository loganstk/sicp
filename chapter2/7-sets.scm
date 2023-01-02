#lang sicp

; 2.3.3 Representing sets

; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Ex. 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Ex. 2.60
; TODO: learn how to import stuff
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define element-of-set-dup? element-of-set?)
(define (adjoin-set-dup x set) (cons x set))
(define (intersection-set-dup set1 set2)
  (if (or (null? set1) (null? set2))
          '()
          (filter (lambda (x)
                    (not (element-of-set-dup? x set2)))
                  set1)))
(define (union-set-dup set1 set2) (append set1 set2))

; Sets as ordered lists
(define (element-of-set-ord? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-ord set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ord (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set-ord (cdr set1) set2))
              (else (intersection-set-ord set1 (cdr set2)))))))

; Ex. 2.61
(define (adjoin-set-ord x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-ord x (cdr set))))))

; Ex. 2.62
(define (union-set-ord set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-ord (cdr set1)
                                           (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set-ord (cdr set1)
                                           set2)))
                      (else
                       (cons x2 (union-set-ord set1
                                           (cdr set2)))))))))

; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        (else
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)
            (make-tree (entry set)
                       (adjoin-set-tree x (left-branch set))
                       (right-branch set))))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

; Ex. 2.63
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

; Ex. 2.64
; The '(partial-tree elts n) function constructs a balanced tree
; from the first 'n elements of list 'elts. Left subtree 'left-result
; consists of the first (n-1)/2 elements and is built by invoking
; 'partial-tree recursively and returning a pair consisting of left
; subtree and the rest of the 'elts (named 'non-left-elts) that are
; not included in the left subtree.
;
; The first element of 'non-left-elts is the root (named 'this-entry)
; of a balanced binary tree being constructed.
;
; The right subtree 'right-result is constructed recursively from the
; rest (n-1)/2 elements of the 'elts that are neither included in the
; left sub-tree nor are the root of a current tree.
;
; The function then constructs a binary tree by calling a constructor
; 'make-tree with 'this-entry as a root, 'left-result as left subtree
; and 'right-result as right subtree.

; The function always returns a pair of the constructed balanced tree
; and the list consisting of rest of 'elts except the first 'n ones.

; If 'n is equal to 0, the function terminates returning an empty tree
; and the original list.
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result
                (partial-tree (cdr non-left-elts)
                              right-size))
             (right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; Ex. 2.65
(define (union-set-tree set1 set2)
  (list->tree (union-set-ord (tree->list-2 set1)
                             (tree->list-2 set2))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set-ord (tree->list-2 set1)
                                    (tree->list-2 set2))))

; Ex. 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records))
         (entry set-of-records))
        ((> given-key (entry set-of-records))
         (lookup given-key (right-branch set-of-records)))
        (else
         (lookup given-key (left-branch set-of-records)))))
