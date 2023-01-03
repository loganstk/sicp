#lang sicp

; 2.3.4 Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                       (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; Ex. 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; (A D A B B C A)

; Ex. 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contains? symbol symbols)
  (cond ((null? symbols) false)
        ((eq? symbol (car symbols)) true)
        (else (contains? symbol (cdr symbols)))))

(define (encode-symbol symbol tree)
  (define (encode bits tree)
    (if (leaf? tree)
        (reverse bits)
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond ((contains? symbol (symbols left))
                 (encode (cons 0 bits) left))
                ((contains? symbol (symbols right))
                 (encode (cons 1 bits) right))
                (else (error "Unknown symbol:" symbol))))))
  (encode '() tree))

(equal? sample-message (encode '(A D A B B C A) sample-tree)) ; #t

; Ex. 2.69
(define (insert-odered ordering-key x xs)
  (cond ((null? xs) (list x))
        ((> (ordering-key x) (ordering-key (car xs)))
         (cons (car xs) (insert-odered ordering-key x (cdr xs))))
        (else (cons x xs))))

(define (successive-merge trees)
  (if (eq? (length trees) 1)
      (car trees)
      (let ((left (car trees))
            (right (cadr trees)))
        (successive-merge (insert-odered weight
                                         (make-code-tree left right)
                                         (cddr trees))))))
            
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Ex. 2.70
(define frequencies '((A 2) (GET 2) (SHA 3) (WAH 1)
                            (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define code-tree (generate-huffman-tree frequencies))

; LOL
(define text '(GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                     SHA BOOM))

(define encoded-message (encode text code-tree))
