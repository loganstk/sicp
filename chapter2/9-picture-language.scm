#lang sicp

(#%require sicp-pict)

; 2.2.4 A Picture Language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

; (define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; Ex. 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
    (below bottom top))))

(define (flipped-pairs-v2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit-v2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
  (combine4 (corner-split painter n))))

; Ex. 2.45
(define (split p1 p2)
  (define (combined painter n)
    (if (= n 0)
        painter
        (let ((smaller (combined painter (- n 1))))
          (p1 painter (p2 smaller smaller)))))
  (lambda (painter n) (combined painter n)))

(define right-split-v2 (split beside below))
(define up-split-v2 (split below beside))

; NOTE: The implementation for the commented out procedures is
; provided by the sicp-pict package

; Ex. 2.46
; (define (make-vect x y) (cons x y))
; (define (xcor-vect v) (car v))
; (define (ycor-vect v) (cdr v))

; (define (add-vect v1 v2)
;   (make-vect (+ (xcor-vect v1)
;                 (xcor-vect v2))
;              (+ (ycor-vect v1)
;                 (ycor-vect v2))))
; (define (sub-vect v1 v2)
;   (make-vect (- (xcor-vect v1)
;                 (xcor-vect v2))
;              (- (ycor-vect v1)
;                 (ycor-vect v2))))
; (define (scale-vect v s)
;   (make-vect (* s (xcor-vect v))
;              (* s (ycor-vect v))))

; Ex. 2.47
; (define (make-frame origin edge1 edge2)
;   (list origin edge1 edge2))
; (define (origin-frame frame) (car frame))
; (define (edge1-frame frame) (cadr frame))
; (define (edge2-frame frame) (caddr frame))

; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))
; (define (origin-frame frame) (car frame));
; (define (edge1-frame frame) (cadr frame));
; (define (edge2-frame frame) (cddr frame));

(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v) (frame-edge1 frame))
                 (vector-scale (vector-ycor v) (frame-edge2 frame))))))

; Ex. 2.48
; (define (make-segment start end) (cons start end))
; (define (start-segment s) (car s))
; (define (end-segment s) (cdr s))

; (define (segments->painter segment-list)
;   (lambda (frame)
;     (for-each
;      (lambda (segment)
;        (draw-line
;         ((frame-coord-map frame)
;          (start-segment segment))
;         ((frame-coord-map frame)
;          (end-segment segment))))
;      segment-list)))

; Ex. 2.49a
(define zero (make-vect 0 0))
(define one-xy (make-vect 1 1))
(define one-x (make-vect 1 0))
(define one-y (make-vect 0 1))

(define frame-painter
  (segments->painter
   (list (make-segment zero one-x)
         (make-segment one-x one-xy)
         (make-segment zero one-y)
         (make-segment one-y one-xy))))

; Ex. 2.49b
(define x-frame-painter
  (segments->painter
   (list (make-segment zero one-xy)
         (make-segment one-x one-y))))

; Ex. 2.49c
(define midpoint-left (make-vect 0 0.5))
(define midpoint-top (make-vect 0.5 1))
(define midpoint-right (make-vect 1 0.5))
(define midpoint-bottom (make-vect 0.5 0))

(define diamond-painter
  (segments->painter
   (list (make-segment midpoint-left midpoint-top) 
         (make-segment midpoint-left midpoint-bottom)
         (make-segment midpoint-right midpoint-top)
         (make-segment midpoint-right midpoint-bottom))))

; Ex. 2.49d
; (c) https://stackoverflow.com/a/41507786/5660182
(define wave-segments
  (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))))

(define wave (segments->painter wave-segments))
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

; Ex. 2.50
; Fliip horizontaly
(define (my-flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Rotate 180 degrees counter-clockwise using frame transform
(define (my-rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

; Or reuse rotate90 procedure
(define (apply-transformation painter transformation times)
  (if (= times 0)
      painter
      (apply-transformation (transformation painter)
                            transformation
                            (- times 1))))

(define (another-rotate180 painter)
  (apply-transformation painter rotate90 2))

; Rotate 270 degrees counter-clockwise
(define (my-rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Or reuse rotate90 procedure
(define (another-rotate270 painter)
  (apply-transformation painter rotate90 3))

; Ex. 2.51
(define (my-below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-above
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0)))
         (paint-below
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point)))
    (lambda (frame)
      (paint-above frame)
      (paint-below frame))))

; In terms of beside
(define (another-below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

; Ex. 2.52a
(define smiling-wave
  (segments->painter
   (append
    (list (make-segment (make-vect 0.0 0.0) (make-vect 0.1 0.1)) ; imagine these are not just some random coords :)
          (make-segment (make-vect 0.1 0.1) (make-vect 0.2 0.0)))
    wave-segments)))

; Ex. 2.52b
(define (another-corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (corner (another-corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

; Ex. 2.52c
(define (another-square-limit painter n)
  (let* ((flipped-painter (flip-horiz painter))
         (quarter (corner-split flipped-painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))
