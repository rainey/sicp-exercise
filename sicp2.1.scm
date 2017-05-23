#lang sicp

;2.1.1 Example

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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

;From 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Ex. 2.1;  Signed make-rat
(define (make-rat num den)
  (let ((g (gcd (abs num) (abs den)))
        (sign (* (/ num (* num -1)) (/ den (* den -1)) -1)))
    (cons (* sign (abs (/ num g)))
          (* (/ (abs den) g)))))

(display "make-rat test:")
(newline)
(make-rat -1 5)
(make-rat 1 -5)

;Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (mid-point seg)
  (let ((avg (lambda (b e)
               (/ (+ b e ) 2))))
    (make-point
     (avg (x-point (start-segment seg)) (x-point (end-segment seg)))
     (avg (y-point (start-segment seg)) (y-point (end-segment seg))))))

;Given in book
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;Tests
(print-point
 (mid-point
  (make-segment (make-point 0 0) (make-point 10 10))))

(print-point
 (mid-point
  (make-segment (make-point 0 1) (make-point 5.0 10.0))))


;Ex 2.3: Rectangle
(define (make-rectangle len width)
  (cons len width))

(define (rect-area rect)
  (* (car rect) (cdr rect)))

(define (rect-perimeter rect)
  (* 2 (+ (car rect) (cdr rect))))

;Ex 2.3 Part 2
;Make a len,width rectangle from segment
;Translate segment mid-point to 0,0 and 2*x1, 2*y1
;Not sure if this is exactly what the exercise meant.
;We are still representing the rectangle only with w,h
;But constructing differently
(define (translate-seg-mid seg new-mid)
  (let ((mx (x-point (mid-point seg)))
        (my (y-point (mid-point seg)))
        (startx (x-point (start-segment seg)))
        (starty (y-point (start-segment seg)))
        (endx (x-point (end-segment seg)))
        (endy (y-point (end-segment seg))))
    (let ((mid-diffx (- mx (x-point new-mid)))
          (mid-diffy (- my (y-point new-mid))))
      (make-segment (make-point (- startx mid-diffx) (- starty mid-diffy))
                    (make-point (- endx mid-diffx) (- endy mid-diffy))))))
;Quick test of translation:
;Should Print -5,-5; 5,5
(let ((translated
       (translate-seg-mid (make-segment (make-point 0 0) (make-point 10 10)) (make-point 0 0))))
  (print-point (start-segment translated))
  (print-point (end-segment translated)))

(define (make-rectangle-from-seg seg)
  (let ((translated (translate-seg-mid seg (make-point 0 0))))
    (cons (* 2 (abs (x-point (start-segment translated))))
          (* 2 (abs (y-point (start-segment translated)))))))

(rect-area (make-rectangle-from-seg (make-segment (make-point -1 -1) (make-point -12.0 -12.0))))


