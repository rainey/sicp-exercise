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
        (sign (/ (* num den) (abs (* num den)))))
    (cons (* sign (abs (/ num g)))
          (* (/ (abs den) g)))))

(display "make-rat test:")
(newline)
(make-rat 2 6)
(make-rat -2 6)
(make-rat 2 -6)
(make-rat -2 -6)

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
;Not sure if this is exactly what the exercise meant, but I'm a bit bored
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

(rect-area (make-rectangle-from-seg
            (make-segment (make-point -1 -1)
                          (make-point -12.0 -12.0))))

;Ex 2.4.1: Verification of car, cons implementations
(define (cons1 x y) 
  (lambda (m) (m x y)))

(define (car1 z) 
  (z (lambda (p q) p)))

(car1 (cons1 1 2))
;First Substitution:
; l1: cons lambda       l2: inner car lambda 
((lambda (m) ( m 1 2))
 (lambda (p q) p))

;Second Substitution (m<-l2)
((lambda (p q)
   p) 1 2)
;1,2 may be replaced by any 'a','b', and the above lambda will yield 'a'

;Ex 2.4.2
(define (cdr1 z)
  (z (lambda (p q) q)))

(cdr1 (cons1 1 2))

;2.5: cons/car/cdr of natural numbers using only
;the exponential formula (2^a)*(3^b) for (cons a b)
(define (cons-int a b)
  (* (expt 2 a) (expt 3 b)))

(define (log2 n)
    (/ (log n) (log 2)))
(define (log3 n)
    (/ (log n) (log 3)))

(define (reduce base n)
  (if (= (remainder n base) 0)
      (reduce base (/ n base))
      n))

(define (car-int i)
  (log2 (reduce 3 i)))

(define (cdr-int i)
  (log3 (reduce 2 i)))

(car-int (cons-int 4 6))
(cdr-int (cons-int 4 6))

;Exercise 2.6: Church numerals
(define zero (lambda (q) (lambda (y) y)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;Not completed

;Section 2.1.4 Extended exercise
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;Ex. 2.7:
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))