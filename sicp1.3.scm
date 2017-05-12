#lang sicp

;Some Examples from section 1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (cube a ) (* a a a ))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-ints a b)
  (sum identity a inc b))

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


;Exercise 1.29
;Integrate function f between a and b
; (changed to use isum from 1.30
(define (simpson-integrate f a b n)
  (define (h-fun) (/ (- b a) n))
  (define (sum-fun k)
    (define (y-fun) (f (+ a (* (h-fun) k))))
    (cond ((or (= k 0) (= k n)) (y-fun))
          ((= (remainder k 2) 0) (* 2 (y-fun)))
          (else (* 4 (y-fun)))))
  (* (/ (h-fun) 3) (isum sum-fun 0 inc n)))

;Exercise 1.30
;Iterative sum
(define (isum term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (+ res (term a)))))
  (iter a 0))

;Exercise 1.31.1
(define (iprod term a b next)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))

;Factorial using the product function
(define (factorial n)
  (iprod identity 1 n inc))


;Find pi using the product function.  Quite slow
;> (exact->inexact (find-pi 30000))
;3.14154029938395
(define (find-pi steps)
  (define (pi-numer n)
    (+ 2 (if (= (remainder n 2) 0)
             n
             (+ n 1))))
  (define (pi-denom n)
    (+ 3 (if (= (remainder n 2) 1)
             (- n 1)
             n)))
  (define (pi-term n)
    (/ (pi-numer n) (pi-denom n)))
  (* 4 (iprod pi-term 0 steps inc)))

;Exercise 1.31.2
(define (rprod term a b next)
  (define (prod-rec a)
    (if (> a b)
        1
        (* (term a) (prod-rec (next a)))))
  (prod-rec a))

;Exercise 1.32.1: Iterative
(define (i-accumulate combiner nullval term a b next)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res (term a)))))
  (iter a nullval))

(define (asum term a b next)
  (i-accumulate + 0 term a b next))

(define (aprod term a b next)
  (i-accumulate * 1 term a b next))

(define (r-accumulate combiner nullval term a b next)
  (define (rec a)
    (if (> a b)
        nullval
        (combiner (term a) (rec (next a)))))
  (rec a))

(define (rasum term a b next)
  (r-accumulate + 0 term a b next))

(define (raprod term a b next)
  (r-accumulate * 1 term a b next))