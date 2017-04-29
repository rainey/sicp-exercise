#lang sicp


;Ex 1.2
(define (ex1.2) (/ (+ 5 4 (- 2 ( - 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))


;Ex 1.3
(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y))
    )

(define (f a)
    (sum-of-squares (+ a 1 ) (* a 2)))

;(define (abs x)
;  (cond ((> x 0) x)
;        ((= x 0) 0)
;        ((< x 0) (- x))))

;(define (abs x)
;  (cond ((< x 0) (- x))
;        (else x)))

(define (abs x)
  (if (< x 0) (- x) x))

(define x 9)
(define y 13)

(define (max x y z)
     (cond ((and (>= x y) (>= x z)) x)
           ((and (>= y x) (>= y z)) y)
           (else z)))

(define (mid x y z)
  (cond ((or (and (<= x y) (>= x z)) (and (>= x y) (<= x z))) x)
        ((or (and (<= y x) (>= y z)) (and (>= y x) (<= y z))) y)
         (else z)))

(define (maxsquares x y z)
  (sum-of-squares (max x y z) (mid x y z)))


;Ex 1.5 Code
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))
;Applicative-order:
; (test 0 (p))
;   Infinite recurse at evaluation of (p) before entering 'test' routine
;Normal-order:
;  (test 0 (p))
;  (if (= x 0) ; ->predicate evaluates to true
;    returns 0


;------------------Ex 1.7
(define (sqrt-iter guess x prev_diff)
  (if (good-enough? guess x prev_diff)
      guess
      (sqrt-iter (improve guess x)
                 x
                 (abs-sq-diff guess x))))

(define (improve guess x)
  (average guess ( / x guess)))

(define (average x y)
  (/ ( + x y) 2))

(define (abs-sq-diff guess x)
  (abs (- (square guess) x)))
  
(define (good-enough? guess x prev_diff)
  (< (abs-sq-diff (abs-sq-diff guess x) prev_diff) 0.000001))

(define (sqrt x)
  (sqrt-iter 1.0 x 0))

;------------Ex 1.8
(define (cube x)
  (* x x x))

(define (abs-cube-diff guess x)
  (abs (- (cube guess) x)))

(define (good-enough-cu? guess x prev_diff)
  (< (abs-cube-diff (abs-cube-diff guess x) prev_diff) 0.000001))

(define (improve-curt guess x)
  (/ (+ guess guess (/ x (square guess))) 3 ))

(define (curt-iter guess x prev_diff)
  (if (good-enough-cu? guess x prev_diff)
      guess
      (curt-iter (improve-curt guess x)
                 x
                 (abs-cube-diff guess x))))

(define (curt x)
  (curt-iter 1.0 x 0.0))

;----------Block structure cube-root (curt)
(define (curt2 x)
  (define (cube y)
    (* y y y))

  (define (abs-cube-diff guess y)
    (abs (- (cube guess) y)))

  (define (good-enough-cu? guess prev_diff)
    (< (abs-cube-diff (abs-cube-diff guess x) prev_diff) 0.000001))

  (define (improve-curt guess)
    (/ (+ guess guess (/ x (square guess))) 3 ))

  (define (curt-iter guess prev_diff)
    (if (good-enough-cu? guess prev_diff)
        guess
        (curt-iter (improve-curt guess)
                   
                   (abs-cube-diff guess x))))
  (curt-iter 1.0 0.0))
