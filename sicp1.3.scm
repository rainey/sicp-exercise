#lang sicp

;Some Examples from section 1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (square a) (* a a))
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

(define (even? val)
  (= (remainder val 2) 0))

;Ex. 1.33: Filtered accumulator
(define (filter-accumulate combiner nullval term a b next pred)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a)
              (if (pred (term a))
                  (combiner res (term a))
                  res))))
  
  (iter a nullval))


;Section 1.3.2
(define (test-let x)
  (let ((x 3)
        (y (+ x 2))) ; x on this line uses the 'test-let' x binding
    (* x y))) ;x on this line is bound to the 'let' x, 


(define (search-root f neg-point pos-point)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (search-root f neg-point midpoint))
            ((negative? test-value)
             (search-root f midpoint pos-point))
            (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.00001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search-root f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search-root f b a))
          (else
           (display "Values not of opposite sign" a b))))) ;No error function defined


(define tolerance .00000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (define (average a b)
    (/ (+ a b) 2.0))
  (fixed-point (lambda (y)
                 (average y (/ x y))) 1.0))

;Ex. 1.35
(define (phi)
  (fixed-pointD (lambda (x) (+ 1 (/ 1 x))) 1.0))

;1.36
(define (fixed-pointD f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (ex1-36)
    (define (average a b)
    (/ (+ a b) 2.0))
  (fixed-pointD (lambda (x)
                  (average x (/ (log 1000) (log x))))
                2.0))

(define (ex1-36-noaverage)
    (define (average a b)
      (/ (+ a b) 2.0))
  (fixed-pointD (lambda (x)
                  (/ (log 1000) (log x)))
                2.0))

;1.37.1
(define (cont-frac-rec n d k)
  (define (cont-f-rec i)
    (if ( = i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-f-rec (+ i 1))))))
  (cont-f-rec 1))

(define (find-phi-rec k)
  (cont-frac-rec (lambda (x) 1.0)
                 (lambda (x) 1.0)
                 k))

;1.37.2
(define (cont-frac-it n d k)
  (define (cont-f-it i res)
    (if (= i 1)
        res
        (cont-f-it (- i 1) ( / (n (- i 1))
                               (+ (d (- i 1)) res)))))
  (cont-f-it k (/ (n k) (d k))))

(define (find-phi-it k)
  (cont-frac-it (lambda (x) 1.0)
                 (lambda (x) 1.0)
                 k))
;1.38
(define (e-2 k)
  (cont-frac-it (lambda (x) 1.0)
                (lambda (x)
                  (if (= (remainder (+ x 1) 3) 0)
                      (* 2 (/ (+ x 1) 3))
                      1))
                k))

;1.39
(define (tan-cf x k)
  (exact->inexact
   (cont-frac-it (lambda (k) (if (= k 1) x (* x x -1)))
                 (lambda (k) (+ 1 (* 2 (- k 1))))
                 k)))

;Section 1.3.4
(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x)
    (average x (f x))))

(define (sqrt-ad x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (curt-ad x)
  (fixed-point
   (average-damp (lambda (y) (/ x (* y y))))
   1.0))

;Returns a function which approximates Derivative of g(x), or Dg(x) at x
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.000001)

;Newton's method:
;If g ( x ) is a differentiable function, then a solution
;of the equation g(x) = 0 is a
;fixed point of the function f(x) where
;f(x) = x − (g(x)/Dg(x))

;Gives us f(x) relative to given g
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt-nm x)
  (newtons-method (lambda (y)
                    (- (* y y) x))
                  1.0))

(define (curt-nm x)
  (newtons-method (lambda (y)
                    (- (* y y y) x))
                  1.0))
;Ex. 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;Ex 1.41
(define (double g)
  (lambda (x) (g (g x))))

;Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;Ex 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;Ex. 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ dx x)) (f (- x dx))) 3.0)))

;Ex. 1.45
;n-smooth or n-folded smooth implementation
;Had some trouble with this one.
;Partly because there were no sample outputs
; Explanation: Returns a function which takes one argument, as needed by the lambda in smooth
; The function returned should be, when called as ((n-smooth f n) x)
;       ((smooth (smooth (smooth (smooth f)))) x) ;For n=4
;Example:
;> ((smooth (smooth (smooth square))) 2)
;4.000000000002001
;> ((n-smooth square 3) 2)
;4.000000000002001
(define (n-smooth f n)
   ((repeated smooth n) f))

;Ext 1.46
(define (iterative-improve guess-good? f-improve)
  (define (iterate guess)
    (if (guess-good? guess)
        guess
        (iterate (f-improve guess))))
  iterate)

(define (sqrt-ii x)
  (exact->inexact ((iterative-improve (lambda (y)
                                        (< (abs (- (* y y) x)) 0.00001 ))
                                      (lambda (y)
                                        (/ (+ y (/ x y)) 2)))
                   x)))


(define tolerance .00000001)

(define (fixed-point-ii f first-guess)
  (define (close-enough 
  ((iterative-improve 

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2))
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))
 