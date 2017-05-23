#lang sicp

;; SICP 1.2

;Start from n
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;Start from 1, use lexical scope of n
(define (factorial-up n)
  (define (fact-iter prod i)
    (if (> i n)
        prod
        (fact-iter (* i prod) (+ i 1))))
  (fact-iter 1 1))


;;;;;;;;;
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount
                     (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;;;;;;;;;;;;
;;;Exercise 1.11
;Recursive, straigtforward
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

;Iterative, took a bit of experimentation
(define (f-it n)
  (define (f-it-loop fn-1 fn-2 fn-3 counter)
    
    (define (f-core )
      (+ fn-1 (* 2 fn-2) (* 3 fn-3)))
  
      (cond ((< n 3) n)
          ((= counter n) (f-core ) )
          (else
           (f-it-loop (f-core ) fn-1 fn-2 (+ 1 counter)))))

  (f-it-loop 2 1 0 3))

;Tests.  f-rec starts getting slow after 20
;(f-rec 1) 
;(f-rec 2)
;(f-rec 3)
;(f-rec 4)
;(f-it 1)
;(f-it 2)
;(f-it 3)
;(f-it 4)
;(f-rec 10)
;(f-it 10)
;(f-rec 20)
;(f-it 20)
;(f-rec 30)
;(f-it 30)

;;;;;;;;;;;;;;;;;;
;Exercise 1.12 Pascal's triangle w/recursive process
;Below: 9 layers of Pascal's Triangle, left-aligned
;1
;1   1
;1   2   1
;1   3   3   1
;1   4   6   4   1
;1   5   10  10  5   1
;1   6   15  20  15  6   1
;1   7   21  35  35  21  7   1
;1   8   28  56  70  56  28  8  1

;Could be slightly more optimal in the case of
; (= position 2) or (= position (- layer 1)), where the result
; would be known to be (- layer 1) 
(define (pasc-val layer position)
  (if ( or (<= position 1)
           (>= position layer))
      1
      (+ (pasc-val (- layer 1) (- position 1))
         (pasc-val (- layer 1) position))))

;Exercise 1.16: Iterative exponent algo
; in O(log n) time
(define (fast-exp b n)
 (define (even x) (= (remainder x 2) 2))
 (define (halve x) (truncate (/ x 2)))
 (define (square x) (* x x))

  ;If unclear, the parameters mutate as follows:
  ; next(a) = a if nn is even, otherwise a * bb
  ; next(bb) = square(bb)
  ; next(nn) = half(nn).  Note that the halve function truncates.
  (define (b^n-it a bb nn)
    (cond ((= bb 0) 0)    ;This and (= nn 0) could be moved to the outer function.
          ((= nn 0 ) 1)
          ((= nn 1 ) (* a bb))
          ((even nn)
           (b^n-it a (* bb bb) (halve nn)))
          (else
           (b^n-it (* a bb) (* bb bb) (halve nn)))))
  (b^n-it 1 b n))

;Ex 1.17 O(log n) time multiplication with only add, double, and half
;Very similar to the given exp algo:
; (* a b) = (double (* a (halve b))) if b is even
;           (+ a (* a (- b 1)) if b odd
;  Special cases for b1 == 1, and when either is 0
(define (fast-* a b)
  (define (even x) (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (define (halve x) (truncate (/ x 2)))

  (define (fast-*rec a1 b1)
    (cond ((= b1 1) a1)
          ((or (= a1 0) (= b1 0)) 0)
          ((even b1)
           (double (fast-*rec a1 (halve b1))))
          (else
           (+ a1 (fast-*rec a1 (- b1 1))))))

  ;Ex 1.18
  ;Similar to fast-exp, parameters advance as:
  ; next(n) = n if b1 is even, n+a1 otherwise
  ; next(a1) = double(a1)
  ; next(b1) = halve(b1), where halve is truncated to integer (eg (halve 5) -> 2) 
  (define (fast-*-it n a1 b1)
    (cond ((= b1 1) (+ n a1))
          ((or (= a1 0) (= b1 0)) 0)
          ((even b1)
           (fast-*-it n (double a1) (halve b1)))
          (else
           (fast-*-it (+ n a1) (double a1)  (halve b1)))))

 ;We could further optimize by always using the greater value as a1
 (fast-*-it 0 a b))

;;Fermat's Little Theorem
(define (even? x) (remainder x 2))
(define (square x) (* x x))
(define (halve x) (truncate (/ x 2)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (halve  exp) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))