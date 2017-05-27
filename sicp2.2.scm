#lang sicp

;Givens:
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;Exercise 2.17: Define a procedure last-pair that returns
;the list that contains only the last element of a given (nonempty)
;list:
(define (last-pair list1)
  (define (last-pair-iter list1 item)
    (if (null? (cdr list1))
        (cons item (car list1))
        (last-pair-iter (cdr list1) (car list1))))
  (if (null? (cdr list1))
      list1
      (last-pair-iter (cdr list1) (car list1))))

;Exercise 2.18 Define a procedure reverse that takes a list as
;argument and returns a list of the same elements in reverse
;order: 
(define (reverse list1)
  (if (null? list1)
      list1
      (cons (reverse (cdr list1)) (car list1))))

;Exercise 2.19
;Consider the change-counting program of 1.2.2.
;It would be nice to be able to easily change the currency used by
;the program, so that we could compute the number of ways to
;change a British pound, for example. As the program is written,
;the knowledge of the currency is distributed partly into the procedure
;first-denomination and partly into the procedure
;count-change (which knows that there are five kinds of U.S.
;coins). It would be nicer to be able to supply a list of coins to be
;used for making change.

;We want to rewrite the procedure cc so that its second argument
;is a list of the values of the coins to use rather than an
;integer specifying which coins to use. We could then have lists
;that defined each kind of currency: 
(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))
;The order of these lists does in fact matter, as the
;Algorithm depends on the largest denominations being
;made available first

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;Ex 2.20: same-parity with dotted notation
(define (same-parity . l1)
  (if (null? (car l1))
      l1
      (let ((parity (remainder (car l1) 2)))
        (define (build-list l)
          (if (null? l)
              l
              (let ((q (car l)))
                (if (= parity (remainder q 2))
                    (cons (car l) (build-list (cdr l)))
                    (build-list (cdr l))))))
        (build-list l1))))

(same-parity 1 2 3 79 33 22 43)
(same-parity 2 3 4 5 6 7)

;Mapping over lists
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list '(1 2 3 4 5))
(square-list2 '(1 2 3 4 5))

;Ex 2.22:  I did not know about 'begin' before writing this
(define (for-each fn items)
  (if (null? items)
      true
      (begin
        (fn (car items))
        (for-each fn (cdr items)))))

(define (for-each2 fn items)
  (cond ((null? items)
         (true))
        (else
         (fn (car items))
         (for-each fn (cdr items)))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

(for-each2
 (lambda (x) (newline) (display x))
 (list 57 321 88))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Ex 2.24

;Ex 2.25: Gets tricky.
(define ex2251 (list 1 3 (list 5 7) 9))
;ex2251
(cadr (caddr ex2251))
(define ex2252 (list (list 7)))
;ex2252
(car (car ex2252))
(define ex2253 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;ex2253
(cadr (cadr (cadr (cadr (cadr (cadr ex2253))))))

;2.27 Deep-Reverse:
;I'm fairly sure this works as intended, but not sure if
;the reversed lists are valid, as it seems lists are
;meant to be null terminated, and this will return pairs
;that begin with null, and terminate with a value
(define (deep-reverse listp)
  (cond ((or (not (pair? listp)) (null? listp)) listp)
        ;((null? (cdr listp)) (cons (car listp) nil))
        (else
         (cons (deep-reverse (cdr listp)) (deep-reverse (car listp))))))

;2.28 Fringe: Recurse tree in prefix order
;(define (fringe listp))


;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))