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