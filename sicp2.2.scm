#lang sicp

;Note: Temporarily switched to Racket R5RS as the interpreter
;displays lists/trees a bit more clearly

(define nil '())

;Givens:
(define (list-ref1 items n)
  (if (= n 0)
      (car items)
      (list-ref1 (cdr items) 
                (- n 1))))

(define (length1 items)
  (if (null? items)
      0
      (+ 1 (length1 (cdr items)))))

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
(define (reverse1 list1)
  (if (null? list1)
      list1
      (cons (reverse1 (cdr list1)) (car list1))))

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
(define (map1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map1 proc (cdr items)))))

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
(define (for-each1 fn items)
  (if (null? items)
      items
      (begin
        (fn (car items))
        (for-each1 fn (cdr items)))))

(define (for-each2 fn items)
  (cond ((null? items)
         (true))
        (else
         (fn (car items))
         (for-each1 fn (cdr items)))))

(for-each1 
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
;(done in a notebook)

;Ex 2.25: Select the cell containing '7' - Gets tricky.
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

(define rlist (deep-reverse (list (list 1 2 3) (list (list 10 11) 5 (list 12 13)) (list 7 8 9))))

;2.28 Fringe: Recurse and flatten a tree in prefix order 
;First Try:  This does in fact recurse in the correct order, but does not flatten the tree
(define (fringe-bad listp)
  (define (fringe-left listp)
    (cond ((not (pair? (car listp))) (newline) (display (car listp)) (car listp))
          (else
           (cons (fringe-left (car listp))
                   (fringe-right (car listp))))))
  (define (fringe-right listp)
    (cond ((not (pair? (cdr listp))) (newline) (display (cdr listp)) (cdr listp))
          (else
           (cons (fringe-left (cdr listp))
                   (fringe-right (cdr listp))))))
  (cons (fringe-left listp) (fringe-right listp)))

;Taken from scheme wiki
(define (fringe listp)
  (cond ((null? listp) listp)
        ((not (pair? listp)) (list listp))
        (else
         (append (fringe (car listp)) (fringe (cdr listp))))))
  

(fringe-bad (list (list 1 (list 5 6) 2) (list 3 4)))
(fringe (list (list 1 (list 5 6) 2) (list 3 4)))
                      
;2.29 - I made a mistake in my initial branch-weight function which
; caused me some grief, and this took me a few sessions to work through.
; I'm also not watching the lectures along with the book, so the
; mutual recursion threw me off a bit at first.
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define test-mobile
  (make-mobile
   (make-branch 1
                (make-mobile (make-branch 2 3) (make-branch 4 5)))
   (make-branch 3 14)))

(define test-mobile-long ;0
  (cons     ;1<-0   ;@1, from 0
   (cons 1  ;2<-1
    (cons   ;3<-2
     (cons  ;4<-3
      (cons  2;5<-4
       (cons 3;6<-5
        '()
        )    ;5<-6   ;@level 5, closed 6
       )     ;4<-5
      (cons ;5.1<-4
       (cons 4;6.1<-5.1
        (cons 5;7.1<-6.1
         '()
         )   ;6.1<-7.1
        )    ;5.1<-6.1
       '()
       )     ;4<-5.1
      )      ;3<-4
     '()
     )       ;2<-3
    )        ;1<-2
   (cons    ;2.2<-1
    (cons 3 ;3.2<-2.1
     (cons 14  ;4.2<-3.2
      '()
      )      ;3.2<-4.2
     )       ;2.2<-3.2
    '()
    )        ;2.2<-1
   )         ;0<-1
  )          ;00

;2.29.1 Selectors
(define (left-branch mobile)
  (car mobile))

(left-branch test-mobile)

(define (right-branch mobile)
  (cdr mobile))

(right-branch test-mobile)

(define (branch-length branch)
  (if (null? (cdr branch)) 
      (caar branch)
      (car branch)))

(newline)
(display "Branch-length, right branch: ")
(branch-length (right-branch test-mobile))

(newline)
(display "Branch-length, right branch: ")
(branch-length (left-branch test-mobile))


(define (branch-structure branch)
  (if (null? (cdr branch))
      (cadar branch)
      (cadr branch)))

(newline)
(display "Branch-structure, right branch: ")
(branch-structure (right-branch test-mobile))

(newline)
(display "Branch-structure, left branch: ")
(branch-structure (left-branch test-mobile))

;2.29.2
(define (has-mobile branch)
  (pair? (branch-structure branch)))

(define (branch-weight branch)
    (if (not (has-mobile branch))
        (branch-structure branch)
        (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;2.29.3
(define (balanced? mobile)
  (let ((left-torque (* (branch-length (left-branch mobile)) (branch-weight (left-branch mobile))))
        (right-torque (* (branch-length (right-branch mobile)) (branch-weight (right-branch mobile))))
        (left-mobile? (has-mobile (left-branch mobile)))
        (right-mobile? (has-mobile (right-branch mobile))))
    (and
     (= right-torque left-torque)
     (if left-mobile?
         (balanced? (branch-structure (left-branch mobile)))
         #t)
     (if right-mobile?
         (balanced? (branch-structure (right-branch mobile)))
         #t))))
     

(define test-mobile-balanced
  (make-mobile
   (make-branch 2
                (make-mobile (make-branch 2 10) (make-branch 4 5)))
   (make-branch 6 5)))
  

(total-weight test-mobile)
(balanced? test-mobile)
(balanced? test-mobile-balanced)
(balanced? (make-mobile (make-branch 2 test-mobile-balanced) (make-branch 2 test-mobile-balanced)))

;2.29.4
;I'm not going to actually implement the cons'd mobiles
;Theoretically the selectors and supporting functions which use car/cdr-type
;functions would need to be rewritten.  As well the supporting functions which
;use null?/pair? to determine the contents of a branch would need to be rewritten

;2.30.1: square-tree, direct
(define (square-tree-d in)
  (cond ((not (pair? in)) (* in in))
        ((not (pair? (cdr in)))
         (square-tree-d (car in)))
        ((not (pair? (car in)))
         (list (* (car in) (car in)) (square-tree-d (cdr in))))
        (else
         (list (square-tree-d (car in)) (square-tree-d (cdr in))))))

(square-tree-d test-mobile)

;2.30.2: square tree using map
(define (square-tree-m in)
  (map (lambda (sub)
         (if (pair? sub)
             (square-tree-m sub)
             (* sub sub))) in))

(square-tree-m test-mobile)

;2.31
(define (tree-map fn tree)
  (map (lambda (sub)
         (if (pair? sub)
             (tree-map fn sub)
             (fn sub)))
       tree))

(tree-map (lambda (x) (* x x)) test-mobile)

;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;Section 2.2.3 givens
(define (filter1 predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter1 predicate 
                       (cdr sequence))))
        (else  (filter1 predicate 
                       (cdr sequence)))))

(define (accumulate1 op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate1 op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

;2.33.1
;Naming the lambda params really helps here.
;current_item is the item that map_a is running the procedure with.
;computed_rest is the remainder of the sequence
(define (map_a p seq)
  (accumulate1
   (lambda (current_item computed_rest)
     (cons (p current_item) computed_rest))
   nil seq))

;2.33.2
(define (append_a seq1 seq2)
  (accumulate1 cons seq2 seq1))

;Test, these should be the same:
(append (list 1 2 3) (list 4 5 6))
(append_a (list 1 2 3) (list 4 5 6))

;2.33.3
(define (length_a sequence)
  (accumulate1 (lambda (x y) (+ 1 y)) 0 sequence))


;2.34
