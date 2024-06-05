#lang racket

;divisible-by-x?
(display "\ndivisible-by-x?")

(define (divisible-by-x? x)
(lambda (y)
(cond
  ((= (modulo y x) 0) #t)
  (else #f))))
(define div-by-7 (divisible-by-x? 7))

(displayln ((divisible-by-x? 7)20))
(displayln ((divisible-by-x? 5)20))
(displayln ((divisible-by-x? 3)9))
(displayln (div-by-7 49))

;function-9
(display "\nfunction-9")

(define function-9
(lambda (number) (number 9)))

(displayln (function-9 sqrt))
(displayln (function-9 add1))
(displayln (function-9 (lambda (x) (+ x 7))))

;my-map
(displayln "\nmy-map")

(define (my-map funct mmap)
(cond
  [(empty? mmap) empty]
  [else (cons (funct (first mmap))
  (my-map funct (rest mmap)))]))

(displayln (my-map sqrt '(9 25 81 49)))
(displayln (my-map add1 '(6 4 8 3)))
(displayln (my-map (lambda (n) (* n n)) '(5 7)))
(displayln (my-map even? '(2 5 7 12)))

;pair-up
(displayln "\npair-up")

(define (pair-up pup1 pup2)
(cond
  ((null? pup1) '())
  ((null? pup2) '())
(else
  (cons (list (car pup1) (car pup2))
  (pair-up (cdr pup1) (cdr pup2))))))

(displayln (pair-up '(1 2 3 4) '(a b c d)))
(displayln (pair-up '(1 2 3) '(4 9 5 7)))
(displayln (pair-up '(3 5 6) '("one" 6.18 #t "two")))
(displayln (pair-up '(5) '()))

;classify
(displayln "\nclassify")

(define (class1 fn lst1)
(cond
  ((null? lst1) lst1)
  ((fn (car lst1)) (cons (car lst1) (class1 fn (cdr lst1))))  
  (#t (class1 fn (cdr lst1))))) 
(define (class2 fn lst2)
(cond 
  ((null? lst2) lst2)
  ((fn (car lst2)) (class2 fn (cdr lst2)))  
  (#t (cons (car lst2) (class2 fn (cdr lst2)))))) 
(define (classify fn lst)
    (cons (class1 fn lst) (cons (class2 fn lst) '())))

(displayln (classify even? '(7 2 3 5 8)))
(displayln (classify integer? '(3.0 -5.2 8 16 99.7)))
(displayln (classify real? '()))

;is-member?
(displayln "\nis-member?")

(define (is-member? atom ismem) 
  (if (null? ismem) #f
  (if (equal? atom (car ismem)) #t
      (is-member? atom (cdr ismem)))))

(displayln (is-member? 6 ' (4 8 6 2 1)))
(displayln (is-member? 7 '(4 8 6 2 1)))
(displayln (is-member? "foo" '(4 5 #f "foo" a)))
(displayln (is-member? '(3 4) '(4 5 #f "foo" (3 4))))

;my-sorted?
(displayln "\nmy-sorted?")

(define (my-sorted? ms1 ms2)
  (if (<= (length ms2) 1) #t
  (if (ms1 (car ms2) (car (cdr ms2)))
      (my-sorted? ms1 (cdr ms2))#f)))

(displayln (my-sorted? < '(2 5 6 9 11 34)))
(displayln (my-sorted? < '(7 25 4 15 11 34)))
(displayln (my-sorted? string<? '("alpha" "beta" "gamma")))
(displayln (my-sorted? string<? '("john" "zack" "bob")))

;my-flatten
(displayln "\nmy-flatten")

(define-values (my-flatten)         
(lambda (notflat)               
(if (null? notflat)           
  (let-values () '())      
(if (pair? notflat)         
  (let-values()          
  (append                     
    (my-flatten (car notflat))   
    (my-flatten (cdr notflat)))) 
    (let-values () (list notflat))))))   

(displayln (my-flatten '(1)))
(displayln (my-flatten '((1 2) 3)))
(displayln (my-flatten '(((4 3) 6) ((7 2 9) (5 1)))))

;upper-threshold
(displayln "\nupper-threshold")

(define (upper-threshold lst threshold)
(cond ((null? lst) '()) 
((< (car lst) threshold) (cons (car lst) (upper-threshold (cdr lst) threshold))) 
(else (upper-threshold (cdr lst) threshold)))) 

(displayln (upper-threshold '(3 6.2 7 2 9 5.3 1) 6))
(displayln (upper-threshold '(1 2 3 4 5) 4))
(displayln (upper-threshold '(4 8 5 6 7) 6.1))
(displayln (upper-threshold '(8 3 5 7) 2))

;my-list-ref
(displayln "\nmy-list-ref")

(define (my-list-ref lst index)
(cond ((null? lst) 'ERROR:Index-out-of-bounds) 
((= index 0) (car lst)) 
(else (my-list-ref (cdr lst) (- index 1))))) 

(displayln (my-list-ref '(4 7 9) 0))
(displayln (my-list-ref '(4 7 9) 1))
(displayln (my-list-ref '(4 7 9) 3))

;deep-reverse
(displayln "\ndeep-reverse")

(define (deep-reverse x)
  (if (list? x)
      (reverse (map deep-reverse x)) x))

(displayln (deep-reverse '(((4 3) 6)((7 2 9)(5 1)))))
(displayln (deep-reverse '((1 2) 3)))
(displayln (deep-reverse '((4 5))))
(displayln (deep-reverse '(3 6 9 12)))

