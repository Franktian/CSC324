#lang racket

; 4. Write a function to determine the length of a list.
(define (length-of-list lst)
  (if (empty? lst)
      0
      (+ 1 (length-of-list (rest lst)))))

; 5. Write a function to determine if a given item appears in a list.
(define (item-in-list lst item)
  (if (empty? lst)
      #f
      (or (equal? (first lst) item) (item-in-list (rest lst) item))))

; 6. Write a function to determine the number of duplicates in a list.
(define (num-duplicates lst)
  (cond
    [(empty? lst) 0]
    [(member (first lst) (rest lst))
     (+ 1 (num-duplicates (rest lst)))]
    [else (num-duplicates (rest lst))]))

; 7. Write a function to remove all duplicates from a list.
(define (remove-duplicates lst)
  (cond
    [(empty? lst) '()]
    [(member (first lst) (rest lst))
     (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))

; Look up “rest” arguments in Racket, which allow you to define functions
; that take in an arbitrary number of arguments, then
; implement a function ($$ f x1 ... xn), which is equivalent to (f x1
; ... xn)
(define $$
  (lambda arguments
    (apply (first arguments) (rest arguments))))

; Write a function that takes a single argument x, and returns a new
; function which takes a list and checks whether x is in that list or not
(define (check-in-list x)
  (lambda (lst) (member x lst)))

; Write a function that takes a unary function and a positive integer n,
; and returns a new unary function that applies the function to its argument
; n times
(define (apply-n-times f n)
  (lambda (x)
    (if (= n 0)
        x
        (f ((apply-n-times f (- n 1)) x)))))

(define 2-sqr (apply-n-times sqr 2))

; Write a function flip that takes a binary function f, and returns a
; new binary function g such that (g x y) = (f y x) for all valid
; arguments x and y.
(define (flip f)
  (lambda (x y) (f y x)))

(define flip-minus (flip -))

; Write a function that takes two unary functions f and g, and returns
; a new unary function that always returns the max of f ang g applied
; to its argument
(define (max-unary f g)
  (lambda (x) (max (apply f (list x)) (apply g (list x)))))

; Write a function num-evens that takes a list of integers and return
; the number of even numbers in that list
(define (num-evens lst)
  (if (empty? lst)
      0
      (if (even? (first lst))
                 (+ 1 (num-evens (rest lst)))
                 (num-evens (rest lst)))))

; Write a function num-strings which takes a list of arbitrary values,
; and returns the number of strings in that list
(define (num-strings lst)
  (if (empty? lst)
      0
      (if (string? (first lst))
                 (+ 1 (num-strings (rest lst)))
                 (num-strings (rest lst)))))

(define (count-pred f lst)
  (if (empty? lst)
      0
      (if (f (first lst))
          (+ 1 (count-pred f (rest lst)))
          (count-pred f (rest lst)))))

; Write a function add-to-all that takes a list of lists and an item, and appends
; that item to the front of each list
; > (add-to-all '((1 2 3) () (4) (5 6)) 10)
; '((10 1 2 3) (10) (10 4) (10 5 6))
; (define (add-to-all lst item)
;  (map (lambda (l) (append (list item) l)) lst))

(define (add-to-all lst item)
  (if (empty? lst)
      null
      (cons (cons item (first lst)) (add-to-all (rest lst) item))))

; Write a function subsets that takes a list (representing a set) and
;returns a list of lists containg all subsets of its input.
; Hint: use add-to-all

; Modify subsets to additionally take an optional non-negative integer
; argument k that, if specified, makes subsets only return the subsets
; of size k, if k is not provided, return all the subsets of the input
; list, as before
(define (subsets sets [k -1])
  (if (or (equal? k -1) (> k (length sets)))
      (subsets-helper sets)
      (filter (lambda (l) (equal? k (length l))) (subsets-helper sets))))

(define (subsets-helper sets)
  (if (empty? sets)
      '(())
      (append (subsets (rest sets))
              (add-to-all (subsets (rest sets)) (first sets)))))

; Macro
(define-syntax list-comp
  (syntax-rules (for in)
    [(list-comp <expr> for <var> in <list>)
     (map (lambda (<var>) <expr>) <list>)]

    [(list-comp <expr> for <var> in <list> if <cond>)
     (map (lambda (<var>) <expr>)
          (filter (lambda (<var>) <cond>)
                  <list>))]))

; Hygienic macros
(define-syntax-rule (make-adder x)
  (lambda (y) (+ y y)))

(define y 10)
(define add-10 (make-adder y))

; Macros with ellipses
(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond [else <val>]) <val>]
    [(my-cond [<test> <val>] <next-pair> ...)
     (if <test> <val> (my-cond <next-pair> ...))]))

; Exercise
(define-syntax my-mac
  (syntax-rules ()
    [(my-mac x) (list x x)]))

; Class-based object oriented programming
(define-syntax class
  (syntax-rules ()
    [(class <class-name>
       ; This ellipsis is paired with <attr>
       (<attr> ...)
       ; This ellipsis is paired with <arg>
       [(<method-name> <arg> ...) <body>]
       ; This ellipsis is paired with the whole previous pattern
       ...)
     (define (<class-name> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (symbol->string (quote <attr>))) <attr>]
               ...
               ; This is the new part
               [(equal? msg (symbol->string (quote <method-name>)))
                (lambda (<arg> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]))

(class Point (x y)
  [(distance other-point)
   (let ([dx (- x (other-point "x"))]
         [dy (- y (other-point "y"))])
     (sqrt (+ (* dx dx) (* dy dy))))])

;神代码， 卧槽，吊炸了
(define p (Point 3 2))
(define p2
  (lambda (msg)
    (cond [(equal? msg "sum-coords")
           (+ (p2 "x") (p2 "y"))]
          [else (p msg)])))