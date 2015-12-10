#lang racket
(define x 20)
(define y "Hi")

(define (maker-3 n y)
  (let ([y "byeee"])
    (lambda (m) (+ n m (string-length y)))))

; A function that returns a higher-order function
(define (maker-4 n)
  (lambda (f x)
    (f (+ x n))))

; A function in a let, with shadowing
(define let-ex
  (let* ([x 15]
         [y (- x 6)])
    (lambda (y) (- x y))))

; Lexical vs. dynamic scope
(define const 10)
(define (let-ex-3 y)
  (lambda (z) (+ y (- z const))))

; f1 takes an argument x, and returns a function that takes 0 arguments,
; and returns x when called.
(define (f1 x)
  (lambda () x))

;f2 takes an argument x, and returns a function that takes 1 argument, and:
; if the argument is "equal?" to "x", it returns the value of x
; otherwise, it returns "error"
(define (f2 x)
  (lambda (y)
    (if (equal? "x" y)
        x
        "error")))

; f3 takes three arguments x, y, and z (in that order),
; and returns a function that takes 1 argument, and:
; if the argument is equal to "x", "y", or "z",
; it returns the value of x, y, or z, respectively
;
; otherwise, it returns "error"
(define (f3 x y z)
  (lambda (arg)
    (cond [(equal? arg "x") x]
          [(equal? arg "y") y]
          [(equal? arg "z") z]
          [else "error"])))

; f4 takes two numeric arguments x and y,
; and returns a function that takes 1 argument, and:
; if the argument is equal to "add", it returns the value of x + y
; if the argument is equal to "multiply", it returns the value of x * y
; otherwise, it returns "error"
(define (f4 x y)
  (lambda (arg)
    (cond [(equal? arg "add") (+ x y)]
          [(equal? arg "multiply") (* x y)]
          [else "error"])))

; Macros
(define-syntax my-or
  (syntax-rules ()
    [(my-or p q)
     (if p #t q)]))

(define-syntax my-and
  (syntax-rules ()
    [(my-and p q)
     (if p
         (if q
             #t
             #f)
         #f)]))

#| Lab 3: Higher-order list-ref

Complete each of the following functions.
You're encourage to write your own tests for these ones.
|#


#|
(list-ref-one i)
  i: a non-negative integer

  Returns a function which takes a list and returns
  the item in the list at index 'i', or "ERROR" if
  'i' is out of bounds.
> ((list-ref-one 1) '("a" "b" "c" "d" "e"))
"b"
|#

(define (list-ref-one i)
  (lambda (lst)
    (if (>= i (length lst))
        "ERROR"
        (list-ref lst i))))


#|
(list-ref-many is)
  is: a list of non-negative integers

  Returns a function which takes a list and returns
  the items in the list at the indexes specified by 'is',
  in the order that 'is' is given; or, it returns "ERROR"
  if any one of the items in 'is' is out of bounds.
> ((list-ref-many '(1 2 4 0 1)) '("a" "b" "c" "d" "e"))
'("b" "c" "e" "a" "b")
|#
(define (list-ref-many is)
  (lambda (lst)
    (map (lambda (i)
           ((list-ref-one i) lst)) is)))

#|
(list-ref-rec is)
  is: a possibly nested list of non-negative integers.

  Same as list-ref-many, except now 'is' can contain
  nested lists instead of just integers.

  It may be easier if you define this function to also
  work for single integers in addition to nested lists.

> ((list-ref-rec '(1 2 (3 (4 0)) 1)) '("a" "b" "c" "d" "e"))
'("b" "c" ("d" ("e" "a")) "b")
|#
(define (list-ref-rec is)
  (lambda (lst)
    (map (lambda (i)
           (if (list? i)
               ((list-ref-rec i) lst)
               ((list-ref-one i) lst))) is)))


#|
(list-ref-data data)
  data: a possibly nested list containing arbitrary data.

  Same as list-ref-rec, except any time an atom which is
  not an index or in range is encountered, return that atom
  rather than returning (void).

> ((list-ref-data '(1 "heeey" (3 (4 -10.5)) 7)) '("a" "b" "c" "d" "e"))
'("b" "heeey" ("d" ("e" -10.5)) 7)
|#
(define (list-ref-data is)
  (lambda (lst)
    (map (lambda (i)
           (cond
             [(list? i) ((list-ref-data i) lst)]
             [(and (integer? i) (< i (length lst))) ((list-ref-one i) lst)]
             [else i])) is)))
