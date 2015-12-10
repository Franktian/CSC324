#| Exercise 3 - More Higher-Order Functions (due October 10, noon)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for/*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement the functions below. Note that these functions both
return new functions. Don't make your code too complex; even though
the ideas may be new, each function can be implemented in just a
few lines of code, same as before.
|#
#lang racket
(require racket/string)

(provide make-splitter curry-2 curry-n)

#|
(make-splitter splitter)
  splitter: a string

  Returns a function f that takes a string s and implements the following
  behaviour:
    1. If 'splitter' is *not* a substring of the 's' list,
       return #f.
    2. Else, return a list of two elements (list before after), where
       'before' is the list of words in 's' *before* the first occurrence
       of 'splitter', and 'after' is the list of words in 's' *after* the
       first occurrence of 'splitter'. Note that neither 'before' nor
       'after' include the words from the first occurrence of 'splitter'.

> (define f (make-splitter "hello world"))
> (f "this is a hello world kind of party")
'(("this" "is" "a") ("kind" "of" "party"))
> (f "this is a hello not world kind of party")
#f

**You may assume that both 'splitter' and any input 's' to f are strings
in which words are only separated by one space, and so "string-split" doesn't
lose any information about the strings.**

Hint: our goal is to have you build upon your work from Exercise 1.
You are encouraged to reuse some of your work there as helper function(s)
in this module.
|#
(define (make-splitter splitter)
  (lambda (s) (if (sublist (string-split splitter) (string-split s))
                  (list
                   (take (string-split s) (sublist (string-split splitter) (string-split s)))
                   (list-tail (string-split s) (+ (sublist (string-split splitter) (string-split s)) (length (string-split splitter))) ))
                  #f)))

(define (sublist sub lst)
  (cond
    [(empty? sub) 0]
    [(empty? lst) #f]
    [(list-equal sub lst) 0]
    [else (if (sublist sub (rest lst))
              (+ (sublist sub (rest lst)) 1)
              (sublist sub (rest lst)))]))

(define (list-equal sub lst)
  (cond
    [(empty? sub) #t]
    [(empty? lst) #f]
    [(equal? (first sub) (first lst))
     (list-equal (rest sub) (rest lst))]
    [else #f]))
#|
(curry-2 f)
  f: a *binary* function
  
  Returns a unary function g that takes an argument x, and returns a
  new unary function h that takes an argument y, such that 
  (h y) is equivalent to (f x y).

> (define (add-2-mult x y) (* (+ 2 x) y))
> (define func (curry-2 add-2-mult))
> ((func 4) 5)
30
|#
(define (curry-2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))


#|
(curry-n f n)
  f: a function that takes 'n' arguments
  n: a positive integer

  A generalization of curry-2, except now 'f' takes 'n' arguments; 
  curry-n returns a unary function g that takes an argument x, 
  and outputs a new function which is the curried version of the function 
  (lambda (x2 ... xn) (f x x2 ... xn)). (In other words, this is a recursively-
  defined function).

> (define (f w x y) (+ w (* x y)))
> (define func (curry-n f 3))
> (((func 4) 6) 10)
64

Note: it is possible to define curry-2 in terms of curry-n:
(define (curry-2 f) (curry-n f 2))
|#
(define (curry-n f n)
  (let loop ((counter n)
             (arguments '()))
    (if (= counter 0)
        (apply f (reverse arguments))
        (lambda (x)
          (loop (- counter 1) (cons x arguments))))))