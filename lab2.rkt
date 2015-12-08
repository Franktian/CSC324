#lang racket
; 1. Define a function make-counter, which takes as input a predicate, and returns a function that
; takes a list and returns the number of elements in the list satisfying that predicate
(define (make-counter pred)
  (lambda (lst)
    (length (filter pred lst))))

; 2. Show how to use make-counter to define num-evens in a very concise way
(define num-evens (make-counter even?))

; 3. Show how to use make-counter to count the number of elements greater
; than 5 in a list, without defining any intermediate names
(define greater-five (make-counter (lambda (x) (> x 5))))

; 4. In the same vein, write a function that takes a list of predicates, and returns a function
; that takes a list and returns the number of elements in the list satisfying all of the
; predicates
; Hint: You can actually define such a function using make-counter, if you're smart about what
; predicate to pass in.
(define (list-filter pred-lst)
  (lambda (lst)
    (length (filter-helper pred-lst lst))))

(define (filter-helper pred-lst lst)
  (if (empty? pred-lst)
      lst
      (filter-helper (rest pred-lst) (filter (first pred-lst) lst))))
