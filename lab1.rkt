#lang racket

(define (num-evens lst)
  (count-pred even? lst))

(define (num-strings lst)
  (count-pred string? lst))

(define (count-pred pred lst)
  (if (empty? lst)
      0
      (if (pred (first lst))
          (+ 1 (count-pred pred (rest lst)))
          (count-pred pred (rest lst)))))

; Write a function add-to-all that takes a list of lists and an item,
; and appends that item to the front of each list.
(define (add-to-all lst item)
  (if (empty? lst)
      '()
      (cons (cons item (first lst)) (add-to-all (rest lst) item))))

; Write a function subsets, that takes a list (representing a set),
; and returns a list of lists containing all subsets of its input.
(define (subsets lst)
  (if (empty? lst)
      '(())
      (append (subsets (rest lst)) (add-to-all (subsets (rest lst)) (first lst)))))