#lang racket

; Exercises from the text book page 27
(define (length-of-list lst)
  (if (empty? lst)
      0
      (+ 1 (length-of-list (rest lst)))))

(define (item-in-list lst item)
  (if (empty? lst)
      #f
      (or (equal? (first lst) item) (item-in-list (rest lst) item))))

(define (remove-duplicates lst)
  (cond
    [(empty? lst) '()]
    [(member (first lst) (rest lst))
     (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))
