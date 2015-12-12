#lang racket

(require "choice.rkt")

; Define a function that takes a number, and chooses
; to either add 1 or 2 to it.
(define (f n)
  (+ n (-< 1 2)))

; Define a function that takes a number, and chooses
; to either add 2 to it or multiply it by 10.
(define (g n)
  (
   (-< (lambda (x) (+ x 2)) (lambda (y) (* y 10))) n))

; Define a function that takes a number n, and chooses
; a number between 0 and n, inclusive.
(define (z n)
  (if (equal? n 0)
      (-< 0)
      (-< n (z (- n 1)))))

; Define a function that takes two numbers n and m,
; and chooses a number between n and m, inclusive.
(define (a n m)
  (if (equal? (- m n) 0)
      n
      (-< m (a n (- m 1)))))