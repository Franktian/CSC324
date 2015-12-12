#lang racket
(define global-cont (void))

(+ (let/cc cont
     ; This *mutates* global-cont by assigning it the
     ; value of cont.
     (set! global-cont cont)
     (* 3 4))
   (first '(1 2 3)))

;Write a macro that behaves like let/cc,except it binds
; the current continutation to a global name using set!
;
;
;
;
(define-syntax set!/cc
  (syntax-rules ()
    [(set!cc <global-cont> <expr>)
     (let/cc cont
       (set! <global-cont> cont)
       <expr>)]))

(define yoyo (void))
(+ 3 (set!/cc yoyo (* 2 6)))

; Define a name raise, which, when called on a string,
; causes the program to halt and return that string.
; (Hint: raise can be a very simple continuation.
; This is more an exercise in wrapping your head around
; this concept than it is in writing code.)
(define raise (void))
(let/cc cont (set! raise cont))
