#| Exercise 1 - Getting started with Racket (due September 26, noon)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement the two functions below to get some experience programming in Racket.
You may use either explicit recursion, or higher-order list functions.
(For extra practice, try both!)
|#
#lang racket

; This is the *only* extra module you can use. Look up the documentation
; at http://docs.racket-lang.org/reference/strings.html
(require racket/string)


;-----------------------------------------------------------
; This line exports the required functions. DON'T CHANGE IT.
(provide filter-by-word sublist)
;-----------------------------------------------------------


#|
(filter-by-word strings word)
  strings: a list of strings
  word: a non-empty string

  Returns a list containing all the strings in 'strings' which have 'word'
  as a word. (Note that a "word" is not the same as a substring; it is
  a sequence of non-whitespace characters separated from the rest of the string
  by whitespace.) The equality checks should be case-sensitive.

  The strings must appear in the same order they appear in 'strings'.

> (filter-by-word '("Hello world" "Hello moon" "Goodbye world") "world")
'("Hello world" "Goodbye world")
> (filter-by-word '("Hello world" "whyhellothere" "ello") "hello")
'()

HINT: use the "string-split" function from racket/string.
|#
; Feel free to change this signature to use the shorthand for defining functions
; (define (filter-by-word ...) ...))
(define (filter-by-word strings word)
  (cond
    [(empty? strings) '()]
    [(member word (string-split (first strings)))
     (cons (first strings)
           (filter-by-word (rest strings) word))]
    [else (filter-by-word (rest strings) word)]))

(define (filter-by-word-higher-order strings word)
  (filter (lambda (l)
            (if (member word (string-split l)) #t #f))
          strings))

#|
(sublist sub lst)
  sub: a list
  lst: a list

  Checks whether 'sub' is a sublist of 'lst' (i.e., all the items in
  'sub' appear consecutively in 'lst').

  If 'sub' is a sublist of 'lst', this function returns the *index*
  of the first element of the first occurrence of 'sub' within 'lst'.
  Otherwise, this function returns #f.

  Note that the empty list is a sublist of every list, and it first
  occurs at index 0.

> (sublist '(30 40) '(10 20 30 40 50))
2
> (sublist '(20 30) '(10 20 30 20 30 40 50))
1
> (sublist '(1 2 3) '(5 4 3 2 1))
#f
|#

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