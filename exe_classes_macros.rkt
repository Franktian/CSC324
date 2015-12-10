#lang racket

(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>]
       ...
       )
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Attribute error"])))]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>) (symbol->string (quote <id>))]))

; Modify the class macro to add support for private attributes
(define-syntax class-private
  (syntax-rules (private)
    [(class <Class> (<attr> ...) (private <p_attr> ...)
       [(<method> <param> ...) <body>]
       ...
       )
     (define (<Class> <attr> ... <p_attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <p_attr>)) "Private attribute"]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Attribute error"])))]))