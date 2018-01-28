#lang racket
(require "../utilities.rkt")
(provide uniquify)

(define (uniquify-helper alist)
  (lambda (e)
    (match e
           [(? symbol?) (car (lookup e alist))]
           [(? integer?) e]
           [(? boolean?) e]
           [`(let ([,x ,e]) ,body) 
             (let ([newsym (gensym x)])
               `(let ([,newsym ,((uniquify-helper alist) e)]) ,((uniquify-helper (cons (list x newsym) alist)) body)))]
           [`(program ,type ,e)
             `(program ,type ,((uniquify-helper alist) e))]
           [`(,op ,es ...)
             `(,op ,@(map (uniquify-helper alist) es))]
           [`(read)
             `(read)]
           )))
(define uniquify
  (lambda (e)
    ((uniquify-helper '()) e)))


