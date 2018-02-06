#lang racket
(require "../utilities.rkt")
(provide reveal-functions)

(define (reveal-functions-helper fnames)
  (lambda (rawExp)
    (match rawExp
      [(? symbol?) (if (member rawExp fnames)
                       `(function-ref ,rawExp)
                       rawExp)]
      [(? integer?) rawExp]
      [(? boolean?) rawExp]
      [`(has-type ,e ,t)
        (match e
          [(? symbol?) (if (member e fnames)
                           `(has-type (function-ref ,e) ,t)
                           rawExp)]
          [(? integer?) rawExp]
          [(? boolean?) rawExp]
          [`(let ([,x ,e]) ,body)
              `(has-type (let ([,((reveal-functions-helper fnames) x) ,((reveal-functions-helper fnames) e)])
                              ,((reveal-functions-helper fnames) body)) ,t)]
          [`(+ ,arg1 ,arg2)
            `(has-type (+ ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(- ,arg1)
            `(has-type (- ,((reveal-functions-helper fnames) arg1)) ,t)]
          [`(eq? ,arg1 ,arg2)
            `(has-type (eq? ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(> ,arg1 ,arg2)
            `(has-type (> ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(>= ,arg1 ,arg2)
            `(has-type (>= ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(< ,arg1 ,arg2)
            `(has-type (< ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(<= ,arg1 ,arg2)
            `(has-type (<= ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(and ,arg1 ,arg2)
            `(has-type (and ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(not ,arg1)
            `(has-type (not ,((reveal-functions-helper fnames) arg1)) ,t)]
          [`(vector ,args ...)
            `(has-type (vector ,@(map (reveal-functions-helper fnames) args)) ,t)]
          [`(vector-ref ,arg1 ,arg2)
            `(has-type (vector-ref ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2)) ,t)]
          [`(vector-set! ,arg1 ,arg2 ,arg3)
            `(has-type (vector-set! ,((reveal-functions-helper fnames) arg1) ,((reveal-functions-helper fnames) arg2) ,((reveal-functions-helper fnames) arg3)) ,t)]
          [`(read) rawExp]
          [`(void) rawExp]
          [`(allocate ,rest ...) rawExp]
          [`(global-value ,rest ...) rawExp]
          [`(collect ,rest ...) rawExp]
          [`(if ,cond ,thn ,els)
            `(has-type (if ,((reveal-functions-helper fnames) cond) ,((reveal-functions-helper fnames) thn) ,((reveal-functions-helper fnames) els)) ,t)]
          [`(,op ,es ...)
            `(has-type (app ,((reveal-functions-helper fnames) op) ,@(map (reveal-functions-helper fnames) es)) ,t)])]
      [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
          `(define ,(cadr rawExp) : ,rt ,((reveal-functions-helper fnames) body))])))

(define reveal-functions
  (lambda (e)
    (match e
       [`(program ,type ,defs ,e)
         (define fnames (map caadr defs))
         `(program ,type (defines ,@(map (reveal-functions-helper fnames) defs)) ,((reveal-functions-helper fnames) e))])))
