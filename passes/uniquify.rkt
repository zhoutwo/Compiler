#lang racket
(require "../utilities.rkt")
(provide uniquify)

(define (uniquify-helper alist)
  (lambda (rawExp)
    (match rawExp
           [`(has-type ,e ,t)
             (match e
                    [(? symbol?) `(has-type ,(lookup e alist) ,t)]
                    [(? integer?) rawExp]
                    [(? boolean?) rawExp]
                    [`(let ([,x ,e]) ,body) 
                      (let ([newsym (gensym x)])
                        `(has-type (let ([,newsym ,((uniquify-helper alist) e)]) ,((uniquify-helper (cons (cons x newsym) alist)) body)) ,t))]
                    [`(+ ,arg1 ,arg2)
                      `(has-type (+ ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(- ,arg1)
                      `(has-type (- ,((uniquify-helper alist) arg1)) ,t)]
                    [`(eq? ,arg1 ,arg2)
                      `(has-type (eq? ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(> ,arg1 ,arg2)
                      `(has-type (> ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(>= ,arg1 ,arg2)
                      `(has-type (>= ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(< ,arg1 ,arg2)
                      `(has-type (< ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(<= ,arg1 ,arg2)
                      `(has-type (<= ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(and ,arg1 ,arg2)
                      `(has-type (and ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(not ,arg1)
                      `(has-type (not ,((uniquify-helper alist) arg1)) ,t)]
                    [`(vector ,args ...)
                      `(has-type (vector ,@(map (uniquify-helper alist) args)) ,t)]
                    [`(vector-ref ,arg1 ,arg2)
                      `(has-type (vector-ref ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2)) ,t)]
                    [`(vector-set! ,arg1 ,arg2 ,arg3)
                      `(has-type (vector-set! ,((uniquify-helper alist) arg1) ,((uniquify-helper alist) arg2) ,((uniquify-helper alist) arg3)) ,t)]
                    [`(read) rawExp]
                    [`(void) rawExp]
                    [`(allocate ,rest ...) rawExp]
                    [`(global-value ,rest ...) rawExp]
                    [`(collect ,rest ...) rawExp]
                    [`(lambda: ([,xs : ,ps] ...) : ,rt ,body)
                         (map (lambda (x)
                                (set! alist (cons (cons x (gensym x)) alist))) xs)
                         (define newbody ((uniquify-helper alist) body))
                         (define new-xs-ps (map (lambda (x p)
                                                    `(,(lookup x alist) : ,p)) xs ps))
                         `(has-type (lambda: ,new-xs-ps : ,rt ,newbody) ,t)]
                    [`(if ,cond ,thn ,els)
                      `(has-type (if ,((uniquify-helper alist) cond) ,((uniquify-helper alist) thn) ,((uniquify-helper alist) els)) ,t)]
                    [`(,op ,es ...)
                      `(has-type (,((uniquify-helper alist) op) ,@(map (uniquify-helper alist) es)) ,t)])]
           [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
               (define newf (lookup f alist))
               (map (lambda (x)
                      (set! alist (cons (cons x (gensym x)) alist))) xs)
               (define newbody ((uniquify-helper alist) body))
               (define new-xs-ps (map (lambda (x p)
                                          `(,(lookup x alist) : ,p)) xs ps))
               `(define (,newf ,@new-xs-ps) : ,rt ,newbody)]
           [`(program ,type ,defs ,e)
               (define fnames (map caadr defs))
               (map (lambda (fname)
                      (set! alist (cons (cons fname (gensym fname)) alist))) fnames)
               `(program ,type ,(map (uniquify-helper alist) defs) ,((uniquify-helper alist) e))]
             )))

(define uniquify
  (lambda (e)
    ((uniquify-helper '()) e)))
