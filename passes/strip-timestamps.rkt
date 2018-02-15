#lang racket
(require "../utilities.rkt")
(require "replace-symbols.rkt")
(provide strip-timestamps)

(define (strip-timestamps e)
  (define torename (set->list (list->set ((find-bounded '()) e))))
  (define toextract (set->list (list->set ((find-free '()) e))))
  (define renamemap (map (lambda (b) (cons b (gensym (car (unbox b))))) torename))
  (define extractmap (map (lambda (b) (cons b (car (unbox b)))) toextract))
  (define totalmap (append renamemap extractmap))
  `(program ,@(map (replace-symbols totalmap) (cdr e))))

(define (find-bounded env)
  (lambda (e)
    (match e
        [(? fixnum?) '()]
        [(? boolean?) '()]
        [(? symbol?) '()]
        [(? box?)
            (define origsym (car (unbox e)))
            (if (member origsym env)
                (list e)
                '())]
        [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
            (define new-env (append (map (lambda (x) (car (unbox x))) xs) env))
            ((find-bounded new-env) body)]
        [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
            (define new-env (append (map (lambda (x) (car (unbox x))) xs) env))
            ((find-bounded new-env) body)]
        [`(let ([,x ,(app (find-bounded env) e)]) ,body)
            (define new-env (cons (car (unbox x)) env))
            (append e (list x) ((find-bounded new-env) body))]
        [`(program ,bodies ...)
            (apply append (map (find-bounded env) bodies))]
        [`(,oporands ...)
            (apply append (map (find-bounded env) oporands))])))

(define (find-free env)
  (lambda (e)
    (match e
        [(? fixnum?) '()]
        [(? boolean?) '()]
        [(? symbol?) '()]
        [(? box?)
            (define origsym (car (unbox e)))
            (if (member origsym env)
                '()
                (list e))]
        [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
            (define new-env (append (map (lambda (x) (car (unbox x))) xs) env))
            ((find-free new-env) body)]
        [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
            (define new-env (append (map (lambda (x) (car (unbox x))) xs) env))
            ((find-free new-env) body)]
        [`(let ([,x ,(app (find-free env) e)]) ,body)
            (define new-env (cons x env))
            (append e ((find-free new-env) body))]
        [`(program ,bodies ...)
            (apply append (map (find-free env) bodies))]
        [`(,oporands ...)
            (apply append (map (find-free env) oporands))])))
