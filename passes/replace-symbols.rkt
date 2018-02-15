#lang racket
(require "../utilities.rkt")
(provide replace-symbols)

(define (replace-symbols old-new-map)
  (define to-replaces (map car old-new-map))
  (lambda (e)
    (match e
        [(? fixnum?) e]
        [(? boolean?) e]
        [(? symbol?) (if (member e to-replaces)
                         (lookup e old-new-map)
                         e)]
        [(? box?) (if (member e to-replaces)
                       (lookup e old-new-map)
                       (car (unbox e)))]
        [`(lambda: ([,(app (replace-symbols old-new-map) xs) : ,Ts] ...) : ,rT ,body)
            (set! Ts (map (replace-symbols old-new-map) Ts))
            (define newbody ((replace-symbols old-new-map) body))
            `(lambda: ,(map (lambda (x t) `(,x : ,t)) xs Ts) : ,rT ,newbody)]
        [`(define (,f [,(app (replace-symbols old-new-map) xs) : ,ps] ...) : ,rt ,body)
            `(define (,f ,@(map (lambda (x p) `(,x : ,p)) xs ps)) : ,rt ,((replace-symbols old-new-map) body))]
        [`(let ([,(app (replace-symbols old-new-map) x) ,(app (replace-symbols old-new-map) e)]) ,(app (replace-symbols old-new-map) body))
            `(let ([,x ,e]) ,body)]
        [`(not ,(app (replace-symbols old-new-map) e))
            `(not ,e)]
        [`(and ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(and ,e1 ,e2)]
        [`(if ,(app (replace-symbols old-new-map) e-cnd) ,(app (replace-symbols old-new-map) e-thn) ,(app (replace-symbols old-new-map) e-els))
            `(if ,e-cnd ,e-thn ,e-els)]
        [`(- ,(app (replace-symbols old-new-map) e))
            `(- ,e)]
        [`(set! ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(set! ,e1 ,e2)]
        [`(+ ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(+ ,e1 ,e2)]
        [`(> ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(> ,e1 ,e2)]
        [`(>= ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(>= ,e1 ,e2)]
        [`(< ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(< ,e1 ,e2)]
        [`(<= ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(<= ,e1 ,e2)]
        [`(read) e]
        [`(void) e]
        [`(vector ,(app (replace-symbols old-new-map) e*) ...)
            `(vector ,@e*)]
        [`(vector-ref ,(app (replace-symbols old-new-map) e1) ,(app (replace-symbols old-new-map) e2))
            `(vector-ref ,e1 ,e2)]
        [`(vector-set! ,(app (replace-symbols old-new-map) e1)
                       ,(app (replace-symbols old-new-map) e2)
                       ,(app (replace-symbols old-new-map) e3))
            `(vector-set! ,e1 ,e2 ,e3)]
        [`(eq? ,(app (replace-symbols old-new-map) e1)
               ,(app (replace-symbols old-new-map) e2))
            `(eq? ,e1 ,e2)]
        [`(,(app (replace-symbols old-new-map) op) ,(app (replace-symbols old-new-map) es) ...)
            `(,op ,@es)])))
