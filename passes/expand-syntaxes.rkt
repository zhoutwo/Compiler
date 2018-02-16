#lang racket
(require "../utilities.rkt")
(require "replace-symbols.rkt")
(provide expand-syntaxes)

(define (expand-syntaxes e)
  (match e
    [`(program ,syns ,defs ... ,body)
        `(program ,@(map (expand-syntaxes-general-helper syns) defs) ,((expand-syntaxes-general-helper syns) body))]))

(define (expand-syntaxes-general-helper syns)
  (lambda (e)
    (define last-expanded #t)
    (define time 0)
    (define result e)
    (while last-expanded
      (define-values (newE expanded) ((expand-syntaxes-helper syns time) result))
      (set! result newE)
      (set! time (+ 1 time))
      (set! last-expanded expanded))
    result))

(define (expand-syntaxes-helper syns time)
  (lambda (e)
    (define synnames (map cadr syns))
    (define specialmapping (map caddr syns))
    (define argmapping (map cadddr syns))
    (define bodymapping (map (lambda (syn) (cadr (cdddr syn))) syns))
    (match e
        [(? fixnum?) (values e #f)]
        [(? boolean?) (values e #f)]
        [(? symbol?) (values (box (cons e time)) #f)]
        [(? box?) (values e #f)]
        [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
            (define-values (newXs xExpanded) (map2 (expand-syntaxes-helper syns time) xs))
            (define-values (newbody expanded) ((expand-syntaxes-helper syns time) body))
            (values `(lambda: ,(map (lambda (x t) `(,x : ,t)) newXs Ts) : ,rT ,newbody) expanded)]
        [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
            (define-values (newXs xExpanded) (map2 (expand-syntaxes-helper syns time) xs))
            (define-values (newbody expanded) ((expand-syntaxes-helper syns time) body))
            (values `(define (,f ,@(map (lambda (x t) `(,x : ,t)) newXs ps)) : ,rt ,newbody) expanded)]
        [`(let ([,(app (expand-syntaxes-helper syns time) x xExpanded) ,(app (expand-syntaxes-helper syns time) e eExpanded)]) ,body)
            (define-values (newBody bodyExpanded) ((expand-syntaxes-helper syns time) body))
            (values `(let ([,x ,e]) ,newBody) (or eExpanded bodyExpanded))]
        [`(not ,(app (expand-syntaxes-helper syns time) e t))
            (values `(not ,e) t)]
        [`(and ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(and ,e1 ,e2) (or t1 t2))]
        [`(if ,(app (expand-syntaxes-helper syns time) e-cnd t-cnd) ,(app (expand-syntaxes-helper syns time) e-thn t-thn) ,(app (expand-syntaxes-helper syns time) e-els t-els))
            (values `(if ,e-cnd ,e-thn ,e-els) (or t-cnd t-thn t-els))]
        [`(- ,(app (expand-syntaxes-helper syns time) e t))
            (values `(- ,e) (or t))]
        [`(set! ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(set! ,e1 ,e2) (or t1 t2))]
        [`(+ ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(+ ,e1 ,e2) (or t1 t2))]
        [`(> ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(> ,e1 ,e2) (or t1 t2))]
        [`(>= ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(>= ,e1 ,e2) (or t1 t2))]
        [`(< ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(< ,e1 ,e2) (or t1 t2))]
        [`(<= ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(<= ,e1 ,e2) (or t1 t2))]
        [`(read) (values e #f)]
        [`(void) (values e #f)]
        [`(vector ,(app (expand-syntaxes-helper syns time) e* t*) ...)
            (values `(vector ,@e*) (ormap (lambda (a) a) t*))]
        [`(vector-ref ,(app (expand-syntaxes-helper syns time) e1 t1) ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(vector-ref ,e1 ,e2) (or t1 t2))]
        [`(vector-set! ,(app (expand-syntaxes-helper syns time) e1 t1)
                       ,(app (expand-syntaxes-helper syns time) e2 t2)
                       ,(app (expand-syntaxes-helper syns time) e3 t3))
            (values `(vector-set! ,e1 ,e2 ,e3) (or t1 t2 t3))]
        [`(eq? ,(app (expand-syntaxes-helper syns time) e1 t1)
               ,(app (expand-syntaxes-helper syns time) e2 t2))
            (values `(eq? ,e1 ,e2) (or t1 t2))]
        [`(,op ,exps ...)
            (if (and (symbol? op) (member op synnames))
                (let ([synargs (lookup op argmapping)] ; Macro
                      [synbody (lookup op bodymapping)]
                      [specials (lookup op specialmapping)])
                  (if (not (equal? (length exps) (length synargs)))
                      (error `expand-syntaxes-helper "number of syntax args don't match: ~a, ~a" exps synargs)
                      (let-values ([(es ts) (map2 (expand-syntaxes-helper syns time) exps)])
                        (define synarg-arg-map (map cons synargs es))
                        (define special-arg-map (map (lambda (s) (cons s (box (cons s time)))) specials))
                        (define total-arg-map (append synarg-arg-map special-arg-map))
                        (values ((replace-symbols total-arg-map) synbody) #t))))
                (let-values ([(newop opexpanded) ((expand-syntaxes-helper syns time) op)]
                             [(es ts) (map2 (expand-syntaxes-helper syns time) exps)]) ; Non-macro
                    (values `(,newop ,@es) (or opexpanded (ormap (lambda (a) a) ts)))))])))
