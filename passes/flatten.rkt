#lang racket
(require "../utilities.rkt")
(provide flatten)

(define flatten-helper
  (lambda (exp)
    (match exp
           [(? symbol?) (values exp null null)]
           [(? integer?) (values exp null null)]
           [(? boolean?) (values exp null null)]
           [`(program ,type ,e)
             (define-values (newExp stms vars) (flatten-helper e))
             (values (list) `(program ,(remove-duplicates vars) ,type ,(append stms (list (list `return newExp)))) (list))
             ]
           [`(let ([,x ,exp]) ,body)
             (define-values (prevExp stms vars) (flatten-helper exp))
             (define-values (bodyExp bodystms bodyvars) (flatten-helper body))
             (values bodyExp (append stms (list (list `assign x prevExp)) bodystms) (append (cons x vars) bodyvars))
             ]
           [`(read) (let ([newExp (gensym "tmp")]) (values newExp (list (list `assign newExp `(read))) (list newExp)))]
           [`(if ,cnd ,thn ,els)
             (let ([newExp (gensym "if")])
               (define-values (prev-exp prev-stms prev-vars) (flatten-helper cnd))
               (define-values (thn-exp thn-stms thn-vars) (flatten-helper thn))
               (define-values (els-exp els-stms els-vars) (flatten-helper els))
               (values newExp
                       (append prev-stms
                       (list (list `if `(eq? #t ,prev-exp)
                             (append thn-stms (list (list `assign newExp thn-exp)))
                             (append els-stms (list (list `assign newExp els-exp))))))
                       (cons newExp (append prev-vars thn-vars els-vars))))]
           [`(and ,exp1 ,exp2)
             (flatten-helper `(if ,exp1 ,exp2 ,#f))]
           [`(,op ,exp)
             (let ([newExp (gensym "tmp")])
               (define-values (prevExp stms vars) (flatten-helper exp))
               (values newExp (append stms (list (list `assign newExp `(,op ,prevExp)))) (cons newExp vars)))
             ]
           [`(,op ,exp1 ,exp2) 
             (let ([newExp (gensym "tmp")])
               (define-values (prevExp1 stms1 vars1) (flatten-helper exp1))
               (define-values (prevExp2 stms2 vars2) (flatten-helper exp2))
               (values newExp (append stms1 stms2 (list (list `assign newExp `(,op ,prevExp1 ,prevExp2)))) (cons newExp (append vars1 vars2))))
             ]
           )))

(define flatten 
  (lambda (exp)
    (define-values (tmp stms vars) (flatten-helper exp))
    stms))


