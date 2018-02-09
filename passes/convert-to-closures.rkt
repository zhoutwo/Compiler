#lang racket
(require "../utilities.rkt")
(provide convert-to-closures)

(define (convert-to-closures-helper env)
  (lambda (rawExp)
    (match rawExp
      [(? symbol?) (pretty-print 'here) (values (if (member rawExp env)
                                                     `(function-ref ,rawExp)
                                                     rawExp) '())]
      [(? integer?) (pretty-print 'here) (values rawExp '())]
      [(? boolean?) (pretty-print 'here) (values rawExp '())]
      [`(has-type ,e ,t)
        (match e
          [(? symbol?) (values (if (member e env)
                                                         `(has-type (function-ref ,e) ,t)
                                                         rawExp) '())]
          [(? integer?) (values rawExp '())]
          [(? boolean?) (values rawExp '())]
          [`(lambda: (,ps ...) : ,rT ,body)
              (define lName (gensym 'lambda))
              (define arg-type-exps (map (lambda (p)
                                      `(has-type ,(car p) ,(caddr p))) ps))
              (define xs (map car ps))
              (define ts (map caddr ps))
              (define closureType `(Vector ,t ,@arg-type-exps))
              (define closureExp `(has-type (vector (has-type ,lName ,t) ,@arg-type-exps) ,closureType))
              (define-values (newBody bodyLambdas) ((convert-to-closures-helper env) body))
              ;(define argNames '())
              (define newBodyWithArgs (let loop ([ats xs]
                                         [ts ts]
                                         [pos 1]
                                         [b newBody])
                                         ;(define nameseed (string-append "arg" (num->string pos)))
                                         (if (null? ps)
                                              b;(has-type clos ,closureType)
                                              `(let ([,(car ats) (has-type (vector-ref clos (has-type ,pos Integer)) (car ts))])
                                                    ,(loop (cdr ps) (+ 1 pos))))))
              (define transformedExp `(define (,lName [clos : ,closureType] ,@ps) ,newBodyWithArgs))
              (values closureExp (cons transformedExp bodyLambdas))]
          [`(function-ref ,e) (values `(has-type (vector (has-type (function-ref ,e) ,t)) (Vector ,t)) `())]
          [`(let ([,x ,e]) ,body)
              (define-values (newE eLambdas) ((convert-to-closures-helper env) e))
              (define-values (newBody bodyLambdas) ((convert-to-closures-helper env) body))
              (values `(has-type (let ([,x ,newE])
                              ,newBody) ,t) (append eLambdas bodyLambdas))]
          [`(+ ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (+ ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(- ,arg1)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (values `(has-type (- ,newArg1) ,t) arg1Lambdas)]
          [`(eq? ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (eq? ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(> ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (> ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(>= ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (>= ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(< ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (< ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(<= ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (<= ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(and ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (and ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(not ,arg1)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (values `(has-type (not ,newArg1) ,t) arg1Lambdas)]
          [`(vector ,args ...)
            (define-values (newArgs argsLambdas) (apply (convert-to-closures-helper env) args))
            (values `(has-type (vector ,@newArgs) ,t) (apply append argsLambdas))]
          [`(vector-ref ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (values `(has-type (vector-ref ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas))]
          [`(vector-set! ,arg1 ,arg2 ,arg3)
            (define-values (newArg1 arg1Lambdas) ((convert-to-closures-helper env) arg1))
            (define-values (newArg2 arg2Lambdas) ((convert-to-closures-helper env) arg2))
            (define-values (newArg3 arg3Lambdas) ((convert-to-closures-helper env) arg3))
            (values `(has-type (vector-set! ,newArg1 ,newArg2 ,newArg3) ,t) (append arg1Lambdas arg2Lambdas arg3Lambdas))]
          [`(if ,cond ,thn ,els)
              (define-values (newCond condLambdas) ((convert-to-closures-helper env) cond))
              (define-values (newThn thnLambdas) ((convert-to-closures-helper env) thn))
              (define-values (newEls elsLambdas) ((convert-to-closures-helper env) els))
              (values `(has-type (if ,newCond ,newThn ,newEls) ,t) (append condLambdas thnLambdas elsLambdas))]
          [`(app ,op ,es ...)
              (define-values (newOp opLambdas) ((convert-to-closures-helper env) op))
              (define-values (newEs esLambdas) (map2 (convert-to-closures-helper env) es))
              (define apptmp (gensym 'closure))
              (define tmptype (caddr newOp))
              (define tmpexp `(has-type ,apptmp ,tmptype))
              ;(define functype (cadr tmptype))
              (define funcreturntyep (list-ref tmptype (- (length tmptype) 1)))
              (values `(has-type (let ([,apptmp ,newOp])
                                      (has-type (app (has-type (vector-ref ,tmpexp (has-type 0 Integer)) ,tmptype) ,tmpexp ,@newEs) ,funcreturntyep)) ,t) (append opLambdas (apply append esLambdas)))]
          [else (values rawExp '())])]
      [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
          (define-values (newBody bodyLambdas) ((convert-to-closures-helper env) body))
          (define Ts (map cons xs ps))
          (define closureType `(Vector (,@ps -> ,rt)))
          (values `(define (,f [clos : ,closureType] ,@(cdadr rawExp)) : ,rt ,newBody) bodyLambdas)])))

(define (convert-to-closures e)
  (match e
    [`(program ,type (defines ,defs ...) ,es)
        (define-values (newDefs defLambdas) (map2 (convert-to-closures-helper '()) defs))
        (define-values (newEs eLambdas) ((convert-to-closures-helper '()) es))
        `(program ,type (defines ,@newDefs ,@(apply append defLambdas) ,@eLambdas) ,newEs)]))
