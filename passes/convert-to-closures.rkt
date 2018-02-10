#lang racket
(require "../utilities.rkt")
(provide convert-to-closures)

(define top-level-defs '())

(define (convert-to-closures-helper boundeds)
  (lambda (rawExp)
    (match rawExp
      [`(has-type ,e ,t)
        (match e
          [(? symbol?) (if (member (cons e t) boundeds)
                           (values rawExp '() '())
                           (values rawExp '() `(,(cons e t))))]
          [(? integer?) (values rawExp '() '())]
          [(? boolean?) (values rawExp '() '())]
          [`(lambda: (,ps ...) : ,rT ,body)
              (define lName (gensym 'lambda))
              (define arg-type-exps (map (lambda (p)
                                      `(has-type ,(car p) ,(caddr p))) ps))
              ;(define xs (map car ps))
              (define ts (map caddr ps))
              (define new-boundeds (append (map (lambda (p)
                                                    (cons (car p) (caddr p))) ps) top-level-defs))
              (define-values (newBody bodyLambdas bodyFrees) ((convert-to-closures-helper new-boundeds) body))
              (set! bodyFrees (set-subtract bodyFrees new-boundeds))
              (define closureType `(Vector ,t ,@(map cdr bodyFrees)))
              (define closureExp `(has-type (vector (has-type (function-ref ,lName) ,t) ,@(map make-has-type-exp (map car bodyFrees) (map cdr bodyFrees))) ,closureType))
              (define newBodyWithArgs (let loop ([ats bodyFrees]
                                         [ts (map cdr bodyFrees)]
                                         [pos 1]
                                         [b newBody])
                                         ;(define nameseed (string-append "arg" (num->string pos)))
                                         (if (null? ats)
                                              b
                                              `(has-type (let ([,(caar ats) (has-type (vector-ref (has-type clos ,closureType) (has-type ,pos Integer)) ,(car ts))])
                                                    ,(loop (cdr ats) (cdr ts) (+ 1 pos) b)) ,rT))))
              (define transformedExp `(define (,lName [clos : ,closureType] ,@ps) : ,rT ,newBodyWithArgs))
              (values closureExp (cons transformedExp bodyLambdas) bodyFrees)]
          [`(function-ref ,e) (values `(has-type (vector (has-type (function-ref ,e) ,t)) (Vector ,t)) `() `())]
          [`(let ([,x ,e]) ,body)
              (define-values (newE eLambdas eFrees) ((convert-to-closures-helper boundeds) e))
              (define new-boundeds (cons (cons x (get-type-from-has-type e)) boundeds))
              (define-values (newBody bodyLambdas bodyFrees) ((convert-to-closures-helper new-boundeds) body))
              (values `(has-type (let ([,x ,newE])
                              ,newBody) ,t) (append eLambdas bodyLambdas) (append eFrees bodyFrees))]
          [`(+ ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (+ ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(- ,arg1)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (values `(has-type (- ,newArg1) ,t) arg1Lambdas arg1Frees)]
          [`(eq? ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (eq? ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(> ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (> ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(>= ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (>= ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(< ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (< ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(<= ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (<= ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(and ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (and ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(not ,arg1)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (values `(has-type (not ,newArg1) ,t) arg1Lambdas arg1Frees)]
          [`(vector ,args ...)
            (define-values (newArgs argsLambdas argsFrees) (map3 (convert-to-closures-helper boundeds) args))
            (set! argsFrees (apply append argsFrees))
            (set! argsFrees (set->list (list->set argsFrees)))
            (values `(has-type (vector ,@newArgs) ,t) (apply append argsLambdas) argsFrees)]
          [`(vector-ref ,arg1 ,arg2)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees))))
            (values `(has-type (vector-ref ,newArg1 ,newArg2) ,t) (append arg1Lambdas arg2Lambdas) argsFrees)]
          [`(vector-set! ,arg1 ,arg2 ,arg3)
            (define-values (newArg1 arg1Lambdas arg1Frees) ((convert-to-closures-helper boundeds) arg1))
            (define-values (newArg2 arg2Lambdas arg2Frees) ((convert-to-closures-helper boundeds) arg2))
            (define-values (newArg3 arg3Lambdas arg3Frees) ((convert-to-closures-helper boundeds) arg3))
            (define argsFrees (set->list (list->set (append arg1Frees arg2Frees arg3Frees))))
            (values `(has-type (vector-set! ,newArg1 ,newArg2 ,newArg3) ,t) (append arg1Lambdas arg2Lambdas arg3Lambdas) argsFrees)]
          [`(if ,cond ,thn ,els)
              (define-values (newCond condLambdas condFrees) ((convert-to-closures-helper boundeds) cond))
              (define-values (newThn thnLambdas thnFrees) ((convert-to-closures-helper boundeds) thn))
              (define-values (newEls elsLambdas elsFrees) ((convert-to-closures-helper boundeds) els))
              (define frees (set->list (list->set (append condFrees thnFrees elsFrees))))
              (values `(has-type (if ,newCond ,newThn ,newEls) ,t) (append condLambdas thnLambdas elsLambdas) frees)]
          [`(app ,op ,es ...)
              (define-values (newOp opLambdas opFrees) ((convert-to-closures-helper boundeds) op))
              (define-values (newEs esLambdas esFrees) (map3 (convert-to-closures-helper boundeds) es))
              (define apptmp (gensym 'closure))
              (define tmptype (caddr newOp))
              (define tmpexp `(has-type ,apptmp ,tmptype))
              ;(define functype (cadr tmptype))
              (define funcreturntyep (list-ref tmptype (- (length tmptype) 1)))
              (define frees (set->list (list->set (append opLambdas (apply append esLambdas)))))
              (values `(has-type (let ([,apptmp ,newOp])
                                      (has-type (app (has-type (vector-ref ,tmpexp (has-type 0 Integer)) ,tmptype) ,tmpexp ,@newEs) ,funcreturntyep)) ,t)
                      (append opLambdas (apply append esLambdas))
                      frees)]
          [else (values rawExp '() '())])]
      [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
          (define Ts (map cons xs ps))
          (define new-boundeds (append Ts boundeds))
          (define-values (newBody bodyLambdas bodyFrees) ((convert-to-closures-helper new-boundeds) body))
          (define closureType `(Vector (,@ps -> ,rt)))
          (values `(define (,f [clos : ,closureType] ,@(cdadr rawExp)) : ,rt ,newBody) bodyLambdas bodyFrees)])))

(define func-type?
  (lambda (e)
    (member '-> e)))

(define make-has-type-exp
  (lambda (e t)
    `(has-type ,e ,t)))

(define get-type-from-has-type
  (lambda (e)
    (caddr e)))

(define (convert-to-closures e)
  (match e
    [`(program ,type (defines ,defs ...) ,es)
        (set! top-level-defs (map caadr defs))
        (define-values (newDefs defLambdas defFrees) (map3 (convert-to-closures-helper top-level-defs) defs))
        (define-values (newEs eLambdas eFrees) ((convert-to-closures-helper top-level-defs) es))
        `(program ,type (defines ,@newDefs ,@(apply append defLambdas) ,@eLambdas) ,newEs)]))
