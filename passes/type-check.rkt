#lang racket
(require "../utilities.rkt")

(provide type-check)
(require racket/trace)
(define (type-check-R4 env)
  (trace-lambda (e)
    (pretty-print 'aaaaa)
    (pretty-print e)
    (match e
           [(? fixnum?) (values `(has-type ,e Integer) 'Integer)]
           [(? boolean?) (values `(has-type ,e Boolean) 'Boolean)]
           [(? symbol?) (values `(has-type ,e ,(lookup e env)) (lookup e env))]
           [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
              (pretty-print xs)
              (pretty-print ps)
              (define newenv (let loop ([xs xs]
                                   [ps ps]
                                   [renv '()])
                                   (if (null? xs)
                                     (append renv env)
                                     (loop (cdr xs) (cdr ps) (cons (cons (car xs) (car ps)) renv)))))
              (pretty-print 'bbbbb)
              (pretty-print body)
              (pretty-print "before check")
              (pretty-print env)
              (pretty-print newenv)
              (define-values (e-body t-body) ((type-check-R4 newenv) body))
              (pretty-print "before construct")
              (define newe (cons (car e) (cons (cadr e) (cons (caddr e) (cons (cadddr e) (list e-body))))))
              (pretty-print "after construct")
              (pretty-print newe)
              (pretty-print e)
              (if (equal? rt t-body)
                  (values newe
                          `(,@ps -> ,rt))
                  (error `type-check-R4 "declared return type doesn't match actual return type: ~a" e))]
           [`(let ([,x ,(app (type-check-R4 env) e T)]) ,body)
             (define new-env (cons (cons x T) env))
             (define-values (e-body T-body) ((type-check-R4 new-env) body))
             (values `(has-type (let ([,x ,e]) ,e-body) ,T-body) T-body)]
           [`(not ,(app (type-check-R4 env) e t))
             (match t
                    [`Boolean (values `(has-type (not ,e) Boolean) 'Boolean)]
                    [else (error `type-check-R4 "`not` expects a Boolean: ~a" e)])]
           [`(and ,(app (type-check-R4 env) e1 t1) ,(app (type-check-R4 env) e2 t2))
             (match t1
                    [`Boolean (values `(has-type (and ,e1 ,e2) `Boolean) 'Boolean)]
                    [else (error `type-check-R4 "`and` expects two Booleans: ~a" e)])
             (match t2
                    [`Boolean (values `(has-type (and ,e1 ,e2) `Boolean) 'Boolean)]
                    [else (error `type-check-R4 "`and` expects a Booleans: ~a" e)])]

           [`(if ,(app (type-check-R4 env) e-cnd t-cnd) ,(app (type-check-R4 env) e-thn t-thn) ,(app (type-check-R4 env) e-els t-els))
             (if (eq? 'Boolean t-cnd)
               (if (eq?  t-thn t-els)
                 (values `(has-type (if ,e-cnd ,e-thn ,e-els) ,t-thn) t-thn)
                 (error `type-check-R4 "'if' expects 'then' and 'else' to be the same type: ~a" e))
               (error `type-check-R4 "'if' expects condition to be Boolean: ~a" e))]
           [`(- ,(app (type-check-R4 env) e t))
             (if (eq? 'Integer t)
               (values `(has-type (- ,e) Integer) 'Integer)
               (error 'type-check-R4 "`-` expects an Integers: ~a" e))]
           [`(+ ,(app (type-check-R4 env) e1 t1) ,(app (type-check-R4 env) e2 t2))
             (if (and (eq? 'Integer t1) (eq? 'Integer t2))
               (values `(has-type (+ ,e1 ,e2) Integer) 'Integer)
               (error 'type-check-R4 "'+' expects two Integers: ~a" e))]
           [`(read)
               (values `(has-type (read) 'Integer) 'Integer)]
           [`(void) (values `(has-type (void) Void) `Void)]
           [`(vector ,(app (type-check-R4 env) e* t*) ...)
             (let ([t `(Vector ,@t*)])
               (values `(has-type (vector ,@e*) ,t) t))]
           [`(vector-ref ,(app (type-check-R4 env) e t) ,i)
             (match t
                    [`(Vector ,ts ...)
                      (unless (and (exact-nonnegative-integer? i)
                                   (i . < . (length ts)))
                        (error `type-check "invalid index ~a" i))
                      (let ([t (list-ref ts i)])
                        (values `(has-type (vector-ref ,e (has-type ,i Integer)) ,t)
                                t))]
                    [else (error "expected a vector in vector-ref, not" t)])]
           [`(vector-set! ,(app (type-check-R4 env) e-vec^ t-vec) ,i
                          ,(app (type-check-R4 env) e-arg^ t-arg))
             (match t-vec
                    [`(Vector ,ts ...)
                      (unless (and (exact-nonnegative-integer? i)
                                   (i . < . (length ts)))
                        (error `type-check "invalid index ~a" i))
                      (unless (equal? (list-ref ts i) t-arg)
                        (error `type-check "type mismatch in vector-set! ~a ~a"
                               (list-ref ts i) t-arg))
                      (values `(has-type (vector-set! ,e-vec^
                                                      (has-type ,i Integer)
                                                      ,e-arg^) Void) `Void)]
                    [else (error `type-check
                                 "expected a vector in vector-set!, not ~a" t-vec)])]
           [`(eq? ,(app (type-check-R4 env) e1 t1)
                  ,(app (type-check-R4 env) e2 t2))
             (match* (t1 t2)
                     [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
                      (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)]
                     [(other wise) 
                       (if (eq? t1 t2) 
                         (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)
                         (error `type-check "`eq?` expects two Integers or two Booleans: ~a" e))])]
           [`(program ,ds ... ,body)
             (pretty-print "top-level")
             (define fname-types (map (lambda (d)
                                          (match d
                                            [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
                                                (cons (caadr d)`(,@ps -> ,rt))])) ds))
             (set! env (append fname-types env))
             (pretty-print "afterset!")
             (pretty-print env)
             (pretty-print fname-types)
             (define ds-e (let loop ([ds ds]
                        [ds-e '()])
               (if (null? ds)
                   ds-e
                   (let-values ([(e-body t-body) ((type-check-R4 env) (car ds))]
                                [(func-name) (caadr (car ds))])
                     (loop (cdr ds) (append ds-e `(,e-body)))))))
             (pretty-print "afterloop")
             (pretty-print env)
             (define-values (e-body t-body) ((type-check-R4 env) body))
             `(program (type ,t-body) ,ds-e ,e-body)]
           [`(,op ,(app (type-check-R4 env) es ts) ...)
             (define-values (funce functype) ((type-check-R4 env) op))
             (pretty-print 'functype)
             (pretty-print functype)
             (pretty-print env)
             (let loop ([ftype functype]
                        [les es]
                        [lts ts])
                    (cond
                      [(null? ftype) (error `type-check "Function lack return type: ~a" e)]
                      [(and (null? les) (not (equal? '-> (car ftype))))
                        (error `type-check "Not enough arguments: ~a" e)]
                      [(and (not (null? les)) (equal? '-> (car ftype)))
                        (error `type-check "Too many arguments: ~a" e)]
                      [(not (null? les))
                        (if (equal? (car ftype) (car lts))
                            (loop (cdr ftype) (cdr les) (cdr lts))
                            (error `type-check "Argument type mismatch: ~a ~a ~a" (car ftype) (car les) e))]
                      [else
                        (values `(has-type ,funce ,@es ,(cadr ftype))
                                (cadr ftype))]))]
           [`(,op ,(app (type-check-R4 env) e1 t1) ,(app (type-check-R4 env) e2 t2))
             (if (and (eq? 'Integer t1) (eq? 'Integer t2))
               (values `(has-type (,op ,e1 ,e2) `Boolean) 'Boolean)
               (error `type-check-R4 (string-append (symbol->string op) " expects two Integers: ~a ~a ~a") e t1 t2))]
           )))
;(trace type-check-R4)
(define type-check
  (lambda (e)
  ((type-check-R4 '()) e)))
