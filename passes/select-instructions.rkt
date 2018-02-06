#lang racket
(require "../utilities.rkt")
(provide select-instructions)

(define select-intr-arg
  (lambda (arg)
    (match arg
      [(? integer?) (list 'int arg)]
      [(? boolean?) (list 'int (if arg 1 0))]
      [`(void) `(int 0)]
      [else `(var ,arg)]
      )))

(define (make-tag len types cur_pos)
  (let ([offset 7])
    (cond
        [(null? types) (bitwise-ior 1 (arithmetic-shift len 1))]
        [(and (pair? (car types)) (equal? 'Vector (caar types)))
          (bitwise-ior (arithmetic-shift 1 (+ offset cur_pos)) (make-tag len (cdr types) (+ 1 cur_pos)))]
        [else (make-tag len (cdr types) (+ 1 cur_pos))])))

(define (select-intr-stms func-var-list func-var-storage)
  (lambda (stms)
    (if (null? stms) (list)
      (match (car stms)
             [`(define (,f [,xs : ,ts] ...) : ,rt (,vars ...) ,stmts)
               (define allargs (map (lambda (x t)
                                            (cons x t)) xs ts)) ;(cons (gensym `rs) xs))
               (define allargs-storage (lookup f func-var-storage))
               (define storages (filter (lambda (bool) bool) (map (lambda (arg-storage)
                                                                     (define arg (car arg-storage))
                                                                     (define storage (cdr arg-storage))
                                                                     (if (eq? 'var (car storage))
                                                                         (cons (cadr storage) (lookup arg allargs))
                                                                         #f)) allargs-storage)))
               (define allargslength (length allargs))
               (define allvars (append allargs storages vars))
               `((define (,f)
                         ,allargslength
                         (,allvars ,(if (< allargslength 6) 0 (- allargslength 6)))
                         (,@(map (lambda (as) `(movq ,(cdr as) ,(select-intr-arg (car as)))) allargs-storage)
                          ,@((select-intr-stms func-var-list func-var-storage) stmts)))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))]
             [`(assign ,var (global-value ,arg1))
               `((movq (global-value ,arg1) ,(select-intr-arg var))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(assign ,var (vector-ref ,vec, n))
               `((label ,(gensym "vector_ref_starts"))
                 (movq ,(select-intr-arg vec) (reg r11))
                 (movq (deref r11 ,(* 8 (+ 1 n))) ,(select-intr-arg var))
                 (label ,(gensym "vector_ref_ends"))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(assign ,var (vector-set! ,vec ,n ,arg))
               `((label ,(gensym "vector_set_starts"))
                 (movq ,(select-intr-arg vec) (reg r11))
                 (movq ,(select-intr-arg arg) (deref r11 ,(* 8 (+ 1 n))))
                 (movq (int 0) ,(select-intr-arg var))
                 (label ,(gensym "vector_set_ends"))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(assign ,var (collect ,bytes))
               `((label ,(gensym "collect_starts"))
                 (movq (reg r15) (reg rdi))
                 (movq ,(select-intr-arg bytes) (reg rsi))
                 ,(let ([mac? (equal? 'macosx (system-type 'os))])
                    (if mac?
                        `(callq _collect)
                        `(callq _collect)))
                 (movq (int 0) ,(select-intr-arg var))
                 (label ,(gensym "collect_ends"))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(assign ,var (allocate ,len (Vector ,types ...)))
               `((label ,(gensym "allocate_starts"))
                 (movq (global-value free_ptr) ,(select-intr-arg var))
                 (addq (int ,(* 8 (+ 1 len))) (global-value free_ptr))
                 (movq ,(select-intr-arg var) (reg r11))
                 (movq (int ,(make-tag len types 0)) (deref r11 0))
                 (label ,(gensym "allocate_ends"))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(assign ,var (+ ,arg1 ,arg2)) 
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var)) 
                     (cons (list `addq (select-intr-arg arg2) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms))))
               ]
             [`(assign ,var (- ,arg1))
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var))
                     (cons (list `negq (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms))))
               ]
             [`(assign ,var (read)) 
               (begin
               (define mac? (equal? 'macosx (system-type 'os)))
               (cons `(callq ,(if mac? '_read_int '_read_int)) (cons `(movq (reg rax) ,(select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))
               ]
             [`(assign ,var (not ,arg1))
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var))
                     (cons (list `xorq `(int 1) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms))))]
             [`(assign ,var (eq? ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `e `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))]
             [`(assign ,var (> ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `g `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))]
             [`(assign ,var (>= ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `ge `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))]
             [`(assign ,var (< ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `l `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))]
             [`(assign ,var (<= ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `le `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))))]
             [`(assign ,var (function-ref ,label))
               (set! func-var-list (cons (cons var (lookup label func-var-list)) func-var-list))
               (set! func-var-storage (cons (cons var (lookup label func-var-storage)) func-var-storage))
               `((leaq (function-ref ,label) ,(select-intr-arg var))
                 ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))]
             [`(assign ,var (app ,op ,rands ...))
               (let ([args (lookup op func-var-list)])
                   (let loop ([args args]
                              [rands rands]
                              [as '()])
                        (if (null? args)
                            `(,@as
                                (indirect-callq ,(select-intr-arg op))
                                (movq (reg rax) ,(select-intr-arg var))
                                ,@((select-intr-stms func-var-list func-var-storage) (cdr stms)))
                            (loop (cdr args) (cdr rands) `(,@as (movq ,(select-intr-arg (car rands)) ,(car args)))))))]
             [`(assign ,var ,arg)
               (cons (list `movq (select-intr-arg arg) (select-intr-arg var)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(return ,arg)
               (cons (list `movq (select-intr-arg arg) `(reg rdi)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))
               ]
             [`(if (eq? ,arg1 ,arg2) ,thn ,els)
               (cons (list `if `(eq? ,(select-intr-arg arg1) ,(select-intr-arg arg2)) ((select-intr-stms func-var-list func-var-storage) thn) ((select-intr-stms func-var-list func-var-storage) els)) ((select-intr-stms func-var-list func-var-storage) (cdr stms)))]
             ))))

(define select-instructions
  (lambda (p)
    (match p
           [`(program ,vars ,type (defines ,defs ...) ,stms)
             (define func-var-storage (map (lambda (def)
                                             (match def
                                               [`(define (,f [,xs : ,ts] ...) : ,rt (,vars ...) ,stmts)
                                                  (cons f (let loop ([args xs]
                                                                      [result '()]
                                                                      [counter 0])
                                                                      (cond
                                                                        [(null? args) result]
                                                                        [(< counter 6)
                                                                          (loop (cdr args) (cons (cons (car args) `(reg ,(vector-ref arg-registers counter))) result) (+ 1 counter))]
                                                                        [else
                                                                          (loop (cdr args) (cons (cons (car args) `(var ,(gensym (car args)))) result) (+ 1 counter))])))])) defs))
             (define func-var-list (map (lambda (def)
                                          (match def
                                            [`(define (,f [,xs : ,ts] ...) : ,rt (,vars ...) ,stmts)
                                              (cons f
                                                (map (lambda (x)
                                                       (lookup x (lookup f func-var-storage))) xs))])) defs))
             ;(define func-vars (apply append (map cdr func-var-list)))
             ;(define allvars (append vars func-vars))
             (let ([ndefs ((select-intr-stms func-var-list func-var-storage) defs)]
                   [nstms ((select-intr-stms func-var-list func-var-storage) stms)])
               `(program (,vars ,(length vars)) ,type (defines ,@ndefs) ,nstms))] )))
