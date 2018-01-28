#lang racket
(require "../utilities.rkt")
(provide uncover-live)

(define uncover-live-helper
  (lambda (stms live-afters)
    (if (equal? 0 (length stms)) 
      (values stms live-afters)
      (let*-values ([(nstms befores) (uncover-live-helper (cdr stms) live-afters)] 
                    [(afterK) (car befores)] 
                    [(w) (write-to (car stms))] 
                    [(r) (read-from (car stms))]
                    )
                   (match (car stms)
                          [`(if (eq? ,arg1 ,arg2) ,thn ,els) 
                            (define-values (thns thn-befores) (uncover-live-helper thn (list afterK)))
                            (define-values (elss els-befores) (uncover-live-helper els (list afterK)))
                            (values (cons `(if (eq? ,arg1 ,arg2) ,thns ,(cdr thn-befores) ,elss ,(cdr els-befores)) nstms) (cons (set-union (car thn-befores) (car els-befores) (parse-vars arg1) (parse-vars arg2)) befores))]
                          [else 
                            (values (cons (car stms) nstms) (cons (set-union r (set-subtract afterK w)) befores))]
                          )))))

(define parse-vars
  (lambda (vars)
    (match vars
           [`(var ,var) (list var)]
           [else null])))

(define write-to
  (lambda (stms)
    (match (match stms
                  [`(movq ,arg1 ,arg2)
                    arg2]
                  [`(addq ,arg1 ,arg2)
                    arg2]
                  [`(negq ,arg1)
                    arg1]
                  [`(xorq ,arg1 ,arg2)
                    arg2]
                  [`(cmpq ,arg1 ,arg2)
                    null]
                  [`(set ,cc ,arg)
                    arg]
                  [`(movzbq ,arg1 ,arg2)
                    arg2]
                  [else null]) 
           [`(var ,var)
             (list var)]
           [else null])))

(define read-from
  (lambda (stms)
    (remove null (map (lambda (x)
                        (match x
                               [`(var ,var)
                                 var]
                               [else null]))
                      (match stms
                             [`(movq ,arg1 ,arg2)
                               (list arg1)]
                             [`(addq ,arg1 ,arg2)
                               (list arg1 arg2)]
                             [`(negq ,arg1)
                               (list arg1)]
                             [`(xorq ,arg1 ,arg2)
                               (list arg1 arg2)]
                             [`(cmpq ,arg1 ,arg2)
                               (list arg1 arg2)]
                             [`(set ,cc ,arg)
                               (list arg)]
                             [`(movzbq ,arg1 ,arg2)
                               (list arg1)]
                             [else null])))))

(define uncover-live
  (lambda (program)
    (match program
           [`(program ,vars ,type ,stms)
             (begin
               (define-values (nstsm afters) (uncover-live-helper stms '(())))
               `(program ,(list vars (cdr afters) type) ,nstsm))])))

