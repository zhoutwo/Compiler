#lang racket
(require "../utilities.rkt")
(provide uncover-live)

(define (vars-read e)
  (match e
    [`(indirect-callq (var ,v)) `(,v)]
    [`(addq (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(addq (var ,v) ,rest) `(,v)]
    [`(addq ,rest (var ,v)) `(,v)]
    [`(cmpq (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(cmpq (var ,v) ,rest) `(,v)]
    [`(cmpq ,rest (var ,v)) `(,v)]
    [`(xorq (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(xorq (var ,v) ,rest) `(,v)]
    [`(xorq ,rest (var ,v)) `(,v)]
    [`(eq? (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(eq? (var ,v) ,rest) `(,v)]
    [`(eq? ,rest (var ,v)) `(,v)]
    [`(> (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(> (var ,v) ,rest) `(,v)]
    [`(> ,rest (var ,v)) `(,v)]
    [`(>= (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(>= (var ,v) ,rest) `(,v)]
    [`(>= ,rest (var ,v)) `(,v)]
    [`(<= (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(<= (var ,v) ,rest) `(,v)]
    [`(<= ,rest (var ,v)) `(,v)]
    [`(< (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(< (var ,v) ,rest) `(,v)]
    [`(< ,rest (var ,v)) `(,v)]
    [`(movq (var ,v) ,rest) `(,v)]
    [`(negq (var ,v)) `(,v)]
    [`(eq? (var ,v1) (var ,v2)) `(,v1 ,v2)]
    [`(eq? (var ,v) ,rest) `(,v)]
    [`(eq? ,rest (var ,v)) `(,v)]
    [else '()]))

(define (vars-write e)
  (match e
    [`(leaq ,rest (var ,v)) `(,v)]
    [`(addq ,rest (var ,v)) `(,v)]
    [`(cmpq ,rest (var ,v)) `(,v)]
    [`(eq? ,rest (var ,v)) `(,v)]
    [`(> ,rest (var ,v)) `(,v)]
    [`(>= ,rest (var ,v)) `(,v)]
    [`(<= ,rest (var ,v)) `(,v)]
    [`(< ,rest (var ,v)) `(,v)]
    [`(xorq ,rest (var ,v)) `(,v)]
    [`(movq ,rest (var ,v)) `(,v)]
    [`(negq (var ,v)) `(,v)]
    [else '()]))

(define (last ls)
  (if (null? (cdr ls))
    (car ls)
    (last (cdr ls))))

(define (make-before after e ls)
  (let ([W (vars-write e)]
        [R (vars-read e)])
        (set-union (set-subtract after W) R)))

(define (uncover-live-helper e ls)
  (match e
    [`(program (,var-types ,num-vars) (type ,t) (defines ,defs ...) ,maxStacks ,es)
        (begin
          (define-values (lives ees) (uncover-live-helper es ls))
          (define-values (deflives defees) (map2 (lambda (def) (uncover-live-helper def ls)) defs))
          `(program (,var-types ,(cdr lives) (type ,t)) (defines ,@defees) ,maxStacks ,(reverse ees)))]
    [`(define (,f) ,numParams (,vars ... ,maxStack) ,instrs)
        (begin
          (define-values (lives ees) (uncover-live-helper instrs ls))
          (values (cdr lives) `(define (,f) ,numParams (,vars ,maxStack ,(cdr lives)) ,(reverse ees))))]
    [else
      (let loop ([es (reverse e)]
                 [ls ls])
          (if (null? es) (values ls es)
              (match (car es)
                [`(if (eq? ,v1 ,v2) ,thn ,alt)
                    (let-values ([(thnlive thn) (uncover-live-helper thn ls)]
                                  [(altlive alt) (uncover-live-helper alt ls)])
                                  (let ([before (set-union (car thnlive) (car altlive) (vars-read `(eq? ,v1 ,v2)))])
                                      (define-values (lls ees) (loop (cdr es) (cons before ls)))
                                      (values lls (cons `(if (eq? ,v1 ,v2) ,(reverse thn) ,thnlive ,(reverse alt) ,altlive) ees))))]
                [else
                    (let* ([after (car ls)]
                           [before (make-before after (car es) ls)])
                        (define-values (lls ees) (loop (cdr es) (cons before ls)))
                        (values lls (cons (car es) ees)))])))]))

(define (uncover-live e)
  (uncover-live-helper e '(())))
