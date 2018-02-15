#lang racket
(require "../utilities.rkt")
(provide parse-syntaxes)

(define (parse-syntaxes e)
  (define syndefs (filter (lambda (exp) (and (list? exp)  (equal? (car exp) 'define-syntax))) (cdr e)))
  (define nonsyndefs (filter (lambda (exp) (or (not (list? exp)) (not (equal? (car exp) 'define-syntax)))) (cdr e)))
  `(program ,(map parse-syntaxes-helper syndefs) ,@nonsyndefs))

(define (parse-syntaxes-helper e)
  (match e
    [`(define-syntax (,syn ,args ...) ,body)
        ; `(syntax name argmapping bodymapping)
        `(syntax ,syn ,(cons syn args) ,(cons syn body))]))
