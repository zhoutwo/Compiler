#lang racket
(require "../utilities.rkt")

(provide build-interference)

(define add-edges-helper
  (lambda (d vs ignores graph var-types)
    (let ([ign (remove null 
                       (map (lambda (x) 
                              (match x 
                                     [`(var ,var) var] 
                                     [else null])) ignores))])
      (match d
             [`(var ,var)
               (begin
                 (let ([type (lookup var var-types)])
                    (if (equal? 'Vector type)
                        (map (lambda (r)
                                (add-edge graph var r)) caller-save)
                        (void)))
                    (map (lambda (v) (add-edge graph var v)) (filter (lambda (x) (not (member x ign))) vs))
                 graph)]
             [else graph]))))

(define helper
  (lambda (live-after stms graph var-types)
    (if (null? stms) graph
      (helper (cdr live-after) (cdr stms) 
              (match (car stms)
                     [`(movq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph var-types)]
                     [`(movzbq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph var-types)]
                     [`(negq ,arg1)
                       (add-edges-helper arg1 (car live-after) (list arg1) graph var-types)]
                     [`(if (eq? ,arg1 ,arg2) ,thn ,thn-lives ,els ,els-lives)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) (helper els-lives els (helper thn-lives thn graph var-types) var-types) var-types)]
                     [`(,op ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg2) graph var-types)]
                     [else graph]
                     )
              var-types))))

(define build-interference
  (lambda (program)
    (match program
           [`(define (,fs) ,numParams (,defvar-types ,maxStacks ,lives) ,ees)
              (define alldefvars (map car defvar-types))
               `(define (,fs) ,numParams (,defvar-types ,maxStacks ,(helper lives ees (make-graph alldefvars) defvar-types)) ,ees)]
           [`(program (,var-types ,live-afters ,type) (defines ,defs ...) ,stms)
               (define ndefs (map build-interference defs))
               ;(define def-var-types (apply append defvar-types))
               (define all-var-types var-types)
               (define all-vars (set-union (map car all-var-types) (set->list caller-save)))
               ;(define g (make-graph all-vars))
               ;(set! g (helper live-afters stms g all-var-types))
               ;(let loop ([lives lives]
               ;           [ees ees])
               ;           (if (null? lives)
               ;               (void)
               ;               (begin (set! g (helper (car lives) (car ees) g all-var-types))
               ;                      (loop (cdr defvar-types) (cdr lives) (cdr ees)))))
               `(program (,var-types ,(helper live-afters stms (make-graph all-vars) all-var-types) ,type) (defines ,@ndefs) ,stms)])))
