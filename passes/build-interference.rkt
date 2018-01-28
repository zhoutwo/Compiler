#lang racket
(require "../utilities.rkt")
(require racket/trace)
(provide build-interference)

(define add-edges-helper
  (lambda (d vs ignores graph)
    (let ([ign (remove null 
                       (map (lambda (x) 
                              (match x 
                                     [`(var ,var) var] 
                                     [else null])) ignores))])
      (match d
             [`(var ,var)
               (begin
                 (map (lambda (v) (add-edge graph var v)) (filter (lambda (x) (not (member x ign))) vs))
                 graph)]
             [else graph]))))

(define helper
  (lambda (live-after stms graph)
    (if (null? stms) graph
      (helper (cdr live-after) (cdr stms) 
              (match (car stms)
                     [`(movq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph)]
                     [`(movzbq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph)]
                     [`(negq ,arg1)
                       (add-edges-helper arg1 (car live-after) (list arg1) graph)]
                     [`(if (eq? ,arg1 ,arg2) ,thn ,thn-lives ,els ,els-lives)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) (helper els-lives els (helper thn-lives thn graph)))]
                     [`(,op ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg2) graph)]
                     [else graph]
                     )))))

(define build-interference
  (lambda (program)
    (match program
           [`(program (,vars ,live-afters ,type) ,stms)
             `(program ,(list vars (helper live-afters stms (make-graph vars)) type) ,stms)])))
