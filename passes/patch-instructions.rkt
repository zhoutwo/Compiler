#lang racket
(require "../utilities.rkt")
(provide patch-instructions)

(define patch-instructions-stms
  (lambda (stms)
    (match stms
         [`(leaq (function-ref ,f) (deref ,reg ,l))
                     `((leaq (function-ref ,f) (reg rax))
                       (movq (reg rax) (deref ,reg ,l)))]
         [`(movq (deref ,reg1 ,l1) (deref ,reg2 ,l2))
                     `((movq (deref ,reg1 ,l1) (reg rax))
                       (movq (reg rax) (deref ,reg2 ,l2)))]
         [`(movq (global-value ,ptr) (deref ,reg2 ,l2))
                     `((movq (global-value ,ptr) (reg rax))
                       (movq (reg rax) (deref ,reg2 ,l2)))]
         [`(addq (deref ,reg1 ,l1) (deref ,reg2 ,l2))
                     `((movq (deref ,reg1 ,l1) (reg rax))
                       (addq (reg rax) (deref ,reg2 ,l2)))]
         [`(cmpq ,rest (int ,l2))
                     `((movq (int ,l2) (reg rax))
                       (cmpq ,rest (reg rax)))]
         [`(cmpq ,rest (deref ,reg2 ,l2))
                     `((movq (deref ,reg2 ,l2) (reg rax))
                       (cmpq ,rest (reg rax)))]
         [`(movzbq ,rest (deref ,reg2 ,l2))
                     `((movzbq ,rest (reg rax))
                       (movq (reg rax) (deref ,reg2 ,l2)))]
         [else `(,stms)])))

(define patch-instructions
  (lambda (p)
    (match p
           [`(program ,maxn ,type (defines ,defs ...) ,stms)
              `(program ,maxn ,type (defines ,@(map patch-instructions defs)) ,(apply append (map patch-instructions-stms stms)))]
           [`(define (,f) ,numParam (,maxn ,rspills) (,arg-types ,storage-types) ,stms)
              `(define (,f) ,numParam (,maxn ,rspills) (,arg-types ,storage-types) ,(apply append (map patch-instructions-stms stms)))])))
