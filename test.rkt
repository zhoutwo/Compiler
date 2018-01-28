#lang racket
(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

(define make-list 
  (lambda (n)
    (if (= 0 n) '()
      (append (make-list (- n 1)) (list n)))))
;(compiler-tests "compiler.rkt" type-check r1-passes "r2" (make-list 59))
(compiler-tests "compiler.rkt" type-check r3-passes "r3" (make-list 35))

