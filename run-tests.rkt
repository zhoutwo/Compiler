#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

(debug-level 4)

(compiler-tests "compiler.rkt" type-check r6-passes "r0" (make-list 4))
(compiler-tests "compiler.rkt" type-check r6-passes "r1" (make-list 48))
(compiler-tests "compiler.rkt" type-check r6-passes "r1a" (make-list 8))
(compiler-tests "compiler.rkt" type-check r6-passes "r2" (make-list 59))
(compiler-tests "compiler.rkt" type-check r6-passes "r3" (make-list 35))
(compiler-tests "compiler.rkt" type-check r6-passes "r4" (make-list 38))
;(compiler-tests "compiler.rkt" type-check r6-passes "r5" (make-list 12))
(compiler-tests "compiler.rkt" type-check r6-passes "r5" (list 1 2 3 4 5 6 7 8 10 11 12))
(compiler-tests "compiler.rkt" type-check r6-passes "r6" (make-list 6))
(newline) (display "tests passed!") (newline)
